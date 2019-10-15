open Lwt.Infix
open Httpaf

module Option = struct
  let bind a f = match a with Some a -> f a | None -> None
  let map f = function Some x -> Some (f x) | None -> None
  let ( >>= ) = bind
end

module List = struct
  include List

  let hd_opt = function x :: _ -> Some x | [] -> None
end

let formatter_of_body body =
  let output x off len = Body.write_string body ~off ~len x in
  let flush () = Body.flush body (fun () -> ()) in
  Format.make_formatter output flush

let parse_content_type v =
  let open Angstrom in
  parse_string Multipart_form.(Rfc2045.content <* Rfc822.crlf) (v ^ "\r\n")

let extract_content_type request =
  let exception Found in
  let headers = request.Request.headers in
  let content_type = ref None in
  try
    List.iter
      (fun (field_name, v) -> match String.lowercase_ascii field_name with
         | "content-type" ->
           ( match parse_content_type v with
             | Ok v -> content_type := Some v ; raise Found
             | _ -> () )
         | _ -> ())
      (Headers.to_list headers) ; None
  with Found -> !content_type

let name_of_fields fields =
  let open Multipart_form.Field in
  let name = ref None in
  let exception Found in
  try List.iter (function
      | Field (Disposition, { parameters; _ }) ->
        ( match List.assoc "name" parameters with
          | `Token v | `String v -> name := Some v ; raise Found
          | exception Not_found -> () )
      | _ -> () ) fields ; None
  with Found -> !name

type key = Paste | User | Comment | Mldown | Ln | Raw | Hl

let key_of_string = function
  | "paste" -> Some Paste
  | "user" -> Some User
  | "comment" -> Some Comment
  | "mldown" -> Some Mldown
  | "ln" -> Some Ln
  | "raw" -> Some Raw
  | "hl" -> Some Hl
  | _ -> None

let string_of_key = function
  | Paste -> "paste" | User -> "user" | Comment -> "comment" | Mldown -> "mldown" | Ln -> "ln"
  | Raw -> "raw" | Hl -> "hl"

let pp_string ppf x = Fmt.pf ppf "%S" x
let blit src src_off dst dst_off len = Bigstringaf.blit src ~src_off dst ~dst_off ~len

let extract_parts content_type body =
  let open Angstrom.Unbuffered in

  let hashtbl = Hashtbl.create 7 in
  let emitters fields = match Option.(name_of_fields fields >>= key_of_string) with
    | Some key ->
      let stream, push = Lwt_stream.create () in
      Hashtbl.add hashtbl key stream ; push, Some key
    | None -> (fun _ -> ()), None in

  let thread, finished = Lwt.task () in
  let state = ref (parse (Multipart_form.parser ~emitters content_type)) in
  let rb = Bigstringaf.create 4096 in
  let ke = Ke.Rke.Weighted.from rb in

  let rec on_eof () =
    match !state with
    | Partial { continue; committed; } ->
      Ke.Rke.Weighted.N.shift_exn ke committed ;
      if committed = 0 then Ke.Rke.Weighted.compress ke ;
      ( match Ke.Rke.Weighted.N.peek ke with
        | [] -> state := continue rb ~off:0 ~len:0 Complete
        | [ slice ] -> state := continue slice ~off:0 ~len:(Bigstringaf.length slice) Complete
        | slice :: _ -> state := continue slice ~off:0 ~len:(Bigstringaf.length slice) Complete ) ;
      on_eof ()
    | Fail _ -> Lwt.wakeup finished (Rresult.R.error_msgf "bad POST request")
    | Done (_, v) -> Lwt.wakeup finished (Rresult.R.ok v)
  and on_read buf ~off ~len =
    match !state with
    | Partial { continue; committed; } ->
      Ke.Rke.Weighted.N.shift_exn ke committed ;
      let len' = min (Ke.Rke.Weighted.available ke) len in
      if len' = 0 then Lwt.wakeup finished (Rresult.R.error_msgf "POST buffer is full!") ;
      ( match Ke.Rke.Weighted.N.push ke ~blit:blit ~length:Bigstringaf.length ~off ~len:len' buf with
        | Some _ ->
          if committed = 0 then Ke.Rke.Weighted.compress ke ;
          let[@warning "-8"] slice :: _ = Ke.Rke.Weighted.N.peek ke in
          state := continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete ;
          if len' - len = 0
          then Body.schedule_read body ~on_eof ~on_read
          else on_read buf ~off:(off + len') ~len:(len - len')
        | None -> Lwt.wakeup finished (Rresult.R.error_msgf "POST buffer is full!") )
    | Fail _ -> Lwt.wakeup finished (Rresult.R.error_msgf "bad POST request")
    | Done (_, v) -> Lwt.wakeup finished (Rresult.R.ok v) in
  let open Lwt.Infix in
  Body.schedule_read body ~on_eof ~on_read ;
  thread >>= function
  | Error _ as err -> Lwt.return err
  | Ok _ ->
    let lst = Hashtbl.fold (fun k s a -> (k, Lwt_stream.to_list s) :: a) hashtbl [] in
    Lwt_list.map_p (fun (k, t) -> t >|= fun v -> (k, String.concat "" v)) lst >|= Rresult.R.ok

module Make
    (Random : Mirage_types_lwt.RANDOM)
    (Console : Mirage_types_lwt.CONSOLE)
    (Clock : Mirage_types_lwt.PCLOCK)
    (Public : Mirage_types_lwt.KV_RO)
    (Resolver : Resolver_lwt.S)
    (Conduit : Conduit_mirage.S)
    (HTTP : Httpaf_mirage.Server_intf)
= struct
  module Store = Irmin_mirage_git.Mem.KV(Irmin.Contents.String)
  module Sync = Irmin.Sync(Store)

  let log console fmt = Fmt.kstrf (Console.log console) fmt

  let connect resolver conduit =
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >|= fun repository ->
    repository, Store.remote ~conduit ~resolver (Key_gen.remote ())

  let load console repository remote key =
    Store.find repository key >>= function
    | Some contents -> Lwt.return (Some contents)
    | None ->
      log console "Fetch remote repository." >>= fun () ->
      Sync.pull repository remote `Set >>= function
      | Ok `Empty | Ok (`Head _) ->
        log console "Synchronization done." >>= fun () ->
        Store.find repository key
      | Error (`Msg err) -> failwith err
      | Error (`Conflict err) -> Fmt.failwith "Conflict! [%s]" err

  let show ?(ln:_= false) ?hl console store remote reqd target () =
    let open Httpaf in
    log console "Want to access to: %a." Fmt.(Dump.list string) target >>= fun () ->
    load console store remote target >>= function
    | None ->
      let contents = Fmt.strf "%s Not found." (String.concat "/" target) in
      let headers = Headers.of_list [ "connection", "close" ] in
      let response = Response.create ~headers `Not_found in
      Reqd.respond_with_string reqd response contents ;
      log console "Response: 404 Not found for %a." Fmt.(Dump.list string) target
    | Some contents ->
      let html = Show.html ?code:(Option.map Language.value_of_language hl) contents in
      let contents = Fmt.strf "%a%!" (Tyxml.Html.pp ()) html in
      let headers = Headers.of_list [ "content-type", "text/html"
                                    ; "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return ()

  let show_raw console store remote reqd target () =
    let open Httpaf in
    log console "Want to access to: %a (raw)." Fmt.(Dump.list string) target >>= fun () ->
    load console store remote target >>= function
    | None ->
      let contents = Fmt.strf "%s Not found." (String.concat "/" target) in
      let headers = Headers.of_list [ "connection", "close" ] in
      let response = Response.create ~headers `Not_found in
      Reqd.respond_with_string reqd response contents ;
      log console "Response: 404 Not found for %a." Fmt.(Dump.list string) target
    | Some contents ->
      let headers = Headers.of_list [ "content-type", "text/plain; charset=utf-8"
                                    ; "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return ()

  type dispatch =
    | INDEX
    | GET of { target : string list
             ; ln : bool
             ; hl : Language.t option
             ; raw : bool }
    | CONTENTS of Public.key
    | POST

  let dispatch public reqd =
    let request = Reqd.request reqd in
    let target = Uri.of_string request.Request.target in
    let queries = Uri.query target in
    let target = Astring.String.trim ~drop:(Char.equal '/') (Uri.path target) in
    let target = Astring.String.cuts ~sep: "/" target in

    let key = Mirage_kv.Key.v request.Request.target in
    Public.exists public key >>= function
    | Error err -> Fmt.invalid_arg "%a" Public.pp_error err
    | Ok (Some `Value) ->
      Lwt.return (CONTENTS key)
    | Ok (Some `Dictionary) | Ok None ->
      match target with
      | [] | [ "" ]->
        if request.Request.meth = `POST
        then Lwt.return POST
        else Lwt.return INDEX
      | _ :: _ ->
        let ln = match List.assoc_opt "ln" queries with
          | Some [ "true" ] -> true
          | Some _ | None -> false in
        let hl = let open Option in List.assoc_opt "hl" queries >>= List.hd_opt >>= Language.language_of_value_opt in
        let raw = match List.assoc_opt "raw" queries with
          | Some [ "true" ] -> true
          | Some _ | None -> false in
        Lwt.return (GET { target; ln; hl; raw; })

  let index_contents =
    let languages = List.map (fun c -> Language.string_of_language c, Language.value_of_language c) Language.all in
    let html = Form.html ~title:"Past-isserie" ~documentation:"Pasteur" languages in
    Fmt.strf "%a%!" (Tyxml.Html.pp ()) html

  let index _ reqd () =
    let headers = Headers.of_list [ "content-type", "text/html; charset=utf-8"
                                  ; "content-length", string_of_int (String.length index_contents) ] in
    let response = Response.create ~headers `OK in
    Reqd.respond_with_string reqd response index_contents ;
    Lwt.return ()

  let load console public reqd key () =
    Public.get public key >>= fun contents -> match contents, Mirage_kv.Key.segments key with
    | Error _, _ -> assert false
    | Ok contents, [ "highlight.pack.js" ] ->
      let headers = Headers.of_list
          [ "content-length", string_of_int (String.length contents)
          ; "content-type", "text/javascript" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "highlight.pack.js delivered!"
    | Ok contents, [ "pastisserie.css" ] ->
      let headers = Headers.of_list
          [ "content-length", string_of_int (String.length contents)
          ; "content-type", "text/css" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "pastisserie.css delivered!"
    | Ok contents, _ ->
      let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return ()

  let push console store remote ?(author= "pasteur") key value =
    log console "<<< store:%a." Store.Status.pp (Store.status store) >>= fun () ->
    let commit0 = match Store.status store with
      | `Branch branch ->
        Store.Branch.get (Store.repo store) branch
      | `Commit commit -> Lwt.return commit
      | `Empty -> failwith "Empty repository" in
    commit0 >>= fun commit0 ->
    let _, date = Clock.now_d_ps () in
    let info () = Irmin.Info.v ~date ~author (String.concat "/" key) in
    Store.set ~parents:[ commit0 ] ~info store key value >>= function
    | Error (`Conflict c) -> Fmt.failwith "Conflict! [%s]" c
    | Error (`Test_was _) | Error (`Too_many_retries _) -> failwith "Error to update local repository!"
    | Ok () -> Sync.push store remote >>= function
      | Ok `Empty -> Fmt.failwith "Got an empty repository"
      | Ok (`Head commit1) ->
        log console ">>> commit:%a -> commit:%a." Store.Commit.pp_hash commit0 Store.Commit.pp_hash commit1
      | Error `Detached_head -> failwith "Detached head!"
      | Error (`Msg err) -> failwith err

  let post random console store remote reqd () =
    match extract_content_type (Reqd.request reqd) with
    | None -> assert false (* TODO: redirect to INDEX. *)
    | Some content_type ->
      let body = Reqd.request_body reqd in
      extract_parts content_type body >>= function
      | Error _ as err -> Rresult.R.failwith_error_msg err
      | Ok posts -> match List.assoc Paste posts, List.assoc Hl posts with
        | contents, hl ->
          let random = random () in
          let hl = Language.language_of_value_exn hl in
          let author = List.assoc_opt User posts in
          let ln = List.exists (function (Ln, _) -> true | _ -> false) posts in
          let raw = List.exists (function (Raw, _) -> true | _ -> false) posts in
          push console store remote ?author [ random ] contents >>= fun () ->
          let uri = Uri.make
              ~path:random
              ~query:[ "ln", [ string_of_bool ln ]
                     ; "hl", [ Language.value_of_language hl ]
                     ; "raw", [ string_of_bool raw ] ]
              () in
          let headers = Headers.of_list [ "location", Uri.to_string uri
                                        ; "content-length", "0" ] in
          let response = Response.create ~headers `Found in
          Reqd.respond_with_string reqd response contents ;
          Lwt.return ()
        | exception Not_found ->
          let contents = "Bad POST request." in
          let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
          let response = Response.create ~headers `Not_found in
          Reqd.respond_with_string reqd response contents ;
          Lwt.return ()

  let main random console public store remote reqd = function
    | INDEX ->
      log console "> dispatch index." >>= index console reqd
    | GET { target; ln; hl; raw= false; } ->
      log console "> dispatch get:%a." Fmt.(Dump.list string) target
      >>= show ~ln ?hl console store remote reqd target
    | GET { target; raw= true; _ } ->
      log console "> dispatch raw:%a." Fmt.(Dump.list string) target
      >>= show_raw console store remote reqd target
    | CONTENTS key ->
      log console "> dispatch contents:%a." Mirage_kv.Key.pp key
      >>= load console public reqd key
    | POST ->
      log console "> dispatch post."
      >>= post random console store remote reqd

  let request_handler random console public store remote reqd =
    let open Httpaf in
    let res () =
      Lwt.catch
        (fun () -> dispatch public reqd >>= main random console public store remote reqd)
        (fun exn ->
           let res = Printexc.to_string exn in
           log console "Got an error: %s." res >>= fun () ->
           let headers = Headers.of_list [ "connection", "close" ] in
           let response = Response.create ~headers `Internal_server_error in
           Lwt.return (Reqd.respond_with_string reqd response (Printexc.to_string exn))) in
    Lwt.async res

  let error_handler ?request:_ _ _ = ()

  let fold_left ~f a s =
    let a = ref a in
    for i = 0 to String.length s - 1 do a := f !a s.[i] done ; !a

  let random_bytes ?g len () =
    let res = Bytes.create len in
    let pos = ref 0 in
    while !pos < len do
      let raw = Cstruct.to_string (Random.generate ?g 1024) in
      let safe = fold_left ~f:(fun a -> function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' as chr -> chr :: a | _ -> a) [] raw in
      List.iter (fun chr -> if !pos < len then ( Bytes.set res !pos chr ; incr pos )) safe
    done ; Bytes.unsafe_to_string res

  let start _ console _ public resolver conduit http =
    let random = random_bytes (Key_gen.random_length ()) in
    connect resolver conduit >>= fun (store, remote) ->
    Sync.pull store remote `Set >>= function
    | Ok `Empty -> failwith "Empty remote repository"
    | Ok (`Head _) ->
      let server = HTTP.create_connection_handler
          ~request_handler:(request_handler random console public store remote)
          ~error_handler
          ?config:None in

      http (`TCP (Key_gen.port ())) server
    | Error (`Msg err) -> failwith err
    | Error (`Conflict err) -> failwith err
end
