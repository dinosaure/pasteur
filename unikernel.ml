open Lwt.Infix
open Pasteur
open Httpaf

module Make
    (Random : Mirage_random.S)
    (Console : Mirage_console.S)
    (Time : Mirage_time.S)
    (Clock : Mirage_clock.PCLOCK)
    (Public : Mirage_kv.RO)
    (StackV4 : Mirage_stack.V4)
    (Resolver : Resolver_lwt.S)
    (Conduit : Conduit_mirage.S)
= struct
  module Paf = Paf.Make(Time)(StackV4)
  module Store = Irmin_mirage_git.Mem.KV(Irmin.Contents.String)
  module Sync = Irmin.Sync(Store)

  let log console fmt = Fmt.kstrf (Console.log console) fmt

  let connect resolver conduit =
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >|= fun repository ->
    repository, Store.remote ~conduit ~resolver (Key_gen.remote ())
  (* TODO(dinosaure): try to remove [conduit]. *)

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

  let show ?ln:(_ = false) ?hl console store remote reqd target () =
    let open Httpaf in
    log console "Want to access to: %a." Fmt.(Dump.list string) target >>= fun () ->
    load console store remote target >>= function
    | None ->
      let contents = Fmt.strf "%s Not found." (String.concat "/" target) in
      let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
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
      let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
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

  let ln_default = false
  let raw_default = false

  (* Try to generate URIs as short as possible by taking advantage of the
     default values for the options (ln, hl, ...). *)
  let mk_paste_uri ~id ~ln ~hl ~raw =
    let (@::) o ls = match o with None -> ls | Some x -> x :: ls in
    let boolopt ~default key x =
      if x = default then None
      else Some (
        (* short form for the "=true" case *)
        if x then (key, []) else (key, ["false"])
      ) in
    let query =
      (boolopt ~default:ln_default "ln" ln) @::
      (boolopt ~default:raw_default "raw" raw) @::
      (Option.map (fun s -> "hl", [Language.value_of_language s]) hl) @::
      []
    in
    Uri.make ~path:id ~query ()

  let decode_paste_uri ~target ~queries =
    let query_bool ~default key =
      match List.assoc_opt key queries with
      | Some [ "true" ] | Some [] -> true
      | Some [ "false" ] -> false
      | _ -> default
    in
    let ln = query_bool ~default:ln_default "ln" in
    let hl = let open Option in
      List.assoc_opt "hl" queries >>= List.hd_opt >>=
      Language.language_of_value_opt in
    let raw = query_bool ~default:raw_default "raw" in
    GET { target; ln; hl; raw; }

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
      | [] | [ "" ] ->
        if request.Request.meth = `POST
        then Lwt.return POST
        else Lwt.return INDEX
      | _ :: _ -> Lwt.return (decode_paste_uri ~target ~queries)

  let index_contents =
    let languages =
      ("No highlighting", None) ::
      List.map (fun c ->
        Language.string_of_language c, Some (Language.value_of_language c)
      ) Language.all
    in
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
    | Ok contents, ([ "highlight.pack.js" ] | [ "pasteur.js" ] | [ "sjcl.js" ]) ->
      let headers = Headers.of_list
          [ "content-length", string_of_int (String.length contents)
          ; "content-type", "text/javascript"
          ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "highlight.pack.js delivered!"
    | Ok contents, [ "pastisserie.css" ] ->
      let headers = Headers.of_list
          [ "content-length", string_of_int (String.length contents)
          ; "content-type", "text/css"
          ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "pastisserie.css delivered!"
    | Ok contents, _ ->
      let headers = Headers.of_list
          [ "content-length", string_of_int (String.length contents)
          ; "connection", "close" ] in
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
          let hl = Language.language_of_value_opt hl in
          let author = List.assoc_opt User posts in
          let ln = List.exists (function (Ln, _) -> true | _ -> false) posts in
          let raw = List.exists (function (Raw, _) -> true | _ -> false) posts in
          push console store remote ?author [ random ] contents >>= fun () ->
          let uri = mk_paste_uri ~ln ~hl ~raw ~id:random in
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

  let request_handler random console public store remote (_ipaddr, _port) reqd =
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

  let error_handler _ ?request:_ _ _ = ()

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

  let ( >>? ) x f = x >>= function
    | Ok x -> f x
    | Error err -> Lwt.return (Error err)

  let start _ console _ _ public stack resolver conduit =
    let random = random_bytes (Key_gen.random_length ()) in
    connect resolver conduit >>= fun (store, remote) ->
    Sync.pull store remote `Set >>= function
    | Ok `Empty -> failwith "Empty remote repository"
    | Ok (`Head _) ->
      let config =
        { Tuyau_mirage_tcp.port= Key_gen.port ()
        ; Tuyau_mirage_tcp.keepalive= None
        ; Tuyau_mirage_tcp.nodelay= false
        ; Tuyau_mirage_tcp.stack } in
      let request_handler = request_handler random console public store remote in
      let loop () =
        Tuyau_mirage.serve ~key:Paf.TCP.configuration config ~service:Paf.TCP.service >>? fun (master, _) ->
        Paf.http ~request_handler ~error_handler master in
      ( loop () >>= function
        | Ok () -> (* TODO(dinosaure): properly close [master]. *) Lwt.return ()
        | Error err ->
          log console "Got an error: %a." Tuyau_mirage.pp_error err )
    | Error (`Msg err) -> failwith err
    | Error (`Conflict err) -> failwith err
end
