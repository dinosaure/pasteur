open Lwt.Infix
open Httpaf

module Checkseum = Checkseum
module Digestif = Digestif

module Option = struct
  let bind a f = match a with Some a -> f a | None -> None
  let ( >>= ) = bind
end

let formatter_of_body body =
  let output x off len = Body.write_string body ~off ~len x in
  let flush () = Body.flush body (fun () -> ()) in
  Format.make_formatter output flush

let parse_content_type v =
  let open Angstrom in
  parse_string Multipart_form.(Rfc2045.content <* Rfc822.crlf) (v ^"\r\n")

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

let extract_parts content_type body =
  let open Angstrom.Unbuffered in

  let hashtbl = Hashtbl.create 7 in
  let emitters fields = match Option.(name_of_fields fields >>= key_of_string) with
    | Some key ->
      let stream, push = Lwt_stream.create () in
      Hashtbl.add hashtbl key stream ; push, Some key
    | None -> (fun _ -> ()), None in
  let state = ref (parse (Multipart_form.parser ~emitters content_type)) in
  let tree = ref None in
  let on_eof () = match !state with
    | Partial { continue; _ } ->
      state := continue Bigstringaf.empty ~off:0 ~len:0 Complete
    | Fail _ -> assert false
    | Done (_, v) -> tree := Some v in
  let on_read buf ~off ~len = match !state with
    | Partial { continue; _ } ->
      state := continue buf ~off ~len Incomplete
    | Fail _ -> assert false
    | Done (_, v) -> tree := Some v in
  Body.schedule_read body ~on_eof ~on_read ;
  Body.close_reader body ;

  let lst = Hashtbl.fold (fun k s a -> (k, Lwt_stream.to_list s) :: a) hashtbl [] in

  let open Lwt.Infix in
  Lwt_list.map_p (fun (k, t) -> t >|= fun v -> (k, String.concat "" v)) lst

type contents =
  { contents : string
  ; hl : Language.t
  ; with_ln : bool
  ; raw : bool
  ; author : string option
  ; comment : string option }
and code = Language.t

let contents =
  let open Irmin.Type in
  record "contents" (fun contents hl with_ln raw author comment -> { contents; hl; with_ln; raw; author; comment; })
  |+ field "contents" string (fun t -> t.contents)
  |+ field "hl" Language.witness (fun t -> t.hl)
  |+ field "with_ln" bool (fun t -> t.with_ln)
  |+ field "raw" bool (fun t -> t.raw)
  |+ field "author" (option string) (fun t -> t.author)
  |+ field "comment" (option string) (fun t -> t.comment)
  |> sealr

module T = struct
  type t = contents

  let t = contents
  let merge = Irmin.Merge.(option (default contents))
end

module Make
    (Random : Mirage_types_lwt.RANDOM)
    (Console : Mirage_types_lwt.CONSOLE)
    (Clock : Mirage_types_lwt.PCLOCK)
    (Public : Mirage_types_lwt.KV_RO)
    (Resolver : Resolver_lwt.S)
    (Conduit : Conduit_mirage.S)
    (HTTP : Httpaf_mirage.Server_intf)
= struct
  module Store = Irmin_mirage_git.Mem.KV(T)
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
      | Error (`Msg err) -> invalid_arg err
      | Error (`Conflict err) -> invalid_arg err

  let show console store remote reqd target () =
    let open Httpaf in
    let request = Reqd.request reqd in
    log console "Want to access to: %a." Fmt.(Dump.list string) target >>= fun () ->
    load console store remote target >>= function
    | None ->
      let contents = "Not found." in
      let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `Not_found in
      Reqd.respond_with_string reqd response contents ;
      log console "Response: 404 Not found."
    | Some { contents; hl; _ } ->
      let headers = Headers.of_list [ "transfer-encoding", "chunked" ] in
      let response = Response.create ~headers `OK in
      let html = Show.html ~code:(Language.value_of_language hl) contents in
      let body = Reqd.respond_with_streaming reqd response in
      let ppf = formatter_of_body body in
      Tyxml.Html.pp () ppf html ;
      Body.close_writer body ;
      Lwt.return ()

  type dispatch = INDEX | GET of string list | CONTENTS of Public.key | POST

  let dispatch public reqd =
    let request = Reqd.request reqd in
    let target = Astring.String.trim ~drop:(Char.equal '/') request.Request.target in
    let target = Astring.String.cuts ~sep: "/" target in

    let key = Mirage_kv.Key.v request.Request.target in
    Public.exists public key >>= function
    | Error err -> Fmt.invalid_arg "%a" Public.pp_error err
    | Ok (Some `Value) -> Lwt.return (CONTENTS key)
    | Ok (Some `Dictionary) | Ok None ->
      match target with
      | [] | [ "" ]->
        if request.Request.meth = `POST
        then Lwt.return POST
        else Lwt.return INDEX
      | _ :: _ -> Lwt.return (GET target)

  let index console reqd () =
    let headers = Headers.of_list [ "transfer-encoding", "chunked" ] in
    let response = Response.create ~headers `OK in
    let languages = List.map (fun c -> Language.string_of_language c, Language.value_of_language c) Language.all in
    let html = Form.html ~title:"Past-isserie" ~documentation:"Pasteur" languages in
    let body = Reqd.respond_with_streaming reqd response in
    let ppf = formatter_of_body body in
    Tyxml.Html.pp () ppf html ;
    Body.close_writer body ;
    Lwt.return ()

  let load console public reqd key () =
    Public.get public key >>= function
    | Error _ -> assert false
    | Ok contents ->
      let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return ()

  let push console store remote ?(author= "pasteur") key value =
    log console "<<< %a." Store.Status.pp (Store.status store) >>= fun () ->
    let commit0 = match Store.status store with
      | `Branch branch ->
        Store.Branch.get (Store.repo store) branch
      | `Commit commit -> Lwt.return commit
      | `Empty -> invalid_arg "Empty repository" in
    commit0 >>= fun commit0 ->
    let _, date = Clock.now_d_ps () in
    let info () = Irmin.Info.v ~date ~author (String.concat "/" key) in
    Store.set ~parents:[ commit0 ] ~info store key value >>= function
    | Error (`Conflict c) -> Fmt.invalid_arg "Conflict! [%s]" c
    | Error (`Test_was _) | Error (`Too_many_retries _) -> invalid_arg "Error to update local repository!"
    | Ok () -> Sync.push store remote >>= function
      | Ok `Empty -> Fmt.invalid_arg "Got an empty repository"
      | Ok (`Head commit1) ->
        log console ">>> %a -> %a." Store.Commit.pp_hash commit0 Store.Commit.pp_hash commit1
      | Error `Detached_head -> invalid_arg "Detached head!"
      | Error (`Msg err) -> invalid_arg err

  let post console store remote reqd () =
    match extract_content_type (Reqd.request reqd) with
    | None -> assert false
    | Some content_type ->
      let body = Reqd.request_body reqd in
      extract_parts content_type body >>= fun posts -> match List.assoc Paste posts, List.assoc Hl posts with
      | contents, hl ->
        let random = Cstruct.to_string (Random.generate 16) in
        let uuid = Uuidm.v4 (Bytes.of_string random) in
        let hl = Language.language_of_value hl in
        let author = List.assoc_opt User posts in
        let comment = List.assoc_opt Comment posts in
        let with_ln = List.exists (function (Ln, _) -> true | _ -> false) posts in
        let raw = List.exists (function (Raw, _) -> true | _ -> false) posts in
        let value = { contents; hl; with_ln; raw; author; comment; } in
        let key = Uuidm.to_string uuid in
        push console store remote ?author [ key ] value >>= fun () ->
        let headers = Headers.of_list [ "location", key; "content-length", "0" ] in
        let response = Response.create ~headers `Found in
        Reqd.respond_with_string reqd response contents ;
        Lwt.return ()
      | exception Not_found ->
        let contents = "Not found." in
        let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
        let response = Response.create ~headers `Not_found in
        Reqd.respond_with_string reqd response contents ;
        Lwt.return ()

  let main console public store remote reqd = function
    | INDEX ->
      log console "> dispatch index." >>= index console reqd
    | GET target ->
      log console "> dispatch get:%a." Fmt.(Dump.list string) target >>= show console store remote reqd target
    | CONTENTS key ->
      log console "> dispatch contents:%a." Mirage_kv.Key.pp key >>= load console public reqd key
    | POST ->
      log console "> dispatch post." >>= post console store remote reqd

  let request_handler console public store remote reqd =
    let open Httpaf in
    let res =
      Lwt.catch
        (fun () -> dispatch public reqd >>= main console public store remote reqd)
        (fun exn ->
           let res = Printexc.to_string exn in
           log console "Got an error: %s." res >>= fun () ->
           let headers = Headers.of_list [ "content-length", string_of_int (String.length res) ] in
           let response = Response.create ~headers `Internal_server_error in
           Lwt.return (Reqd.respond_with_string reqd response (Printexc.to_string exn))) in
    Lwt.async (fun () -> res)

  let error_handler ?request error k = ()

  let start random console clock public resolver conduit http =
    connect resolver conduit >>= fun (store, remote) ->
    Sync.pull store remote `Set >>= function
    | Ok `Empty -> failwith "Empty remote repository"
    | Ok (`Head _) ->
      let server = HTTP.create_connection_handler
          ~request_handler:(request_handler console public store remote)
          ~error_handler
          ?config:None in

      http (`TCP 4343) server
    | Error (`Msg err) -> invalid_arg err
    | Error (`Conflict err) -> invalid_arg err
end
