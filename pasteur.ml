open Httpaf
open Lwt.Infix

let src = Logs.Src.create "pasteur" ~doc:"logs git's pasteur"
module Log = (val Logs.src_log src : Logs.LOG)

let extract_content_type request =
  let headers = request.Request.headers in
  match Httpaf.Headers.get headers "content-type" with
  | None -> None
  | Some str ->
    match Multipart_form.Content_type.of_string (str ^ "\r\n") with
    | Ok v -> Some v
    | Error (`Msg _err) -> None

type key =
  | Paste
  | User
  | Comment
  | Ln
  | Raw
  | Hl
  | Encrypted

let key_of_string = function
  | "paste" -> Some Paste
  | "user" -> Some User
  | "comment" -> Some Comment
  | "ln" -> Some Ln
  | "raw" -> Some Raw
  | "hl" -> Some Hl
  | "encrypted" -> Some Encrypted
  | _ -> None

let identify header =
  let open Multipart_form in
  let ( >>= ) = Option.bind in
  let ( >>| ) x f = Option.map f x in
  Header.content_disposition header
  >>= Content_disposition.name
  >>| String.lowercase_ascii
  >>= key_of_string

let string_of_key = function
  | Paste -> "paste"
  | User -> "user"
  | Comment -> "comment"
  | Ln -> "ln"
  | Raw -> "raw"
  | Hl -> "hl"
  | Encrypted -> "encrypted"

let stream_of_body body =
  let stream, push = Lwt_stream.create () in
  let rec on_eof () =
    push None
  and on_read buf ~off ~len =
    let str = Bigstringaf.substring buf ~off ~len in
    Log.debug (fun m -> m "Received: @[<hov>%a@]" (Hxd_string.pp Hxd.default) str) ;
    push (Some str) ;
    Httpaf.Body.schedule_read body ~on_eof ~on_read in
  Httpaf.Body.schedule_read body ~on_eof ~on_read ;
  stream

let extract_parts content_type body =
  let stream = stream_of_body body in
  let `Parse th, stream = Multipart_form_lwt.stream
    ~identify stream content_type in
  th >>= fun result ->
  Httpaf.Body.close_reader body ;
  match result with
  | Error _ as err -> Lwt.return err
  | Ok _tree ->
    Lwt_stream.to_list stream
    >>= Lwt_list.filter_map_s (fun (id, headers, stream) ->
      Lwt_stream.to_list stream
      >|= String.concat ""
      >>= fun contents -> match id with
      | Some key -> Lwt.return_some (key, contents)
      | None -> Lwt.return_none)
    >>= fun bindings -> Lwt.return_ok bindings
