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

let always x = fun _ -> x

let guess_size headers =
  let open Multipart_form in
  match Header.assoc (Field_name.v "Content-Length") headers with
  | Field.Field (_, Field.Field, v) :: _ ->
    let str = Unstrctrd.to_utf_8_string v in
    Int64.of_string_opt str
  | _ -> None

let to_bindings stream =
  let rec go acc stream =
    Lwt_stream.get stream >>= function
    | None -> Lwt.return_ok acc
    | Some (id, headers, stream') ->
      let size = guess_size headers in
      match id, size with
      | Some key, Some size when size >= 1_000_000L ->
        Lwt.return_error `Too_big_paste
      | Some key, Some _ ->
        Lwt_stream.to_list stream' >|= String.concat "" >>= fun contents ->
        go ((key, contents) :: acc) stream
      | Some key, None ->
        let rec flat (contents, size) stream' = Lwt_stream.get stream' >>= function
          | Some chunk when String.length chunk + size >= 1_000_000 ->
            Lwt.return_error `Too_big_paste
          | Some chunk -> flat (chunk :: contents, size + String.length chunk) stream'
          | None -> Lwt.return_ok (String.concat "" (List.rev contents)) in
        ( flat ([], 0) stream' >>= function
        | Ok contents -> go ((key, contents) :: acc) stream
        | Error _ as err -> Lwt.return err )
      | None, _ ->
        Lwt_stream.junk_while (always true) stream' >>= fun () ->
        go acc stream in
  go [] stream

let extract_parts content_type body =
  let stream = stream_of_body body in
  let `Parse th, stream = Multipart_form_lwt.stream
    ~identify stream content_type in
  Lwt.both th (to_bindings stream) >>= fun (res0, res1) ->
  Httpaf.Body.close_reader body ;
  match res0, res1 with
  | Error err0, _ -> Lwt.return_error err0
  | Ok _tree, Error err1 -> Lwt.return_error err1
  | Ok _tree, Ok bindings -> Lwt.return_ok bindings
