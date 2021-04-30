open Httpaf

module Option = struct
  let bind a f = match a with Some a -> f a | None -> None
  let map f = function Some x -> Some (f x) | None -> None
  let ( >>= ) = bind
end

let extract_content_type request =
  match Headers.get request.Request.headers "content-type" with
  | Some v -> Multipart_form.Content_type.of_string (v ^ "\r\n")
  | None -> Rresult.R.error_msgf "Content-Type field not found"

type key = Paste | User | Comment | Ln | Raw | Hl | Encrypted

let key_of_string = function
  | "paste" -> Some Paste
  | "user" -> Some User
  | "comment" -> Some Comment
  | "ln" -> Some Ln
  | "raw" -> Some Raw
  | "hl" -> Some Hl
  | "encrypted" -> Some Encrypted
  | _ -> None

let get_key header =
  let open Multipart_form in
  let ( >>= ) = Option.bind in
  Header.content_disposition header >>= fun v ->
  Content_disposition.name v >>= fun n ->
  key_of_string n

let string_of_key = function
  | Paste -> "paste" | User -> "user" | Comment -> "comment" | Ln -> "ln"
  | Raw -> "raw" | Hl -> "hl" | Encrypted -> "encrypted"

let src = Logs.Src.create "multipart"
module Log = (val Logs.src_log src : Logs.LOG)

let ( >|? ) f x = Lwt_result.map x f
let ( >>? ) = Lwt_result.bind

let extract_parts content_type body =
  let stream, pusher = Lwt_stream.create () in
  let rec on_read buf ~off ~len =
    Log.debug (fun m -> m "Read %d byte(s) from the body." len) ;
    let str = Bigstringaf.substring buf ~off ~len in
    pusher (Some str) ;
    Body.schedule_read body ~on_eof ~on_read
  and on_eof () =
    Log.debug (fun m -> m "End of body.") ;
    pusher None ;
    Body.close_reader body in
  Body.schedule_read body ~on_eof ~on_read ;
  Multipart_form_lwt.of_stream_to_tree stream content_type >>? fun tree ->
  Log.debug (fun m -> m "Got the tree from the multipart/form-data stream.") ;
  Lwt.return_ok (Multipart_form.flatten tree) >|?
  List.fold_left (fun acc { Multipart_form.header; body; } -> match get_key header with
    | Some key -> (key, body) :: acc
    | None -> acc) []
