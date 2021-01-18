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

let parse_content_type str =
  Multipart_form.Content_type.of_string (str ^ "\r\n")

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

let pp_string ppf x = Fmt.pf ppf "%S" x
let blit src src_off dst dst_off len = Bigstringaf.blit src ~src_off dst ~dst_off ~len

module Qe = Ke.Rke
let src = Logs.Src.create "multipart"
module Log = (val Logs.src_log src : Logs.LOG)

let extract_parts content_type body =
  let open Angstrom.Unbuffered in

  let hashtbl = Hashtbl.create 7 in
  let emitters header = 
    match get_key header with
    | Some key ->
      let stream, push = Lwt_stream.create () in
      Hashtbl.add hashtbl key stream ; push, Some key
    | None -> (fun _ -> ()), None in

  let thread, finished = Lwt.task () in
  let state = ref (parse (Multipart_form.parser ~emitters content_type)) in
  let ke = Qe.create ~capacity:0x1000 Bigarray.Char in

  let rec on_eof () =
    match !state with
    | Partial { continue; committed; } ->
      Qe.N.shift_exn ke committed ;
      if committed = 0 then Qe.compress ke ;
      ( match Qe.N.peek ke with
        | [] -> state := continue Bigstringaf.empty ~off:0 ~len:0 Complete
        | [ slice ] -> state := continue slice ~off:0 ~len:(Bigstringaf.length slice) Complete
        | slice :: _ -> state := continue slice ~off:0 ~len:(Bigstringaf.length slice) Complete ) ;
      on_eof ()
    | Fail _ -> Lwt.wakeup finished (Rresult.R.error_msgf "bad POST request")
    | Done (_, v) -> Lwt.wakeup finished (Rresult.R.ok v)
  and on_read buf ~off ~len =
    match !state with
    | Partial { continue; committed; } ->
      Qe.N.shift_exn ke committed ;
      if committed = 0 then Qe.compress ke ;
      Qe.N.push ke ~blit ~length:Bigstringaf.length ~off ~len buf ;
      Log.debug (fun m -> m "Length of internal queue: %x byte(s)." (Qe.length ke)) ;
      if Qe.capacity ke >= 0x10000 then Lwt.wakeup finished (Rresult.R.error_msgf "POST buffer is too big!") ;
      if not (Qe.is_empty ke)
      then ( let[@warning "-8"] slice :: _ = Qe.N.peek ke in
             state := continue slice ~off:0 ~len:(Bigstringaf.length slice) Incomplete ) ;
      Body.schedule_read body ~on_eof ~on_read
    | Fail _ -> Lwt.wakeup finished (Rresult.R.error_msgf "bad POST request")
    | Done (_, v) -> Lwt.wakeup finished (Rresult.R.ok v) in
  let open Lwt.Infix in
  Body.schedule_read body ~on_eof ~on_read ;
  thread >>= fun res -> Body.close_reader body ; match res with
  | Error _ as err -> Lwt.return err
  | Ok _ ->
    let lst = Hashtbl.fold (fun k s a -> (k, Lwt_stream.to_list s) :: a) hashtbl [] in
    Lwt_list.map_p (fun (k, t) -> t >|= fun v -> (k, String.concat "" v)) lst >|= Rresult.R.ok
