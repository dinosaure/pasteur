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

type key = Paste | User | Comment | Ln | Raw | Hl

let key_of_string = function
  | "paste" -> Some Paste
  | "user" -> Some User
  | "comment" -> Some Comment
  | "ln" -> Some Ln
  | "raw" -> Some Raw
  | "hl" -> Some Hl
  | _ -> None

let string_of_key = function
  | Paste -> "paste" | User -> "user" | Comment -> "comment" | Ln -> "ln"
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
  thread >>= fun res -> Body.close_reader body ; match res with
  | Error _ as err -> Lwt.return err
  | Ok _ ->
    let lst = Hashtbl.fold (fun k s a -> (k, Lwt_stream.to_list s) :: a) hashtbl [] in
    Lwt_list.map_p (fun (k, t) -> t >|= fun v -> (k, String.concat "" v)) lst >|= Rresult.R.ok


