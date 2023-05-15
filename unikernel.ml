open Lwt.Infix
open Lwt.Syntax
open Pasteur
open Httpaf

module Blob = struct
  type t = { contents : string; encrypted : bool }

  let t =
    let open Data_encoding in
    obj2 (req "contents" string) (dft "encrypted" bool false)
    |> conv
         (fun { contents; encrypted } -> (contents, encrypted))
         (fun (contents, encrypted) -> { contents; encrypted })

  let to_string_json v =
    let open Data_encoding in
    Json.construct t v |> Json.to_string

  let of_string_json str =
    let open Data_encoding in
    try
      match Json.from_string str with
      | Ok v -> Ok (Json.destruct t v)
      | Error _ -> Error (`Msg "Invalid JSON value")
    with exn ->
      Error (`Msg (Fmt.str "Invalid blob value: %S" (Printexc.to_string exn)))
end

let argument_error = 64

let key_type kt =
  match X509.Key_type.of_string kt with
  | Ok kt -> kt
  | Error (`Msg msg) ->
      Logs.err (fun m -> m "cannot decode key type %s: %s" kt msg);
      exit argument_error

module Make
    (Random : Mirage_random.S)
    (Console : Mirage_console.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Public : Mirage_kv.RO)
    (Stack : Tcpip.Stack.V4V6)
    (HTTP_Client : Http_mirage_client.S) (_ : sig end) =
struct
  module Nss = Ca_certs_nss.Make (Pclock)
  module Paf = Paf_mirage.Make (Stack.TCP)
  module LE = LE.Make (Time) (Stack)
  module Store = Git_kv.Make (Pclock)

  let ignore_error_handler _ ?request:_ _ _ = ()
  let log console fmt = Fmt.kstr (Console.log console) fmt

  let reload _console git key =
    Store.get git key >>= function
    | Ok str -> Lwt.return (Blob.of_string_json str)
    | Error (`Not_found _) -> (
        Git_kv.pull git >>= function
        | Ok _diff ->
            (Store.get git key
            >|= Rresult.R.(
                  reword_error (function
                    | `Not_found _ as err -> err
                    | err -> msgf "%a" Store.pp_error err)))
            >|= fun x -> Rresult.R.bind x Blob.of_string_json
        | Error (`Msg err) -> Lwt.return_error (`Msg err))
    | Error err -> Lwt.return_error (Rresult.R.msgf "%a" Store.pp_error err)

  let show ?ln:(_ = false) ?hl console git reqd target =
    let open Httpaf in
    let* () = log console "Want to access to: %a." Mirage_kv.Key.pp target in
    reload console git target >>= function
    | Error (`Msg err) ->
        let* () = log console "Got an error when reloading database: %s." err in
        let str = "Impossible to reload our internal database." in
        let headers =
          Headers.of_list
            [
              ("content-type", "text/plain");
              ("content-length", string_of_int (String.length str));
              ("connection", "close");
            ]
        in
        let response = Response.create ~headers `Internal_server_error in
        Reqd.respond_with_string reqd response str;
        Lwt.return_unit
    | Error (`Not_found _) ->
        let contents = Fmt.str "%a Not found." Mirage_kv.Key.pp target in
        let headers =
          Headers.of_list
            [ ("content-length", string_of_int (String.length contents)) ]
        in
        let response = Response.create ~headers `Not_found in
        Reqd.respond_with_string reqd response contents;
        log console "Response: 404 Not found for %a." Mirage_kv.Key.pp target
    | Ok { Blob.contents; encrypted } ->
        let code = Option.map Language.to_string hl in
        let html = Show.html ?code ~encrypted contents in
        let contents = Fmt.str "%a%!" (Tyxml.Html.pp ()) html in
        let headers =
          Headers.of_list
            [
              ("content-type", "text/html");
              ("content-length", string_of_int (String.length contents));
            ]
        in
        let response = Response.create ~headers `OK in
        Reqd.respond_with_string reqd response contents;
        Lwt.return_unit

  let show_raw console git reqd target =
    let open Httpaf in
    let* () =
      log console "Want to access to: %a (raw)." Mirage_kv.Key.pp target
    in
    reload console git target >>= function
    | Error (`Msg err) ->
        let* () = log console "Got an error when reloading database: %s." err in
        let str = "Impossible to reload our internal database." in
        let headers =
          Headers.of_list
            [
              ("content-type", "text/plain");
              ("content-length", string_of_int (String.length str));
              ("connection", "close");
            ]
        in
        let response = Response.create ~headers `Internal_server_error in
        Reqd.respond_with_string reqd response str;
        Lwt.return_unit
    | Error (`Not_found _) ->
        let contents = Fmt.str "%a not found." Mirage_kv.Key.pp target in
        let headers =
          Headers.of_list
            [ ("content-length", string_of_int (String.length contents)) ]
        in
        let response = Response.create ~headers `Not_found in
        Reqd.respond_with_string reqd response contents;
        log console "Response: 404 Not found for %a." Mirage_kv.Key.pp target
    | Ok { Blob.contents; _ } ->
        let headers =
          Headers.of_list
            [
              ("content-type", "text/plain; charset=utf-8");
              ("content-length", string_of_int (String.length contents));
            ]
        in
        let response = Response.create ~headers `OK in
        Reqd.respond_with_string reqd response contents;
        Lwt.return_unit

  type dispatch =
    | INDEX
    | GET of {
        target : Mirage_kv.Key.t;
        ln : bool;
        hl : Language.t option;
        raw : bool;
      }
    | CONTENTS of Public.key
    | POST

  let ln_default = false
  let raw_default = false

  type response = {
    ln : bool option;
    raw : bool option;
    hl : Language.t option;
    code : string;
  }

  let json =
    let open Json_encoding in
    let language =
      string_enum (List.map (fun v -> (Language.to_string v, v)) Language.all)
    in
    conv
      (fun { ln; raw; hl; code } -> (ln, raw, hl, code))
      (fun (ln, raw, hl, code) -> { ln; raw; hl; code })
      (obj4 (opt "ln" bool) (opt "raw" bool) (opt "hl" language)
         (req "code" string))

  (* Try to generate JSON as short as possible by taking advantage of the
     default values for the options (ln, hl, ...). *)
  let make_paste_json_string ?ln ?hl ?raw code =
    match
      Json_encoding.construct json { ln; raw; hl; code }
      |> Json_repr.Ezjsonm.repr
    with
    | #Ezjsonm.t as v -> Ezjsonm.to_string v
    | _ -> assert false

  let decode_paste_uri ~target ~queries =
    let query_bool ~default key =
      match List.assoc_opt key queries with
      | Some [ "true" ] | Some [] -> true
      | Some [ "false" ] -> false
      | _ -> default
    in
    let ln = query_bool ~default:ln_default "ln" in
    let hl =
      let ( >>= ) = Option.bind in
      let hd_opt = function [] -> None | x :: _ -> Some x in
      List.assoc_opt "hl" queries >>= hd_opt >>= fun hl ->
      match Language.of_string hl with v -> Some v | exception _ -> None
    in
    let raw = query_bool ~default:raw_default "raw" in
    GET { target; ln; hl; raw }

  let dispatch public reqd =
    let request = Reqd.request reqd in
    let target = Uri.of_string request.Request.target in
    let queries = Uri.query target in
    let target = Astring.String.trim ~drop:(Char.equal '/') (Uri.path target) in
    let target = Mirage_kv.Key.v target in

    let key = Mirage_kv.Key.v request.Request.target in
    Public.exists public key >>= function
    | Error err -> Fmt.invalid_arg "%a" Public.pp_error err
    | Ok (Some `Value) -> Lwt.return (CONTENTS key)
    | Ok (Some `Dictionary) | Ok None -> (
        match Mirage_kv.Key.segments target with
        | [] | [ "" ] ->
            if request.Request.meth = `POST then Lwt.return POST
            else Lwt.return INDEX
        | _ :: _ -> Lwt.return (decode_paste_uri ~target ~queries))

  let index_contents =
    let languages =
      ("No highlighting", None)
      :: List.map
           (fun c -> (Language.to_string c, Some (Language.to_string c)))
           Language.all
    in
    let html =
      Form.html ~title:"Past-isserie" ~documentation:"Pasteur" languages
    in
    Fmt.str "%a%!" (Tyxml.Html.pp ()) html

  let index _ reqd =
    let headers =
      Headers.of_list
        [
          ("content-type", "text/html; charset=utf-8");
          ("content-length", string_of_int (String.length index_contents));
        ]
    in
    let response = Response.create ~headers `OK in
    Reqd.respond_with_string reqd response index_contents;
    Lwt.return ()

  let load console public reqd key =
    Public.get public key >>= fun contents ->
    match (contents, Mirage_kv.Key.segments key) with
    | Error _, _ -> assert false
    | Ok contents, (([ "highlight.js" ] | [ "pasteur.js" ]) as path) ->
        let headers =
          Headers.of_list
            [
              ("content-length", string_of_int (String.length contents));
              ("content-type", "text/javascript");
              ("connection", "close");
            ]
        in
        let response = Response.create ~headers `OK in
        Reqd.respond_with_string reqd response contents;
        log console "%s delivered!" (String.concat "/" path)
    | Ok contents, [ "pastisserie.css" ] ->
        let headers =
          Headers.of_list
            [
              ("content-length", string_of_int (String.length contents));
              ("content-type", "text/css");
              ("connection", "close");
            ]
        in
        let response = Response.create ~headers `OK in
        Reqd.respond_with_string reqd response contents;
        log console "pastisserie.css delivered!"
    | Ok contents, _ ->
        let headers =
          Headers.of_list
            [
              ("content-length", string_of_int (String.length contents));
              ("connection", "close");
            ]
        in
        let response = Response.create ~headers `OK in
        Reqd.respond_with_string reqd response contents;
        Lwt.return_unit

  let push console git key value =
    ( Store.change_and_push git @@ fun git ->
      Store.set git key (Blob.to_string_json value) )
    >|= Result.join

  let is_on = ( = ) "on"

  let post random console git reqd =
    match extract_content_type (Reqd.request reqd) with
    | None ->
        let contents = "Bad POST request (invalid or missing Content-Type)." in
        let headers =
          Headers.of_list
            [
              ("content-type", "text/plain");
              ("content-length", string_of_int (String.length contents));
            ]
        in
        let response = Response.create ~headers `Bad_request in
        Reqd.respond_with_string reqd response contents;
        Lwt.return_unit
    | Some content_type -> (
        let body = Reqd.request_body reqd in
        extract_parts content_type body >>= function
        | Error `Too_big_paste ->
            let* () = log console "Got a big paste, return a bad request." in
            let contents = "Too big paste." in
            let headers =
              Headers.of_list
                [
                  ("content-type", "text/plain");
                  ("content-length", string_of_int (String.length contents));
                ]
            in
            let response = Response.create ~headers `Bad_request in
            Reqd.respond_with_string reqd response contents;
            Lwt.return_unit
        | Error (`Msg err) ->
            let* () =
              log console
                "Got an error when extracting multipart/form contents: %s." err
            in
            let contents = "Bad POST request (malformed POST request)." in
            let headers =
              Headers.of_list
                [
                  ("content-type", "text/plain");
                  ("content-length", string_of_int (String.length contents));
                ]
            in
            let response = Response.create ~headers `Bad_request in
            Reqd.respond_with_string reqd response contents;
            Lwt.return_unit
        | Ok posts -> (
            match
              ( List.assoc Paste posts,
                List.assoc_opt Hl posts,
                List.assoc_opt Encrypted posts )
            with
            | contents, hl, encrypted -> (
                let random = random () in
                let encrypted = Option.fold ~none:false ~some:is_on encrypted in
                let hl =
                  try Option.map Language.of_string hl with _ -> None
                in
                let _author = List.assoc_opt User posts in
                let ln =
                  List.exists (function Ln, _ -> true | _ -> false) posts
                in
                let raw =
                  List.exists (function Raw, _ -> true | _ -> false) posts
                in
                push console git
                  Mirage_kv.Key.(empty / random)
                  { Blob.contents; encrypted }
                >>= function
                | Ok () ->
                    let str = make_paste_json_string ~ln ?hl ~raw random in
                    let headers =
                      Headers.of_list
                        [
                          ("content-type", "application/json");
                          ("content-length", string_of_int (String.length str));
                          ("access-control-allow-origin", "*");
                        ]
                    in
                    let response = Response.create ~headers `OK in
                    Reqd.respond_with_string reqd response str;
                    Lwt.return_unit
                | Error err ->
                    let err = Rresult.R.msgf "%a" Store.pp_write_error err in
                    let* () =
                      log console "Got an error when pushing: %a."
                        Store.pp_write_error err
                    in
                    let str =
                      "Got an error when updating our internal database."
                    in
                    let headers =
                      Headers.of_list
                        [
                          ("content-type", "text/plain");
                          ("content-length", string_of_int (String.length str));
                        ]
                    in
                    let response =
                      Response.create ~headers `Internal_server_error
                    in
                    Reqd.respond_with_string reqd response str;
                    Lwt.return_unit)
            | exception Not_found ->
                let contents = "Bad POST request (content is required)." in
                let headers =
                  Headers.of_list
                    [
                      ("content-type", "text/plain");
                      ("content-length", string_of_int (String.length contents));
                    ]
                in
                let response = Response.create ~headers `Bad_request in
                Reqd.respond_with_string reqd response contents;
                Lwt.return_unit))

  let main random console public git reqd = function
    | INDEX ->
        let* () = log console "[dispatch] index" in
        index console reqd
    | GET { target; ln; hl; raw = false } ->
        let* () = log console "[dispatch] get:%a" Mirage_kv.Key.pp target in
        show ~ln ?hl console git reqd target
    | GET { target; raw = true; _ } ->
        let* () = log console "[dispatch] raw:%a" Mirage_kv.Key.pp target in
        show_raw console git reqd target
    | CONTENTS key ->
        let* () = log console "[dispatch] contents:%a" Mirage_kv.Key.pp key in
        load console public reqd key
    | POST ->
        let* () = log console "[dispatch] post." in
        post random console git reqd

  let request_handler random console public git (_ipaddr, _port) reqd =
    let open Httpaf in
    let res () =
      Lwt.catch
        (fun () -> dispatch public reqd >>= main random console public git reqd)
        (fun exn ->
          let res = Printexc.to_string exn in
          let* () = log console "Got an error: %s" res in
          let headers = Headers.of_list [ ("connection", "close") ] in
          let response = Response.create ~headers `Internal_server_error in
          Lwt.return
            (Reqd.respond_with_string reqd response (Printexc.to_string exn)))
    in
    Lwt.async res

  let fold_left ~f a s =
    let a = ref a in
    for i = 0 to String.length s - 1 do
      a := f !a s.[i]
    done;
    !a

  let random_bytes ?g len () =
    let res = Bytes.create len in
    let pos = ref 0 in
    while !pos < len do
      let raw = Cstruct.to_string (Random.generate ?g 1024) in
      let safe =
        fold_left
          ~f:
            (fun a -> function
              | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as chr -> chr :: a
              | _ -> a)
          [] raw
      in
      List.iter
        (fun chr ->
          if !pos < len then (
            Bytes.set res !pos chr;
            incr pos))
        safe
    done;
    Bytes.unsafe_to_string res

  let ( >>? ) = Lwt_result.bind
  let exit_expired = 80

  let quit_before_expire = function
    | `Single (server :: _, _) as tls ->
        let expiry = snd (X509.Certificate.validity server) in
        let diff = Ptime.diff expiry (Ptime.v (Pclock.now_d_ps ())) in
        (match Ptime.Span.to_int_s diff with
        | None -> invalid_arg "Couldn't convert span to seconds"
        | Some x when x < 0 -> invalid_arg "Diff is negative"
        | Some x ->
            Lwt.async @@ fun () ->
            Time.sleep_ns Int64.(sub (Duration.of_sec x) (Duration.of_day 1))
            >|= fun () -> exit exit_expired);
        Lwt.return tls
    | tls -> Lwt.return tls

  let provision ~production cfg stack http_client =
    Paf.init ~port:80 (Stack.tcp stack) >>= fun t ->
    let service =
      Paf.http_service ~error_handler:ignore_error_handler (fun _ ->
          LE.request_handler)
    in
    let stop = Lwt_switch.create () in
    let (`Initialized th0) = Paf.serve ~stop service t in
    let th1 =
      LE.provision_certificate ~production cfg http_client
      >>? fun certificates ->
      Lwt_switch.turn_off stop >>= fun () -> Lwt.return_ok certificates
    in
    Lwt.both th0 th1 >>= function
    | (), Error (`Msg err) -> failwith err
    | (), Ok certificates -> Lwt.return certificates

  let start _random console _time _mclock _pclock public stack http_client ctx
      _js _hljs =
    let seed = random_bytes (Key_gen.random_length ()) in
    Git_kv.connect ctx (Key_gen.remote ()) >>= fun git ->
    match Key_gen.https () with
    | false ->
        Logs.info (fun m -> m "Initialise an HTTP server (no HTTPS).");
        let request_handler _flow = request_handler seed console public git in
        let port = Option.value ~default:80 (Key_gen.port ()) in
        Paf.init ~port (Stack.tcp stack) >>= fun service ->
        let http =
          Paf.http_service ~error_handler:ignore_error_handler request_handler
        in
        let (`Initialized th) = Paf.serve http service in
        th
    | true ->
        Logs.info (fun m -> m "Download TLS certificate.");
        provision ~production:(Key_gen.production ())
          {
            LE.certificate_seed = Key_gen.cert_seed ();
            LE.certificate_key_type = key_type (Key_gen.cert_key_type ());
            LE.certificate_key_bits = Some (Key_gen.cert_bits ());
            LE.email =
              Option.bind (Key_gen.email ()) (fun e ->
                  Emile.of_string e |> Result.to_option);
            LE.account_seed = Key_gen.account_seed ();
            LE.account_key_type = key_type (Key_gen.account_key_type ());
            LE.account_key_bits = Some (Key_gen.account_bits ());
            LE.hostname =
              Key_gen.hostname () |> Option.get |> Domain_name.of_string_exn
              |> Domain_name.host_exn;
          }
          stack http_client
        >>= quit_before_expire
        >>= fun certificates ->
        Logs.info (fun m -> m "Got a TLS certificate for the server.");
        let tls = Tls.Config.server ~certificates () in
        let request_handler _flow = request_handler seed console public git in
        let port = Option.value ~default:443 (Key_gen.port ()) in
        Paf.init ~port (Stack.tcp stack) >>= fun service ->
        let https =
          Paf.https_service ~tls ~error_handler:ignore_error_handler
            request_handler
        in
        let (`Initialized th) = Paf.serve https service in
        th
end
