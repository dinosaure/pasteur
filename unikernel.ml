open Lwt.Infix
open Lwt.Syntax
open Pasteur
open Httpaf

module Blob = struct
  type t =
    { contents : string
    ; encrypted : bool }

  let t =
    let open Irmin.Type in
    record "blob"
      (fun contents encrypted -> { contents; encrypted; })
    |+ field "contents" string (fun t -> t.contents)
    |+ field "encrypted" bool (fun t -> t.encrypted)
    |> sealr

  let merge = Irmin.Merge.(option (idempotent t))
end

let argument_error = 64

let key_type kt =
  match X509.Key_type.of_string kt with
  | Ok kt -> kt
  | Error `Msg msg ->
    Logs.err (fun m -> m "cannot decode key type %s: %s" kt msg);
    exit argument_error

module Store = Irmin_mirage_git.Mem.KV.Make(Blob)
module Sync = Irmin.Sync.Make(Store)

module Make
    (Random : Mirage_random.S)
    (Console : Mirage_console.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Public : Mirage_kv.RO)
    (Stack : Tcpip.Stack.V4V6)
    (DNS : Dns_client_mirage.S with type Transport.stack = Stack.t)
    (_ : sig end)
= struct
  module Nss = Ca_certs_nss.Make(Pclock)
  module Paf = Paf_mirage.Make(Time)(Stack.TCP)
  module LE = LE.Make(Time)(Stack)

  let ignore_error_handler _ ?request:_ _ _ = ()
  let log console fmt = Fmt.kstr (Console.log console) fmt

  let connect_store ~ctx =
    let config = Irmin_git.config "." in
    let remote, branch = match String.split_on_char '#' (Key_gen.remote ()) with
      | [ remote; branch ] -> remote, branch
      | _ -> (Key_gen.remote ()), "master" in
    Store.Repo.v config >>= fun repository -> Store.of_branch repository branch >>= fun active_branch ->
    Lwt.return (active_branch, Store.remote ~ctx remote)

  let reload _console active_branch remote key =
    Store.find active_branch key >>= function
    | Some contents -> Lwt.return (Some contents)
    | None ->
      Sync.pull active_branch remote `Set >>= function
      | Ok `Empty | Ok (`Head _) -> Store.find active_branch key
      | Error (`Msg err) -> failwith err
      | Error (`Conflict err) -> Fmt.failwith "Conflict! [%s]" err

  let show ?ln:(_ = false) ?hl console active_branch remote reqd target =
    let open Httpaf in
    let* () = log console "Want to access to: %a." Fmt.(Dump.list string) target in
    reload console active_branch remote target >>= function
    | None ->
      let contents = Fmt.str "%s Not found." (String.concat "/" target) in
      let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `Not_found in
      Reqd.respond_with_string reqd response contents ;
      log console "Response: 404 Not found for %a." Fmt.(Dump.list string) target
    | Some { Blob.contents; encrypted; } ->
      let code = Option.map Language.to_string hl in
      let html = Show.html ?code ~encrypted contents in
      let contents = Fmt.str "%a%!" (Tyxml.Html.pp ()) html in
      let headers = Headers.of_list [ "content-type", "text/html"
                                    ; "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return_unit

  let show_raw console active_branch remote reqd target =
    let open Httpaf in
    let* () = log console "Want to access to: %a (raw)." Fmt.(Dump.list string) target in
    reload console active_branch remote target >>= function
    | None ->
      let contents = Fmt.str "%s Not found." (String.concat "/" target) in
      let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `Not_found in
      Reqd.respond_with_string reqd response contents ;
      log console "Response: 404 Not found for %a." Fmt.(Dump.list string) target
    | Some { Blob.contents; _ } ->
      let headers = Headers.of_list [ "content-type", "text/plain; charset=utf-8"
                                    ; "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return_unit

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

  type response =
    { ln : bool option
    ; raw : bool option
    ; hl : Language.t option
    ; code : string }

  let json =
    let open Json_encoding in
    let language =
      string_enum (List.map (fun v -> Language.to_string v, v) Language.all) in
    conv
      (fun { ln; raw; hl; code; } -> (ln, raw, hl, code))
      (fun (ln, raw, hl, code) -> { ln; raw; hl; code; })
      (obj4
        (opt "ln" bool)
        (opt "raw" bool)
        (opt "hl" language)
        (req "code" string))

  (* Try to generate JSON as short as possible by taking advantage of the
     default values for the options (ln, hl, ...). *)
  let make_paste_json_string ?ln ?hl ?raw code =
   match Json_encoding.construct json { ln; raw; hl; code; } with
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
      match Language.of_string hl with
      | v -> Some v | exception _ -> None in
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
        Language.to_string c, Some (Language.to_string c)
      ) Language.all
    in
    let html = Form.html ~title:"Past-isserie" ~documentation:"Pasteur" languages in
    Fmt.str "%a%!" (Tyxml.Html.pp ()) html

  let index _ reqd =
    let headers = Headers.of_list [ "content-type", "text/html; charset=utf-8"
                                  ; "content-length", string_of_int (String.length index_contents) ] in
    let response = Response.create ~headers `OK in
    Reqd.respond_with_string reqd response index_contents ;
    Lwt.return ()

  let load console public reqd key =
    Public.get public key >>= fun contents -> match contents, Mirage_kv.Key.segments key with
    | Error _, _ -> assert false
    | Ok contents, ([ "highlight.pack.js" ] | [ "pasteur.js" ] as path) ->
      let headers = Headers.of_list
          [ "content-length", string_of_int (String.length contents)
          ; "content-type", "text/javascript"
          ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "%s delivered!" (String.concat "/" path)
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
      Lwt.return_unit

  let push console active_branch remote ?(author= "pasteur") key value =
    let commit0 = match Store.status active_branch with
      | `Branch branch -> Store.Branch.get (Store.repo active_branch) branch
      | `Commit commit -> Lwt.return commit
      | `Empty -> failwith "Empty repository" in
    commit0 >>= fun commit0 ->
    let _, date = Pclock.now_d_ps () in
    let info () = Store.Info.v ~author ~message:(String.concat "/" key) date in
    Store.set ~parents:[ commit0 ] ~info active_branch key value >>= function
    | Error (`Conflict c) -> Fmt.failwith "Conflict! [%s]" c
    | Error (`Test_was _) | Error (`Too_many_retries _) -> failwith "Error to update local repository!"
    | Ok () ->
      Sync.push active_branch remote >>= function
      | Ok `Empty -> failwith "Got an empty repository"
      | Ok (`Head commit1) ->
        log console "[update] commit:%a -> commit:%a." Store.Commit.pp_hash commit0 Store.Commit.pp_hash commit1
      | Error `Detached_head -> failwith "Detached head!"
      | Error (`Msg err) -> failwith err

  let is_on = (=) "on"

  let post random console active_branch remote reqd =
    match extract_content_type (Reqd.request reqd) with
    | None -> assert false (* TODO: redirect to INDEX. *)
    | Some content_type ->
      let body = Reqd.request_body reqd in
      extract_parts content_type body >>= function
      | Error _ as err -> Rresult.R.failwith_error_msg err
      | Ok posts ->
        match List.assoc Paste posts,
              List.assoc_opt Hl posts,
              List.assoc_opt Encrypted posts with
        | contents, hl, encrypted ->
          let random = random () in
          let encrypted = Option.fold ~none:false ~some:is_on encrypted in
          let hl = try Option.map Language.of_string hl with _ -> None in
          let author = List.assoc_opt User posts in
          let ln = List.exists (function (Ln, _) -> true | _ -> false) posts in
          let raw = List.exists (function (Raw, _) -> true | _ -> false) posts in
          push console active_branch remote ?author [ random ] { Blob.contents; encrypted; } >>= fun () ->
          let str = make_paste_json_string ~ln ?hl ~raw random in
          let headers = Headers.of_list [ "content-type", "application/json"
                                        ; "content-length", string_of_int (String.length str)
                                        ; "access-control-allow-origin", "*" ] in
          let response = Response.create ~headers `OK in
          Reqd.respond_with_string reqd response str ;
          Lwt.return ()
        | exception Not_found ->
          let contents = "Bad POST request." in
          let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
          let response = Response.create ~headers `Not_found in
          Reqd.respond_with_string reqd response contents ;
          Lwt.return_unit

  let main random console public active_branch remote reqd = function
    | INDEX ->
      let* () = log console "[dispatch] index" in
      index console reqd
    | GET { target; ln; hl; raw= false; } ->
      let* () = log console "[dispatch] get:%a" Fmt.(Dump.list string) target in
      show ~ln ?hl console active_branch remote reqd target
    | GET { target; raw= true; _ } ->
      let* () = log console "[dispatch] raw:%a" Fmt.(Dump.list string) target in
      show_raw console active_branch remote reqd target
    | CONTENTS key ->
      let* () = log console "[dispatch] contents:%a" Mirage_kv.Key.pp key in
      load console public reqd key
    | POST ->
      let* () = log console "[dispatch] post." in
      post random console active_branch remote reqd

  let request_handler random console public active_branch remote (_ipaddr, _port) reqd =
    let open Httpaf in
    let res () =
      Lwt.catch
        (fun () -> dispatch public reqd >>= main random console public active_branch remote reqd)
        (fun exn ->
           let res = Printexc.to_string exn in
           let* () = log console "Got an error: %s" res in
           let headers = Headers.of_list [ "connection", "close" ] in
           let response = Response.create ~headers `Internal_server_error in
           Lwt.return (Reqd.respond_with_string reqd response (Printexc.to_string exn))) in
    Lwt.async res

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

  let ( >>? ) = Lwt_result.bind

  let exit_expired = 80

  let quit_before_expire = function
    | `Single (server :: _, _) as tls ->
      let expiry = snd (X509.Certificate.validity server) in
      let diff = Ptime.diff expiry (Ptime.v (Pclock.now_d_ps ())) in
      ( match Ptime.Span.to_int_s diff with
        | None -> invalid_arg "couldn't convert span to seconds"
        | Some x when x < 0 -> invalid_arg "diff is negative"
        | Some x ->
          Lwt.async @@ fun () ->
          Time.sleep_ns Int64.(sub (Duration.of_sec x) (Duration.of_day 1)) >|= fun () ->
          exit exit_expired ) ;
      Lwt.return tls
    | tls -> Lwt.return tls

  let pull (active_branch, remote) = Sync.pull active_branch remote `Set >>= function
    | Error (`Msg err) -> failwith err
    | Error (`Conflict err) -> failwith err
    | Ok (`Empty | `Head _) -> Lwt.return (active_branch, remote)

  let provision ~production cfg stack dns =
    Paf.init ~port:80 (Stack.tcp stack) >>= fun t ->
    let service = Paf.http_service ~error_handler:ignore_error_handler (fun _ -> LE.request_handler) in
    let stop = Lwt_switch.create () in
    let `Initialized th0 = Paf.serve ~stop service t in
    let th1 =
      let gethostbyname dns domain_name = DNS.gethostbyname dns domain_name >>? fun ipv4 ->
        Lwt.return_ok (Ipaddr.V4 ipv4) in
      LE.provision_certificate
        ~production cfg
        (LE.ctx ~gethostbyname ~authenticator:(Result.get_ok (Nss.authenticator ())) dns stack) >>? fun certificates ->
      Lwt_switch.turn_off stop >>= fun () -> Lwt.return_ok certificates in
    Lwt.both th0 th1 >>= function
    | ((), Error (`Msg err)) -> failwith err
    | ((), Ok certificates) -> Lwt.return certificates

  let start _random console _time _mclock _pclock public stack dns ctx _js _hljs =
    let seed = random_bytes (Key_gen.random_length ()) in
    connect_store ~ctx >>= pull >>= fun (active_branch, remote) ->
    match Key_gen.https () with
    | false ->
      Logs.info (fun m -> m "Initialise an HTTP server (no HTTPS).") ;
      let request_handler _flow = request_handler seed console public active_branch remote in
      let port = Option.value ~default:80 (Key_gen.port ()) in
      Paf.init ~port (Stack.tcp stack) >>= fun service ->
      let http = Paf.http_service ~error_handler:ignore_error_handler request_handler in
      let (`Initialized th) = Paf.serve http service in
      th
    | true ->
      Logs.info (fun m -> m "Download TLS certificate.") ;
      provision ~production:(Key_gen.production ())
        { LE.certificate_seed= Key_gen.cert_seed ()
        ; LE.certificate_key_type= key_type (Key_gen.cert_key_type ())
        ; LE.certificate_key_bits= Some (Key_gen.cert_bits ())
        ; LE.email= Option.bind (Key_gen.email ()) (fun e -> Emile.of_string e |> Result.to_option)
        ; LE.account_seed= Key_gen.account_seed ()
        ; LE.account_key_type= key_type (Key_gen.account_key_type ())
        ; LE.account_key_bits= Some (Key_gen.account_bits ())
        ; LE.hostname= Key_gen.hostname () |> Option.get |> Domain_name.of_string_exn |> Domain_name.host_exn }
        stack dns >>= quit_before_expire >>= fun certificates ->
      Logs.info (fun m -> m "Got a TLS certificate for the server.") ;
      let tls = Tls.Config.server ~certificates () in
      let request_handler _flow = request_handler seed console public active_branch remote in
      let port = Option.value ~default:443 (Key_gen.port ()) in
      Paf.init ~port (Stack.tcp stack) >>= fun service ->
      let https = Paf.https_service ~tls ~error_handler:ignore_error_handler request_handler in
      let (`Initialized th) = Paf.serve https service in
      th
end
