[@@@warning "-40-45"]

open Lwt.Infix
open Pasteur
open Httpaf

let ( <.> ) f g = fun x -> f (g x)

let failwith fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt

let failwith_error_msg = function
  | Error (`Msg err) -> Lwt.fail (Failure err)
  | Ok v -> Lwt.return v

module Option = struct
  let fold ~none ~some = function
    | Some x -> some x
    | None -> none

  let value x ~default = match x with
    | Some x -> x | None -> default

  include Option
end

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

module Make
    (Random : Mirage_random.S)
    (_ : Mirage_console.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Local : Mirage_kv.RO)
    (_ : sig end)
    (Stack : Mirage_stack.V4V6)
= struct
  module Paf = Paf_mirage.Make(Time)(Stack)
  module HTTP_Letsencrypt = LE.Make(Time)
  module DNS_Letsencrypt = DLE.Make(Random)(Pclock)(Time)(Stack)
  module Resolver = Dns_client_mirage.Make(Random)(Time)(Mclock)(Stack)

  let error_handler _ ?request:_ _ _ = ()

  type provision =
    | DNS of DNS_Letsencrypt.configuration
    | HTTP of HTTP_Letsencrypt.configuration

  let tcp_connect scheme stack ipaddr port =
    match scheme with
    | `HTTP -> Lwt.return_some (stack, ipaddr, port)
    | _ -> Lwt.return_none

  let tls_connect scheme domain_name cfg stack ipaddr port =
    Logs.debug (fun m -> m "Start a TLS connection with %a:%d [%a]."
      Fmt.(Dump.option Domain_name.pp) domain_name port Ipaddr.pp ipaddr) ;
    match scheme with
    | `HTTPS -> Lwt.return_some (domain_name, cfg, stack, ipaddr, port)
    | _ ->
      Logs.warn (fun m -> m "Avoid the TLS connection with %a:%d."
        Ipaddr.pp ipaddr port) ;
      Lwt.return_none

  let dns_resolver_v4 dns domain_name =
    Resolver.gethostbyname dns domain_name >>= function
    | Ok ipv4 -> Lwt.return_some (Ipaddr.V4 ipv4)
    | _ -> Lwt.return_none

  let dns_resolver_v6 dns domain_name =
    Resolver.gethostbyname6 dns domain_name >>= function
    | Ok ipv6 -> Lwt.return_some (Ipaddr.V6 ipv6)
    | _ -> Lwt.return_none

  let null =
    let authenticator ~host:_ _ = Ok None in
    Tls.Config.client ~authenticator ()

  let provision ?production v = function
    | DNS cfg  -> DNS_Letsencrypt.provision_certificate v cfg
    | HTTP cfg ->
      let dns   = Mimic.make ~name:"dns" in
      let stack = Mimic.make ~name:"stack" in
      let tls   = Mimic.make ~name:"tls" in

      let ctx =
        let open Paf_cohttp in
        Mimic.empty
        |> Mimic.(fold Paf.tcp_edn Fun.[ req scheme; req stack; req ipaddr; dft port 80; ] ~k:tcp_connect)
        |> Mimic.(fold Paf.tls_edn Fun.[ req scheme; opt domain_name; dft tls null; req stack; req ipaddr; dft port 443; ]
                    ~k:tls_connect)
        (* |> Mimic.(fold ipaddr Fun.[ req dns; req domain_name ] ~k:dns_resolver_v6) *)
        |> Mimic.(fold ipaddr Fun.[ req dns; req domain_name ] ~k:dns_resolver_v4)
        |> Mimic.add dns (Resolver.create v)
        |> Mimic.add stack v in
      Paf.init ~port:80 v >>= fun tcp_service ->
      Lwt_switch.with_switch @@ fun stop ->
      let http_service = Paf.http_service ~error_handler HTTP_Letsencrypt.request_handler in
      let `Initialized th = Paf.serve ~stop http_service tcp_service in
      let fiber =
        HTTP_Letsencrypt.provision_certificate ?production cfg ctx >>= fun res ->
        Logs.info (fun m -> m "Got a TLS certificate and stop the let's encrypt server.") ;
        Lwt_switch.turn_off stop >>= fun () -> Lwt.return res in
      Lwt.both th fiber >>= fun ((), tls) ->
      Logs.info (fun m -> m "Let's encrypt server terminated.") ;
      match tls with
      | Ok (`Single (certchain, `RSA key)) ->
        Lwt.return_ok (`Single (certchain, `RSA key))
      | Error _ as err -> Lwt.return err

  module Store = Irmin_mirage_git.Mem.KV(Blob)
  module Sync = Irmin.Sync(Store)

  let connect_store ~ctx =
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >|= fun repository ->
    repository, Store.remote ~ctx (Key_gen.remote ())

  let load repository remote key =
    Store.find repository key >>= function
    | Some contents -> Lwt.return (Some contents)
    | None ->
      Logs.debug (fun m -> m "Resynchronize the unikernel with the remote repository.") ;
      Sync.pull repository remote `Set >>= function
      | Ok `Empty | Ok (`Head _) ->
        Logs.debug (fun m -> m "Synchronization is done.") ;
        Store.find repository key
      | Error (`Msg err) -> failwith "%s" err
      | Error (`Conflict err) -> failwith "Conflict! [%s]" err

  let show ?ln:(_ = false) ?hl store remote reqd target =
    let open Httpaf in
    Logs.debug (fun m -> m "Want to access to: %a." Fmt.(Dump.list string) target) ;
    load store remote target >>= function
    | None ->
      let contents = Fmt.strf "%s Not found." (String.concat "/" target) in
      let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `Not_found in
      Reqd.respond_with_string reqd response contents ;
      Logs.debug (fun m -> m "Response: 404 Not found for %a." Fmt.(Dump.list string) target) ;
      Lwt.return_unit
    | Some { Blob.contents; encrypted; } ->
      let html = Show.html ?code:(Option.map Language.value_of_language hl) ~encrypted contents in
      let contents = Fmt.strf "%a%!" (Tyxml.Html.pp ()) html in
      let headers = Headers.of_list [ "content-type", "text/html"
                                    ; "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return ()

  let show_raw store remote reqd target =
    let open Httpaf in
    Logs.debug (fun m -> m "Want to access to: %a (raw)." Fmt.(Dump.list string) target) ;
    load store remote target >>= function
    | None ->
      let contents = Fmt.strf "%s Not found." (String.concat "/" target) in
      let headers = Headers.of_list [ "content-length", string_of_int (String.length contents) ] in
      let response = Response.create ~headers `Not_found in
      Reqd.respond_with_string reqd response contents ;
      Logs.debug (fun m -> m "Response 404 Not found for %a (raw)." Fmt.(Dump.list string) target) ;
      Lwt.return_unit
    | Some { Blob.contents; _ } ->
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
    | CONTENTS of Local.key
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
    conv
      (fun { ln; raw; hl; code; } -> (ln, raw, hl, code))
      (fun (ln, raw, hl, code) -> { ln; raw; hl; code; })
      (obj4
        (opt "ln" bool)
        (opt "raw" bool)
        (opt "hl" Language.json)
        (req "code" string))

  (* Try to generate JSON as short as possible by taking advantage of the
     default values for the options (ln, hl, ...). *)
  let make_paste_json_string ?ln ?hl ?raw code =
   match Json_encoding.construct json { ln; raw; hl; code; } with
   | #Ezjsonm.t as v -> Ezjsonm.to_string v
   | _ -> assert false (** TODO(dinosaure): check that! *)

  let decode_paste_uri ~target ~queries =
    let query_bool ~default key =
      match List.assoc_opt key queries with
      | Some [ "true" ] | Some [] -> true
      | Some [ "false" ] -> false
      | _ -> default in
    let ln = query_bool ~default:ln_default "ln" in
    let hl = let open Option in
      List.assoc_opt "hl" queries >>= function
      | [] -> None | hd :: _ ->
        Language.language_of_value_opt hd in
    let raw = query_bool ~default:raw_default "raw" in
    GET { target; ln; hl; raw; }

  let dispatch local reqd =
    let request = Reqd.request reqd in
    let target = Uri.of_string request.Request.target in
    let queries = Uri.query target in
    let target = Astring.String.trim ~drop:(Char.equal '/') (Uri.path target) in
    let target = Astring.String.cuts ~sep: "/" target in

    let key = Mirage_kv.Key.v request.Request.target in
    Local.exists local key >>= function
    | Error err -> Fmt.invalid_arg "%a" Local.pp_error err
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
      List.map (fun c -> Language.string_of_language c, Some (Language.value_of_language c))
        Language.all
    in
    let html = Form.html ~title:"Past-isserie" ~documentation:"Pasteur" languages in
    Fmt.strf "%a%!" (Tyxml.Html.pp ()) html

  let index reqd =
    let headers = Headers.of_list [ "content-type", "text/html; charset=utf-8"
                                  ; "content-length", string_of_int (String.length index_contents) ] in
    let response = Response.create ~headers `OK in
    Reqd.respond_with_string reqd response index_contents ;
    Lwt.return ()

  let load local reqd key =
    Local.get local key >>= fun contents -> match contents, Mirage_kv.Key.segments key with
    | Error _, _ -> assert false
    | Ok contents, ([ "highlight.pack.js" ] | [ "pasteur.js" ]) ->
      Local.digest local key >>= fun hash ->
      let etag = Digestif.SHA1.of_raw_string (Rresult.R.get_ok hash) |> Digestif.SHA1.to_hex in 
      let headers = Headers.of_list
          [ "content-length", string_of_int (String.length contents)
          ; "etag", etag
          ; "content-type", "text/javascript"
          ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return_unit
    | Ok contents, [ "pastisserie.css" ] ->
      Local.digest local key >>= fun hash ->
      let etag = Digestif.SHA1.of_raw_string (Rresult.R.get_ok hash) |> Digestif.SHA1.to_hex in 
      let headers = Headers.of_list
          [ "content-length", string_of_int (String.length contents)
          ; "etag", etag
          ; "content-type", "text/css"
          ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return_unit
    | Ok contents, _ ->
      Local.digest local key >>= fun hash ->
      let etag = Digestif.SHA1.of_raw_string (Rresult.R.get_ok hash) |> Digestif.SHA1.to_hex in 
      let headers = Headers.of_list
          [ "content-length", string_of_int (String.length contents)
          ; "etag", etag
          ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return_unit

  let push store remote ?(author= "pasteur") key value =
    let commit0 = match Store.status store with
      | `Branch branch ->
        Store.Branch.get (Store.repo store) branch
      | `Commit commit -> Lwt.return commit
      | `Empty -> failwith "Empty repository" in
    commit0 >>= fun commit0 ->
    let _, date = Pclock.now_d_ps () in
    let info () = Irmin.Info.v ~date ~author (String.concat "/" key) in
    Store.set ~parents:[ commit0 ] ~info store key value >>= function
    | Error (`Conflict c) -> failwith "Conflict! [%s]" c
    | Error (`Test_was _) | Error (`Too_many_retries _) -> failwith "Error to update local repository!"
    | Ok () ->
      Sync.push store remote >>= function
      | Ok `Empty -> failwith "Got an empty repository"
      | Ok (`Head commit1) ->
        Logs.debug (fun m -> m "commit:%a -> commit:%a." Store.Commit.pp_hash commit0 Store.Commit.pp_hash commit1) ;
        Lwt.return_unit
      | Error `Detached_head -> failwith "Detached head!"
      | Error (`Msg err) -> Lwt.fail (Failure err)

  let is_on = (=) "on"

  let post random store remote reqd =
    match extract_content_type (Reqd.request reqd) with
    | None -> assert false (* TODO: redirect to INDEX. *)
    | Some content_type ->
      Logs.debug (fun m -> m "Start to extract multipart/form-data.") ;
      let body = Reqd.request_body reqd in
      extract_parts content_type body >>= function
      | Error _ as err -> failwith_error_msg err
      | Ok posts ->
        match List.assoc Paste posts,
              List.assoc_opt Hl posts,
              List.assoc_opt Encrypted posts with
        | contents, hl, encrypted ->
          let random = random () in
          let encrypted = Option.fold ~none:false ~some:is_on encrypted in
          let hl = Option.fold ~none:None ~some:Language.language_of_value_opt hl in
          let author = List.assoc_opt User posts in
          let ln = List.exists (function (Ln, _) -> true | _ -> false) posts in
          let raw = List.exists (function (Raw, _) -> true | _ -> false) posts in
          push store remote ?author [ random ] { Blob.contents; encrypted; } >>= fun () ->
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
          Lwt.return ()

  let cache local reqd key =
    let request = Reqd.request reqd in
    match Headers.get request.Request.headers "If-None-Match" with
    | None -> load local reqd key
    | Some hash ->
      Lwt.catch
        (fun () ->
           let hash = Digestif.SHA1.of_hex hash in
           Local.digest local key >>= function
           | Ok hash' when Digestif.SHA1.equal (Digestif.SHA1.of_raw_string hash') hash ->
             let response = Response.create `Not_modified in
             Reqd.respond_with_string reqd response "" ;
             Lwt.return ()
           | _ -> load local reqd key)
        @@ fun _exn -> load local reqd key

  let main random local store rd_remote wr_remote reqd = function
    | INDEX -> index reqd
    | GET { target; ln; hl; raw= false; } -> show ~ln ?hl store rd_remote reqd target
    | GET { target; raw= true; _ } -> show_raw store rd_remote reqd target
    | CONTENTS key -> cache local reqd key
    | POST -> post random store wr_remote reqd

  let request_handler random local store rd_remote wr_remote (_ipaddr, _port) reqd =
    let open Httpaf in
    let res () =
      Lwt.catch
        (fun () -> dispatch local reqd >>= main random local store rd_remote wr_remote reqd)
        (fun exn ->
           let res = Printexc.to_string exn in
           Logs.err (fun m -> m "Got an error: %s." res) ;
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
    | Ok (`Single (server :: _, _) as tls) ->
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
    | Ok tls -> Lwt.return tls
    | Error (`Msg err) -> failwith "TLS: %s" err

  let cfg () =
    match Key_gen.https (),
          Key_gen.dns_key (), Key_gen.dns_port (), Key_gen.dns_addr (), Key_gen.cert_seed (),
          Key_gen.email (), Key_gen.account_seed (), Key_gen.hostname () with
    | true, Some dns_key, dns_port, Some dns_addr, cert_seed,
      _, _, tls_hostname ->
      Logs.info (fun m -> m "Ready to get the TLS certificate from the DNS service.") ;
      let hostname = Rresult.(R.error_msg_to_invalid_arg Domain_name.(of_string tls_hostname >>= host)) in
      Some (DNS { key= dns_key; port= dns_port; addr= dns_addr; seed= cert_seed; hostname; })
    | true, _, _, _, cert_seed, email, account_seed, tls_hostname ->
      Logs.info (fun m -> m "Ready to get the TLS certificate from a local HTTP service.") ;
      let hostname = Rresult.(R.error_msg_to_invalid_arg Domain_name.(of_string tls_hostname >>= host)) in
      let email = Option.bind email (Rresult.R.to_option <.> Emile.of_string) in
      Some (HTTP { email; seed= account_seed; certificate_seed= cert_seed; hostname; })
    | _ -> None

  let pull (store, rd_remote) = Sync.pull store rd_remote `Set >>= function
    | Error (`Msg err) -> Lwt.fail (Failure err)
    | Error (`Conflict err) -> Lwt.fail (Failure err)
    | Ok (`Empty | `Head _) -> Lwt.return (store, rd_remote)

  let ( let* ) = Lwt.bind

  let start _random _console _time _mclock _pclock local ctx stack =
    let seed = random_bytes (Key_gen.random_length ()) in
    let* store_rd, rd_remote = connect_store ~ctx in
    let* store_rd, rd_remote = pull (store_rd, rd_remote) in
    let  wr_remote = Store.remote ~ctx (Key_gen.remote ()) in
    let  request_handler = request_handler seed local store_rd rd_remote wr_remote in
    let* service, port = match cfg () with
      | None ->
        Logs.info (fun m -> m "Initialise an HTTP server (no HTTPS).") ;
        let port = Option.value ~default:80 (Key_gen.port ()) in
        Lwt.return (Paf.http_service ~error_handler request_handler, port)
      | Some cfg ->
        Logs.info (fun m -> m "Download TLS certificate.") ;
        let* tls = provision ~production:(Key_gen.production ()) stack cfg in
        Logs.info (fun m -> m "Got a TLS certificate for the server.") ;
        let* tls = quit_before_expire tls in
        let  tls = Tls.Config.server ~certificates:tls () in
        let port = Option.value ~default:443 (Key_gen.port ()) in
        Lwt.return (Paf.https_service ~tls ~error_handler request_handler, port) in
    let* tcp_service = Paf.init ~port stack in
    let `Initialized th = Paf.serve service tcp_service in th
end
