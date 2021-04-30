(* (c) Romain Calascibetta 2019 *)

open Mirage

(* [mimic] stuffs. *)

type mimic = Mimic

let mimic = typ Mimic

let mimic_count =
  let v = ref (-1) in
  fun () -> incr v ; !v

let mimic_conf () =
  let packages = [ package "mimic" ] in
  impl @@ object
       inherit base_configurable
       method ty = mimic @-> mimic @-> mimic
       method module_name = "Mimic.Merge"
       method! packages = Key.pure packages
       method name = Fmt.str "merge_ctx%02d" (mimic_count ())
       method! connect _ _modname =
         function
         | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
         | [ x ] -> Fmt.str "%s.ctx" x
         | _ -> Fmt.str "Lwt.return Mimic.empty"
     end

let merge ctx0 ctx1 = mimic_conf () $ ctx0 $ ctx1

(* [tcp] connector. *)

let mimic_tcp_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "tcp_ctx"
       method! connect _ modname = function
         | [ stack ] ->
           Fmt.str {ocaml|Lwt.return (%s.with_stack %s %s.ctx)|ocaml}
             modname stack modname
         | _ -> assert false
     end

let mimic_tcp_impl stackv4v6 = mimic_tcp_conf $ stackv4v6

(* [ssh] connector. *)

let mimic_ssh_conf ~kind ~seed ~auth =
  let seed = Key.abstract seed in
  let auth = Key.abstract auth in
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> mimic @-> mclock @-> mimic
       method! keys = [ seed; auth; ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = match kind with
         | `Rsa -> "ssh_rsa_ctx"
         | `Ed25519 -> "ssh_ed25519_ctx"
       method! connect _ modname =
         function
         | [ _; tcp_ctx; _ ] ->
             let with_key =
               match kind with
               | `Rsa -> "with_rsa_key"
               | `Ed25519 -> "with_ed25519_key"
             in
             Fmt.str
               {ocaml|let ssh_ctx00 = Mimic.merge %s %s.ctx in
                      let ssh_ctx01 = Option.fold ~none:ssh_ctx00 ~some:(fun v -> %s.%s v ssh_ctx00) %a in
                      let ssh_ctx02 = Option.fold ~none:ssh_ctx01 ~some:(fun v -> %s.with_authenticator v ssh_ctx01) %a in
                      Lwt.return ssh_ctx02|ocaml}
               tcp_ctx modname
               modname with_key Key.serialize_call seed
               modname Key.serialize_call auth
         | _ -> assert false
     end

let mimic_ssh_impl ~kind ~seed ~auth stackv4v6 mimic_git mclock =
  mimic_ssh_conf ~kind ~seed ~auth $ stackv4v6 $ mimic_git $ mclock

(* [dns] connector. *)

let mimic_dns_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "dns" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = random @-> mclock @-> time @-> stackv4v6 @-> mimic @-> mimic
       method module_name = "Git_mirage_dns.Make"
       method! packages = Key.pure packages
       method name = "dns_ctx"
       method! connect _ modname =
         function
         | [ _; _; _; stack; tcp_ctx ] ->
             Fmt.str
               {ocaml|let dns_ctx00 = Mimic.merge %s %s.ctx in
                      let dns_ctx01 = %s.with_dns %s dns_ctx00 in
                      Lwt.return dns_ctx01|ocaml}
               tcp_ctx modname
               modname stack
         | _ -> assert false
     end

let mimic_dns_impl random mclock time stackv4v6 mimic_tcp =
  mimic_dns_conf $ random $ mclock $ time $ stackv4v6 $ mimic_tcp

(* [docteur] file-system. *)

let docteur_solo5 ~name directory () =
  impl @@ object
       inherit base_configurable
       method ty = kv_ro
       method name = Fmt.str "docteur-%s" name
       method module_name = Fmt.str "Docteur_solo5.Fast"
       method! keys = [ Key.abstract directory ]
       method! packages = Key.pure [ package "docteur" ~sublibs:[ "solo5" ] ]
       method! configure _info =
         Hashtbl.add Mirage_impl_block.all_blocks name
           { Mirage_impl_block.filename= name; number= 0 } ;
         Ok ()
       method! build info =
         let ctx = Info.context info in
         let directory = match Key.get ctx directory with
           | Some path -> Fpath.v path
           | None -> Fpath.(Rresult.R.get_ok (Bos.OS.Dir.current ()) / "public") in
         Bos.OS.Cmd.run Bos.Cmd.(v "docteur.make" % Fmt.str "file://%a/" Fpath.pp directory % Fmt.str "%s.img" name)
       method! connect _ modname _ =
         Fmt.str
           {ocaml|let ( <.> ) f g = fun x -> f (g x) in
                  let f = Rresult.R.(failwith_error_msg <.> reword_error (msgf "%%a" %s.pp_error)) in
                  Lwt.map f (%s.connect %S)|ocaml}
           modname modname name
     end

let docteur_unix ~name directory () =
  impl @@ object
       inherit base_configurable
       method ty = kv_ro
       method name = Fmt.str "docteur-%s" name
       method module_name = Fmt.str "Docteur_unix.Fast"
       method! keys = [ Key.abstract directory ]
       method! packages = Key.pure [ package "docteur" ~sublibs:[ "unix" ] ]
       method! configure _info =
         Hashtbl.add Mirage_impl_block.all_blocks name
           { Mirage_impl_block.filename= name; number= 0 } ;
         Ok ()
       method! build info =
         let ctx = Info.context info in
         let directory = match Key.get ctx directory with
           | Some path -> Fpath.v path
           | None -> Fpath.(Rresult.R.get_ok (Bos.OS.Dir.current ()) / "public") in
         Bos.OS.Cmd.run Bos.Cmd.(v "docteur.make" % Fmt.str "file://%a/" Fpath.pp directory % Fmt.str "%s.img" name)
       method! connect _ modname _ =
         Fmt.str
           {ocaml|let ( <.> ) f g = fun x -> f (g x) in
                  let f = Rresult.R.(failwith_error_msg <.> reword_error (msgf "%%a" %s.pp_error)) in
                  Lwt.map f (%s.connect %S)|ocaml}
           modname modname (name ^ ".img")
     end

let docteur ~name directory =
  let choose = function
    | #Key.mode_unix -> `Unix
    | #Key.mode_solo5 -> `Solo5
    | _ -> failwith "Xen is not supported" in
  let v = Key.(pure choose $ (value target)) in
  match_impl v
    [ (`Solo5, docteur_solo5 ~name directory ())
    ; (`Unix,  docteur_unix  ~name directory ()) ]
    ~default:(docteur_unix ~name directory ())

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(opt string "git://127.0.0.1/pasteur" doc))

let port =
  let doc = Key.Arg.info ~doc:"port of HTTP service." [ "p"; "port" ] in
  Key.(create "port" Arg.(opt (some int) None doc))

let dns_key =
  let doc = Key.Arg.info ~doc:"DNS key." [ "dns-key" ] in
  Key.(create "dns_key" Arg.(opt (some string) None doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"DNS server port." [ "dns-port" ] in
  Key.(create "dns_port" Arg.(opt int 53 doc))

let dns_addr =
  let doc = Key.Arg.info ~doc:"DNS server." [ "dns-server" ] in
  Key.(create "dns_addr" Arg.(opt (some ip_address) None doc))

let email =
  let doc = Key.Arg.info ~doc:"Let's encrypt email." [ "email" ] in
  Key.(create "email" Arg.(opt (some string) None doc))

let hostname =
  let doc = Key.Arg.info ~doc:"Hostname of the unikernel." [ "hostname" ] in
  Key.(create "hostname" Arg.(required string doc))

let ssh_seed =
  let doc = Key.Arg.info ~doc:"Seed of the private SSH key." [ "ssh-seed" ] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_auth =
  let doc = Key.Arg.info ~doc:"SSH Public key of the remote Git endpoint." [ "ssh-auth" ] in
  Key.(create "ssh_auth" Arg.(opt (some string) None doc))

let random_len =
  let doc = Key.Arg.info ~doc:"Length of generated URI." [ "length" ] in
  Key.(create "random_length" Arg.(opt int 3 doc))

let cert_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt certificate seed." [ "cert-seed" ] in
  Key.(create "cert_seed" Arg.(opt (some string) None doc))

let account_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt account seed." [ "account-seed" ] in
  Key.(create "account_seed" Arg.(opt (some string) None doc))

let production =
  let doc = Key.Arg.info ~doc:"Let's encrypt production environment." [ "production" ] in
  Key.(create "production" Arg.(opt bool false doc))

let https =
  let doc = Key.Arg.info ~doc:"Start an HTTP server with a TLS certificate." [ "https" ] in
  Key.(create "https" Arg.(opt bool false doc))

let local =
  let doc = Key.Arg.info ~doc:"Local directory which contains *.js and *.css files." [ "local" ] in
  Key.(create "local" Arg.(required ~stage:`Configure string doc))

let pasteur =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract remote
          ; Key.abstract port
          ; Key.abstract https
          ; Key.abstract dns_key
          ; Key.abstract dns_addr
          ; Key.abstract dns_port
          ; Key.abstract ssh_seed
          ; Key.abstract ssh_auth
          ; Key.abstract email
          ; Key.abstract hostname
          ; Key.abstract random_len
          ; Key.abstract cert_seed
          ; Key.abstract account_seed
          ; Key.abstract production ]
    (random @-> console @-> time @-> mclock @-> pclock @-> kv_ro @-> mimic @-> stackv4v6 @-> job)

let mimic ~kind ~seed ~auth stackv4v6 random mclock time =
  let mtcp = mimic_tcp_impl stackv4v6 in
  let mdns = mimic_dns_impl random mclock time stackv4v6 mtcp in
  let mssh = mimic_ssh_impl ~kind ~seed ~auth stackv4v6 mtcp mclock in
  merge mssh mdns

let random = default_random
let mclock = default_monotonic_clock
let pclock = default_posix_clock
let time = default_time
let stack = generic_stackv4v6 default_network
let resolver = resolver_dns stack
let console = console

let mimic = mimic ~kind:`Rsa ~seed:ssh_seed ~auth:ssh_auth stack random mclock time

let packages =
  [ package "uuidm"
  ; package "tyxml"
  ; package "irmin-mirage-git" ~min:"2.5.3"
  ; package ~sublibs:[ "mirage" ] "dns-certify"
  ; package "multipart_form" ~sublibs:[ "lwt" ] ~min:"0.2.0"
  ; package "paf" ~min:"0.0.3"
  ; package "ocplib-json-typed"
  ; package "ezjsonm"
  ; package ~sublibs:[ "le" ] "paf" ]

let () =
  register "pasteur"
    ~packages
    [ pasteur $ random $ default_console $ time $ mclock $ pclock $ docteur ~name:"public" local $ mimic $ stack ]
