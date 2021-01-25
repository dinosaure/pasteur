(* (c) Romain Calascibetta 2019 *)

open Mirage

type mimic = Mimic

let mimic = typ Mimic

let mimic_conf () =
  let packages = [ package "mimic" ] in
  impl @@ object
       inherit base_configurable
       method ty = mimic @-> mimic @-> mimic
       method module_name = "Mimic.Merge"
       method! packages = Key.pure packages
       method name = "ctx"
       method! connect _ _modname =
         function
         | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
         | [ x ] -> Fmt.str "%s.ctx" x
         | _ -> Fmt.str "Lwt.return Mimic.empty"
     end

let merge ctx0 ctx1 = mimic_conf () $ ctx0 $ ctx1

let mimic_tcp_conf () =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "tcp_ctx"
       method! connect _ modname =
         function
         | [ stack ] ->
             Fmt.str "Lwt.return (%s.with_stack %s %s.ctx)" modname stack
               modname
         | _ -> Fmt.str "Lwt.return %s.ctx" modname
     end

let mimic_tcp_impl stackv4 = mimic_tcp_conf () $ stackv4

let mimic_git_conf ~cap ~edn () =
  let packages = [ package "git-mirage" ] in
  let edn = Key.abstract edn in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic @-> mimic
       method! keys = [ edn ]
       method module_name = "Git_mirage.Make"
       method! packages = Key.pure packages
       method name = "git_ctx"
       method! connect _ modname _ =
         let with_cap = match cap with
           | `Rd -> "fetch" | `Wr -> "push" in
         Fmt.str
           "let ctx_git0 = %s.with_resolv (%s.with_smart_git_endpoint (%a) %s.ctx) in
            let ctx_git1 = %s.%s ctx_git0 in
            Lwt.return ctx_git1"
           modname modname Key.serialize_call edn modname
           modname with_cap
     end

let mimic_git_impl ~cap ~edn stackv4 mimic_tcp =
  mimic_git_conf ~cap ~edn () $ stackv4 $ mimic_tcp

let mimic_ssh_conf ~edn ~kind ~seed ~auth () =
  let seed = Key.abstract seed in
  let auth = Key.abstract auth in
  let edn = Key.abstract edn in
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4 @-> mimic @-> mimic @-> mclock @-> mimic
       method! keys = [ seed; auth; edn ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = "ssh_ctx"
       method! connect _ modname =
         function
         | [ _; tcp_ctx; git_ctx; _ ] ->
             let with_key = match kind with
               | `Rsa -> "with_rsa_key"
               | `Ed25519 -> "with_ed25519_key" in
             Fmt.str
               {| let ctx00 = Mimic.merge %s %s in
             let ctx01 = Option.fold ~none:ctx00 ~some:(fun v -> %s.%s v ctx00) %a in
             let ctx02 = Option.fold ~none:ctx01 ~some:(fun v -> %s.with_authenticator v ctx01) %a in
             let ctx03 = %s.with_resolv ctx02 in
             Lwt.return (%s.with_resolv (%s.with_smart_git_endpoint (%a) ctx03)) |}
               tcp_ctx git_ctx modname with_key Key.serialize_call seed modname
               Key.serialize_call auth modname modname modname
               Key.serialize_call edn
         | _ -> Fmt.str "Lwt.return %s.ctx" modname
     end

let mimic_ssh_impl ~edn ~kind ~seed ~auth stackv4 mimic_tcp mimic_git mclock =
  mimic_ssh_conf ~edn ~kind ~seed ~auth ()
  $ stackv4
  $ mimic_tcp
  $ mimic_git
  $ mclock

let mimic_ssh_destruct =
  impl @@ object
       inherit base_configurable
       method ty = mimic @-> mimic
       method module_name = "Git_mirage_ssh.Destruct"
       method! packages = Key.pure [ package "git-mirage" ~sublibs:[ "ssh" ] ]
       method name = "is_ssh"
       method! connect _ modname _ = Fmt.str "Lwt.return %s.is" modname
     end

(* TODO(dinosaure): user-defined nameserver and port. *)

let mimic_dns_conf ~edn () =
  let packages = [ package "git-mirage" ~sublibs:[ "dns" ] ] in
  let edn = Key.abstract edn in
  impl @@ object
       inherit base_configurable
       method ty = random @-> mclock @-> time @-> stackv4 @-> mimic @-> mimic
       method! keys = [ edn ]
       method module_name = "Git_mirage_dns.Make"
       method! packages = Key.pure packages
       method name = "dns_ctx"
       method! connect _ modname =
         function
         | [ _; _; _; _; ctx ] ->
             Fmt.str
               "Lwt.return (%s.with_resolv (%s.with_smart_git_endpoint %a %s))"
               modname modname Key.serialize_call edn ctx
         | _ -> Fmt.str "Lwt.return %s.ctx" modname
     end

let mimic_dns_impl ~edn random mclock time stackv4 mimic_tcp =
  mimic_dns_conf ~edn () $ random $ mclock $ time $ stackv4 $ mimic_tcp

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(opt string "git://127.0.0.1/pasteur" doc))

let http_port =
  let doc = Key.Arg.info ~doc:"port of HTTP service." [ "p"; "port" ] in
  Key.(create "http-port" Arg.(opt int 80 doc))

let https_port =
  let doc = Key.Arg.info ~doc:"port of HTTPS service." [ "https-port" ] in
  Key.(create "https-port" Arg.(opt (some int) None doc))

let dns_key =
  let doc = Key.Arg.info ~doc:"DNS key." [ "dns-key" ] in
  Key.(create "dns_key" Arg.(opt (some string) None doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"DNS server port." [ "dns-port" ] in
  Key.(create "dns_port" Arg.(opt int 53 doc))

let dns_addr =
  let doc = Key.Arg.info ~doc:"DNS server." [ "dns-server" ] in
  Key.(create "dns_addr" Arg.(opt (some ipv4_address) None doc))

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

let pasteur =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract remote
          ; Key.abstract http_port
          ; Key.abstract https_port
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
    (random @-> console @-> time @-> mclock @-> pclock @-> kv_ro
     @-> mimic @-> mimic
     @-> stackv4 @-> stackv4v6 @-> job)

let mimic ~cap ~edn ~kind ~seed ~auth stackv4 random mclock time =
  let mtcp = mimic_tcp_impl stackv4 in
  let mgit = mimic_git_impl ~cap ~edn stackv4 mtcp in
  let mdns = mimic_dns_impl ~edn random mclock time stackv4 mtcp in
  let mssh = mimic_ssh_impl ~edn ~kind ~seed ~auth stackv4 mtcp mgit mclock in
  merge mssh mdns, mimic_ssh_destruct $ mssh

let random = default_random
let mclock = default_monotonic_clock
let time = default_time
let stackv4 = generic_stackv4 default_network
let stack = generic_stackv4v6 default_network
let conduit = conduit_direct ~tls:true stackv4
let resolver = resolver_dns stackv4
let console = console
let public = generic_kv_ro "public"

let mimic_rd, is_ssh = mimic ~cap:`Rd ~edn:remote ~kind:`Rsa ~seed:ssh_seed ~auth:ssh_auth stackv4 random mclock time
let mimic_wr, _ = mimic ~cap:`Wr ~edn:remote ~kind:`Rsa ~seed:ssh_seed ~auth:ssh_auth stackv4 random mclock time

let packages =
  [ package "httpaf"
  ; package "uuidm"
  ; package "tyxml"
  ; package "git-mirage" ~min:"3.1.0"
  ; package "irmin-mirage-git" ~min:"2.3.0"
  ; package ~sublibs:[ "mirage" ] "dns-certify"
  ; package "multipart_form"
  ; package "paf" ~pin:"git+https://github.com/dinosaure/paf-le-chien.git#cohttp-and-letsencrypt"
  ; package "ocplib-json-typed"
  ; package "ezjsonm"
  ; package ~sublibs:[ "le" ] "paf" ]

let () =
  register "pasteur"
    ~packages
    [ pasteur $ random $ default_console $ time $ mclock $ default_posix_clock $ public
      $ mimic_rd $ mimic_wr
      $ stackv4 $ stack ]
