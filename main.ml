module Blk = struct
  type t = Mkernel.Block.t

  let pagesize = Mkernel.Block.pagesize
  let read = Mkernel.Block.atomic_read
  let write = Mkernel.Block.atomic_write
end

module RNG = Mirage_crypto_rng.Fortuna
module Fat = Mfat_bos.Make (Blk)

let ( let@ ) finally fn = Fun.protect ~finally fn
let open_error_msg = function Ok _ as v -> v | Error (`Msg _) as err -> err
let rng () = Mirage_crypto_rng_mkernel.initialize (module RNG)

let fat ~name =
  let fn blk () =
    let v = Fat.create blk in
    let v = Result.map_error (fun (`Msg msg) -> msg) v in
    Result.error_to_failure v
  in
  Mkernel.map fn [ Mkernel.block name ]

let from_documents ~mime contents =
  let fn ctx str = Digestif.SHA256.feed_string ctx str in
  let ctx = Array.fold_left fn Digestif.SHA256.empty contents in
  let hash = Digestif.SHA256.get ctx in
  let hash = Digestif.SHA256.to_hex hash in
  fun req _server _ ->
    let open Vifu.Response.Syntax in
    let* () = Vifu.Response.add ~field:"content-type" mime in
    let hdrs = Vifu.Request.headers req in
    let if_none_match =
      match Vifu.Headers.get hdrs "if-none-match" with
      | Some hash' -> String.equal hash' hash
      | None -> false
    in
    if if_none_match then
      let* () = Vifu.Response.empty in
      Vifu.Response.respond `Not_modified
    else
      let from = Flux.Source.array contents in
      let* () = Vifu.Response.add ~field:"etag" hash in
      let* () = Vifu.Response.with_source req ~compression:`DEFLATE from in
      Vifu.Response.respond `OK

type cfg = {
    max: int
  ; len: int
  ; title: string
  ; subtitle: string
  ; languages: string list
  ; auth: (string * string) option
}

module Form = struct
  type t = {
      encrypted: bool
    ; contents: string option
    ; user: string option
    ; comment: string option
    ; line: bool
    ; raw: bool
    ; language: string option
  }

  let default =
    {
      encrypted= false
    ; contents= None
    ; user= None
    ; comment= None
    ; line= false
    ; raw= false
    ; language= None
    }

  let bool =
    let parser =
      let open Angstrom in
      let+ value = string "off" <|> string "on" in
      match value with "on" -> true | "off" -> false | _ -> assert false
    in
    let open Flux.Sink.Infix in
    Flux_angstrom.parser ~size:0x10 parser <@> function
    | Ok value -> value
    | Error _ -> false

  let limit max =
    let init () = Ok (Buffer.create 0x7ff) in
    let push state str =
      match state with
      | Ok buf ->
          let len = Int.min (max - Buffer.length buf) (String.length str) in
          Buffer.add_substring buf str 0 len;
          if len >= String.length str then Ok buf else Error `Too_big_value
      | Error _ as err -> err
    in
    let full = function
      | Ok buf -> Buffer.length buf >= max
      | Error _ -> true
    in
    let stop = function
      | Ok buf -> Ok (Buffer.contents buf)
      | Error _ as err -> err
    in
    Flux.Sink { init; push; full; stop }

  let _0x7ff =
    let init () = (Bytes.create 0x7ff, 0) in
    let push (buf, off) str =
      if off < Bytes.length buf then begin
        let len = Int.min (Bytes.length buf - off) (String.length str) in
        Bytes.blit_string str 0 buf off len;
        (buf, off + len)
      end
      else (buf, off)
    in
    let full = Fun.const false in
    let stop (buf, len) =
      if len <= 0 then None else Some (Bytes.sub_string buf 0 len)
    in
    Flux.Sink { init; push; full; stop }

  let run from ?(via = Flux.Flow.identity) into =
    let value, src = Flux.Stream.run ~from ~via ~into in
    let src = Option.map Flux.Stream.from src in
    Option.iter Flux.Stream.drain src;
    value

  let part cfg (part, from) =
    let ( let* ) = Result.bind in
    match Vifu.Multipart_form.name part with
    | Some "paste" ->
        let* contents = run from (limit cfg.max) in
        Ok (fun t -> { t with contents= Some contents })
    | Some "encrypted" ->
        let v = run from bool in
        Ok (fun t -> { t with encrypted= v })
    | Some "ln" ->
        let v = run from bool in
        Ok (fun t -> { t with line= v })
    | Some "raw" ->
        let v = run from bool in
        Ok (fun t -> { t with raw= v })
    | Some "user" ->
        let v = run from _0x7ff in
        Ok (fun t -> { t with user= v })
    | Some "comment" ->
        let v = run from _0x7ff in
        Ok (fun t -> { t with comment= v })
    | Some "hl" ->
        let language = run from _0x7ff in
        let language = Option.value ~default:"" language in
        let language = List.find_opt (String.equal language) cfg.languages in
        Ok (fun t -> { t with language })
    | _ ->
        Flux.(Stream.drain (Stream.from from));
        Ok Fun.id
end

module Database = struct
  [@@@ocamlformat "disable"]

  module V = struct include Int let weight = Fun.id end
  module Lru = Lru.F.Make (Fpath) (V)

  [@@@ocamlformat "enable"]

  type t = { fat: Blk.t Mfat.t; mutable lru: Lru.t }
  type value = Encrypted of string | Clear of string

  let create fat =
    let capacity = Fat.capacity fat in
    let lru = Lru.empty capacity in
    let fn filepath lru =
      let { Mfat.size; _ } = Fat.stat fat filepath |> Result.get_ok in
      let filepath = Fpath.base filepath in
      Logs.debug (fun m -> m "[+] %a" Fpath.pp filepath);
      Lru.add filepath (Int32.to_int size) lru
    in
    let traverse = `None in
    let elements = `Files in
    let _ = Fat.fold ~traverse ~elements fat fn lru [ Fpath.v "/" ] in
    { fat; lru }

  let json =
    let open Jsont in
    let open Object in
    let contents = map Fun.id |> mem "contents" ~enc:Fun.id string |> finish in
    let encrypted = Case.map true ~dec:(fun v -> Encrypted v) contents in
    let clear = Case.map false ~dec:(fun v -> Clear v) contents in
    let enc_case = function
      | Encrypted v -> Case.value encrypted v
      | Clear v -> Case.value clear v
    in
    let cases = Case.[ make encrypted; make clear ] in
    map Fun.id
    |> case_mem "encrypted" bool ~enc:Fun.id ~enc_case cases
    |> finish

  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-"

  let gen =
    let alphabet = Array.init (String.length alphabet) (String.get alphabet) in
    fun cfg fat ->
      let buf0 = Bytes.create cfg.len in
      let buf1 = Bytes.create cfg.len in
      let rec go tries =
        if tries >= 10 then Error `Too_many_tries
        else begin
          Mirage_crypto_rng.generate_into buf0 cfg.len;
          for idx = 0 to cfg.len - 1 do
            Bytes.set buf1 idx alphabet.(Bytes.get_uint8 buf0 idx land 0b111111)
          done;
          let filepath = Fpath.v (Bytes.unsafe_to_string buf1) in
          let exists = Fat.File.exists fat filepath |> Result.get_ok in
          if exists then go (succ tries) else Ok filepath
        end
      in
      go 0

  let add t cfg form =
    let contents = Option.get form.Form.contents in
    let ( let* ) = Result.bind in
    let* filepath = gen cfg t.fat in
    let value =
      match form.Form.encrypted with
      | true -> Encrypted contents
      | false -> Clear contents
    in
    let str = Jsont_bytesrw.encode_string json value in
    let str = Result.get_ok str in
    let weight = String.length str in
    let rec trim lru =
      if Fat.available t.fat < weight then
        match Lru.lru t.lru with
        | Some (filepath, _weight) ->
            ignore (Fat.delete t.fat ~must_exist:false filepath);
            trim (Lru.remove filepath t.lru)
        | None -> lru
      else lru
    in
    let lru = trim t.lru in
    let* () = Fat.File.write t.fat filepath str in
    let lru = Lru.add filepath weight lru in
    t.lru <- lru;
    Ok filepath

  let get t uid =
    let ( let* ) = Result.bind in
    let* filepath = Fpath.of_string uid |> open_error_msg in
    let* str = Fat.File.read t.fat filepath in
    let* value =
      Jsont_bytesrw.decode_string json str
      |> Result.map_error (fun _ -> `Invalid_json)
    in
    t.lru <- Lru.promote filepath t.lru;
    Ok value

  let uid len =
    if len <= 0 then invalid_arg "Database.uid";
    let open Re in
    Tyre.regex (repn (set alphabet) 1 (Some len))
end

let json =
  let open Jsont in
  Object.map (fun code hl ln raw -> (code, hl, ln, raw))
  |> Object.mem "code" ~enc:(fun (code, _, _, _) -> code) string
  |> Object.opt_mem "hl" ~enc:(fun (_, hl, _, _) -> hl) string
  |> Object.mem "ln" ~enc:(fun (_, _, ln, _) -> ln) ~enc_omit:not bool
  |> Object.mem "raw" ~enc:(fun (_, _, _, raw) -> raw) ~enc_omit:not bool
  |> Object.finish

let authentication =
  Vifu.Middlewares.v ~name:"auth" @@ fun req _target _server cfg ->
  let hdrs = Vifu.Request.headers_of_request req in
  match (cfg.auth, Vifu.Headers.get hdrs "authorization") with
  | None, _ -> None
  | Some _, None -> Some false
  | Some (user, password), Some str0 ->
      let str1 = Fmt.str "%s:%s" user password in
      let str2 = Fmt.str "Basic %s" (Base64.encode_string str1) in
      if Eqaf.equal str0 str2 then Some true else Some false

let authentified req fn =
  let open Vifu.Response.Syntax in
  match Vifu.Request.get authentication req with
  | Some false ->
      let field = "www-authenticate" in
      let* () = Vifu.Response.add ~field "Basic realm=\"pasteur\"" in
      let* () = Vifu.Response.empty in
      Vifu.Response.respond `Unauthorized
  | None | Some true -> fn ()

let index req _server cfg =
  let open Vifu.Response.Syntax in
  authentified req @@ fun () ->
  let title = cfg.title and subtitle = cfg.subtitle in
  let languages =
    let fn str = (str, Some str) in
    ("No highlight", None) :: List.map fn cfg.languages
  in
  let tyxml = Index.html ~title ~subtitle languages in
  let* () = Vifu.Response.with_tyxml req tyxml in
  Vifu.Response.respond `OK

let add db req _server cfg =
  authentified req @@ fun () ->
  let result =
    let ( let* ) = Result.bind in
    let* stream = Vifu.Request.of_multipart_form req in
    let tasks =
      let fn part = Miou.async @@ fun () -> Form.part cfg part in
      Flux.Stream.into Flux.Sink.list (Flux.Stream.map fn stream)
    in
    let* form =
      List.fold_left
        (fun acc prm ->
          let* form = acc in
          let* update = Miou.await_exn prm in
          Ok (update form))
        (Ok Form.default) tasks
    in
    let* _ = Option.to_result ~none:`Empty form.Form.contents in
    let* filepath = Database.add db cfg form in
    Ok (filepath, form)
  in
  let open Vifu.Response.Syntax in
  match result with
  | Ok (filepath, form) ->
      let code = Fpath.to_string filepath in
      let resp = (code, form.Form.language, form.Form.line, form.Form.raw) in
      let* () = Vifu.Response.with_json req json resp in
      Vifu.Response.respond `OK
  | Error `Too_many_tries ->
      let txt = "No available slot for a new paste" in
      let* () = Vifu.Response.with_text req txt in
      Vifu.Response.respond `Internal_server_error
  | Error (`Msg _msg) ->
      let* () = Vifu.Response.with_text req "Internal server error" in
      Vifu.Response.respond `Internal_server_error
  | Error `Empty ->
      let* () = Vifu.Response.with_text req "The paste is empty" in
      Vifu.Response.respond `Bad_request
  | Error _ ->
      let* () = Vifu.Response.with_text req "Invalid request" in
      Vifu.Response.respond `Bad_request

let show db req uid _server cfg =
  authentified req @@ fun () ->
  let hl = Vifu.Queries.get req "hl" |> String.concat ""
  and ln = Vifu.Queries.exists req "ln"
  and raw = Vifu.Queries.exists req "raw" in
  let code = List.find_opt (String.equal hl) cfg.languages in
  let result = Database.get db uid in
  let open Vifu.Response.Syntax in
  match result with
  | Ok (Encrypted str) ->
      let html = Show.html ~title:cfg.title ?code ~encrypted:true ~ln str in
      let* () = Vifu.Response.with_tyxml req html in
      Vifu.Response.respond `OK
  | Ok (Clear str) when raw ->
      let* () = Vifu.Response.with_text req str in
      Vifu.Response.respond `OK
  | Ok (Clear str) ->
      let html = Show.html ~title:cfg.title ?code ~encrypted:false ~ln str in
      let* () = Vifu.Response.with_tyxml req html in
      Vifu.Response.respond `OK
  | Error _ ->
      let* () = Vifu.Response.empty in
      Vifu.Response.respond `Not_found

let devices ?gateway cidr ipv6 =
  let open Mkernel in
  let rng = Mkernel.map rng Mkernel.[]
  and net = Mnet.stack ~name:"service" ?gateway ~ipv6 cidr
  and fat = fat ~name:"pasteur" in
  [ rng; net; fat ]

let run _ (cidr, gateway, ipv6) app port =
  Mkernel.run (devices ?gateway cidr ipv6) @@ fun rng (daemon, tcp, _) fat () ->
  let@ () = fun () -> Mirage_crypto_rng_mkernel.kill rng in
  let@ () = fun () -> Mnet.kill daemon in
  let cfg = Vifu.Config.v port in
  let javascript = "text/javascript" and css = "text/css" in
  let db = Database.create fat in
  let routes =
    let open Vifu.Route in
    let open Vifu.Uri in
    let open Vifu.Type in
    let any = Vifu.Uri.any in
    [
      get (rel /?? any) --> index
    ; get (rel / "highlight.js" /?? any)
      --> from_documents ~mime:javascript Documents.highlight
    ; get (rel / "script.js" /?? any)
      --> from_documents ~mime:javascript Documents.script
    ; get (rel / "pastisserie.css" /?? any)
      --> from_documents ~mime:css Documents.pastisserie
    ; get (rel / "highlight.css" /?? any)
      --> from_documents ~mime:css Documents.style
    ; post multipart_form (rel /?? any) --> add db
    ; get (rel /% Database.uid app.len /?? any) --> show db
    ]
  in
  let middlewares =
    match app.auth with
    | Some _ -> Vifu.Middlewares.[ authentication ]
    | None -> Vifu.Middlewares.[]
  in
  Vifu.run ~middlewares ~cfg tcp routes app

open Cmdliner

let output_options = "OUTPUT OPTIONS"
let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let t0 = Mkernel.clock_monotonic ()
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let neg fn = fun x -> not (fn x)

let reporter sources ppf =
  let re = Option.map Re.compile sources in
  let print src =
    let some re = (neg List.is_empty) (Re.matches re (Logs.Src.name src)) in
    Option.fold ~none:true ~some re
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let pp header _tags k ppf fmt =
      let t1 = Mkernel.clock_monotonic () in
      let delta = Float.of_int (t1 - t0) in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f"))
        delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    match (level, print src) with
    | Logs.Debug, false -> k ()
    | _, true | _ -> msgf @@ fun ?header ?tags fmt -> pp header tags k ppf fmt
  in
  { Logs.report }

let regexp =
  let parser str =
    match Re.Pcre.re str with
    | re -> Ok (str, `Re re)
    | exception _ -> error_msgf "Invalid PCRegexp: %S" str
  in
  let pp ppf (str, _) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let sources =
  let doc = "A regexp (PCRE syntax) to identify which log we print." in
  let open Arg in
  value & opt_all regexp [ ("", `None) ] & info [ "l" ] ~doc ~docv:"REGEXP"

let setup_sources = function
  | [ (_, `None) ] -> None
  | res ->
      let res = List.map snd res in
      let res =
        List.fold_left
          (fun acc -> function `Re re -> re :: acc | _ -> acc)
          [] res
      in
      Some (Re.alt res)

let setup_sources = Term.(const setup_sources $ sources)

let setup_logs utf_8 style_renderer sources level =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Logs.set_level level;
  Logs.set_reporter (reporter sources Fmt.stdout);
  Option.is_none level

let setup_logs =
  Term.(const setup_logs $ utf_8 $ renderer $ setup_sources $ verbosity)

let port =
  let doc = "The HTTP port" in
  let open Arg in
  value & opt int 80 & info [ "p"; "port" ] ~doc ~docv:"PORT"

let bytes_of_string s =
  let s = String.trim s in
  let len = String.length s in
  let rec find_non_digit i =
    if i >= len then i
    else if s.[i] >= '0' && s.[i] <= '9' then find_non_digit (i + 1)
    else i
  in
  let idx = find_non_digit 0 in
  let number_str = String.sub s 0 idx |> String.trim in
  let unit_str = String.sub s idx (len - idx) |> String.trim in
  let ( let* ) = Option.bind in
  let* number = int_of_string_opt number_str in
  let* multiplier =
    match String.lowercase_ascii unit_str with
    | "" | "b" -> Some 1
    | "kib" -> Some 1024
    | "mib" -> Some (1024 * 1024)
    | "gib" -> Some (1024 * 1024 * 1024)
    | "tib" -> Some (1024 * 1024 * 1024 * 1024)
    | _ -> None
  in
  Some (number * multiplier)

let sizes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB" |]

let bytes_to_size = function
  | 0 -> "0b"
  | n ->
      let n = float_of_int n in
      let i = Float.floor (Float.log n /. Float.log 1024.) in
      let r = n /. Float.pow 1024. i in
      Fmt.str "%.0f%s" r sizes.(int_of_float i)

let size =
  let parser str =
    match bytes_of_string str with
    | Some n -> Ok n
    | None -> error_msgf "Invalid size: %S" str
  in
  Arg.conv (parser, Fmt.(using bytes_to_size string))

let max =
  let doc = "The maximum size of a snippet that a user can save." in
  let open Arg in
  value & opt size 65535 & info [ "m"; "max" ] ~doc ~docv:"SIZE"

let len =
  let doc = "The length of the ID when a user creates a new snippet." in
  let open Arg in
  value & opt int 4 & info [ "length" ] ~doc ~docv:"LENGTH"

let title =
  let doc = "The title of the unikernel (in the front page)." in
  let open Arg in
  value & opt string "Pasteur" & info [ "title" ] ~doc ~docv:"STRING"

let subtitle =
  let doc = "The subtitle of the unikernel (in the front page)." in
  let open Arg in
  value & opt string "past-isserie" & info [ "subtitle" ] ~doc ~docv:"STRING"

let authentication =
  let parser str =
    match String.split_on_char ':' str with
    | [] -> assert false
    | username :: password ->
        let password = String.concat ":" password in
        Ok (username, password)
  in
  let pp ppf (username, password) = Fmt.pf ppf "%s:%s" username password in
  let doc = "Secures access to the service with a password." in
  let open Arg in
  value
  & opt (some (conv (parser, pp))) None
  & info [ "auth" ] ~doc ~docv:"USERNAME:PASSWORD"

let setup_config max len title subtitle auth =
  { max; len; title; subtitle; languages= Languages.languages; auth }

let setup_config =
  let open Term in
  const setup_config $ max $ len $ title $ subtitle $ authentication

let term =
  let open Term in
  const run $ setup_logs $ Mnet_cli.setup $ setup_config $ port

let cmd =
  let info = Cmd.info "pasteur" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
