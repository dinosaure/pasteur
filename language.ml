type t =
 [ `ABNF
 | `ARM_assembler
 | `Apache
 | `Awk
 | `Bash
 | `BNF
 | `C_Sharp
 | `Cpp
 | `Coq
 | `CSS
 | `Cap_n_Proto
 | `Clojure
 | `CoffeeScript
 | `DNS_Zone_file
 | `Diff
 | `Dockerfile
 | `EBNF
 | `Elm
 | `Erlang
 | `F_Sharp
 | `Go
 | `HTML
 | `HTTP
 | `Haskell
 | `Haxe
 | `Ini
 | `JSON
 | `Java
 | `JavaScript
 | `Makefile
 | `Markdown
 | `Nginx
 | `Nix
 | `OCaml
 | `Objective_C
 | `PHP
 | `Perl
 | `Prolog
 | `Python
 | `Ruby
 | `Rust
 | `ReasonML
 | `SQL
 | `Scala
 | `Scheme
 | `Shell
 | `Swift
 | `TeX
 | `VHDL
 | `Verilog
 | `x86 ]

let all =
 [ `ABNF
 ; `ARM_assembler
 ; `Apache
 ; `Awk
 ; `Bash
 ; `BNF
 ; `C_Sharp
 ; `Cpp
 ; `Coq
 ; `CSS
 ; `Cap_n_Proto
 ; `Clojure
 ; `CoffeeScript
 ; `DNS_Zone_file
 ; `Diff
 ; `Dockerfile
 ; `EBNF
 ; `Elm
 ; `Erlang
 ; `F_Sharp
 ; `Go
 ; `HTML
 ; `HTTP
 ; `Haskell
 ; `Haxe
 ; `Ini
 ; `JSON
 ; `Java
 ; `JavaScript
 ; `Makefile
 ; `Markdown
 ; `Nginx
 ; `Nix
 ; `OCaml
 ; `Objective_C
 ; `PHP
 ; `Perl
 ; `Prolog
 ; `Python
 ; `Ruby
 ; `Rust
 ; `ReasonML
 ; `SQL
 ; `Scala
 ; `Scheme
 ; `Shell
 ; `Swift
 ; `TeX
 ; `VHDL
 ; `Verilog
 ; `x86 ]

let string_of_language = function
  | `ABNF          -> "ABNF"
  | `ARM_assembler -> "ARM assembler"
  | `Apache        -> "Apache"
  | `Awk           -> "Awk"
  | `Bash          -> "Bash"
  | `BNF           -> "BNF"
  | `C_Sharp       -> "C#"
  | `Cpp           -> "Cpp"
  | `Coq           -> "Coq"
  | `CSS           -> "CSS"
  | `Cap_n_Proto   -> "Cap’n Proto"
  | `Clojure       -> "Clojure"
  | `CoffeeScript  -> "CoffeeScript"
  | `DNS_Zone_file -> "DNS Zone file"
  | `Diff          -> "Diff"
  | `Dockerfile    -> "Dockerfile"
  | `EBNF          -> "EBNF"
  | `Elm           -> "Elm"
  | `Erlang        -> "Erlang"
  | `F_Sharp       -> "F#"
  | `Go            -> "Go"
  | `HTML          -> "HTML"
  | `HTTP          -> "HTTP"
  | `Haskell       -> "Haskell"
  | `Haxe          -> "Haxe"
  | `Ini           -> "Ini"
  | `JSON          -> "JSON"
  | `Java          -> "Java"
  | `JavaScript    -> "JavaScript"
  | `Makefile      -> "Makefile"
  | `Markdown      -> "Markdown"
  | `Nginx         -> "Nginx"
  | `Nix           -> "Nix"
  | `OCaml         -> "OCaml"
  | `Objective_C   -> "Objective C"
  | `PHP           -> "PHP"
  | `Perl          -> "Perl"
  | `Prolog        -> "Prolog"
  | `Python        -> "Python"
  | `Ruby          -> "Ruby"
  | `Rust          -> "Rust"
  | `ReasonML      -> "ReasonML"
  | `SQL           -> "SQL"
  | `Scala         -> "Scala"
  | `Scheme        -> "Scheme"
  | `Shell         -> "Shell"
  | `Swift         -> "Swift"
  | `TeX           -> "TeX"
  | `VHDL          -> "VHDL"
  | `Verilog       -> "Verilog"
  | `x86           -> "x86"

let language_of_value_exn = function
  | "abnf" -> `ABNF
  | "arm" -> `ARM_assembler
  | "apache" -> `Apache
  | "awk" -> `Awk
  | "bash" -> `Bash
  | "bnf" -> `BNF
  | "cs" -> `C_Sharp
  | "cpp" -> `Cpp
  | "coq" -> `Coq
  | "css" -> `CSS
  | "capnproto" -> `Cap_n_Proto
  | "clojure" -> `Clojure
  | "coffeescript" -> `CoffeeScript
  | "dns" -> `DNS_Zone_file
  | "diff" -> `Diff
  | "dockerfile" -> `Dockerfile
  | "ebnf" -> `EBNF
  | "elm" -> `Elm
  | "erlang" -> `Erlang
  | "fsharp" -> `F_Sharp
  | "go" -> `Go
  | "html" -> `HTML
  | "http" -> `HTTP
  | "haskell" -> `Haskell
  | "haxe" -> `Haxe
  | "ini" -> `Ini
  | "json" -> `JSON
  | "java" -> `Java
  | "javascript" -> `JavaScript
  | "makefile" -> `Makefile
  | "markdown" -> `Markdown
  | "nginx" -> `Nginx
  | "nix" -> `Nix
  | "ocaml" -> `OCaml
  | "objectivec" -> `Objective_C
  | "php" -> `PHP
  | "perl" -> `Perl
  | "prolog" -> `Prolog
  | "python" -> `Python
  | "ruby" -> `Ruby
  | "rust" -> `Rust
  | "reasonml" -> `ReasonML
  | "sql" -> `SQL
  | "scala" -> `Scala
  | "scheme" -> `Scheme
  | "shell" -> `Shell
  | "swift" -> `Swift
  | "tex" -> `TeX
  | "vhdl" -> `VHDL
  | "verilog" -> `Verilog
  | "x86asm" -> `x86
  | v -> Fmt.invalid_arg "invalid language: %s" v

let language_of_value_opt x =
  try Some (language_of_value_exn x) with _ -> None

let value_of_language = function
  | `ABNF -> "abnf"
  | `ARM_assembler -> "arm"
  | `Apache -> "apache"
  | `Awk -> "awk"
  | `Bash -> "bash"
  | `BNF -> "bnf"
  | `C_Sharp -> "cs"
  | `Cpp -> "cpp"
  | `Coq -> "coq"
  | `CSS -> "css"
  | `Cap_n_Proto -> "capnproto"
  | `Clojure -> "clojure"
  | `CoffeeScript -> "coffeescript"
  | `DNS_Zone_file -> "dns"
  | `Diff -> "diff"
  | `Dockerfile -> "dockerfile"
  | `EBNF -> "ebnf"
  | `Elm -> "elm"
  | `Erlang -> "erlang"
  | `F_Sharp -> "fsharp"
  | `Go -> "go"
  | `HTML -> "html"
  | `HTTP -> "http"
  | `Haskell -> "haskell"
  | `Haxe -> "haxe"
  | `Ini -> "ini"
  | `JSON -> "json"
  | `Java -> "java"
  | `JavaScript -> "javascript"
  | `Makefile -> "makefile"
  | `Markdown -> "markdown"
  | `Nginx -> "nginx"
  | `Nix -> "nix"
  | `OCaml -> "ocaml"
  | `Objective_C -> "objectivec"
  | `PHP -> "php"
  | `Perl -> "perl"
  | `Prolog -> "prolog"
  | `Python -> "python"
  | `Ruby -> "ruby"
  | `Rust -> "rust"
  | `ReasonML -> "reasonml"
  | `SQL -> "sql"
  | `Scala -> "scala"
  | `Scheme -> "scheme"
  | `Shell -> "shell"
  | `Swift -> "swift"
  | `TeX -> "tex"
  | `VHDL -> "vhdl"
  | `Verilog -> "verilog"
  | `x86 -> "x86asm"
