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

let language_of_value = function
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
  | _ -> invalid_arg "invalid language"

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

let witness =
  let open Irmin.Type in
  variant "language"
   (fun
    abnf
    arm
    apache
    awk
    bash
    bnf
    cs
    cpp
    coq
    css
    capnproto
    clojure
    coffeescript
    dns
    diff
    dockerfile
    ebnf
    elm
    erlang
    fsharp
    go
    html
    http
    haskell
    haxe
    ini
    json
    java
    javascript
    makefile
    markdown
    nginx
    nix
    ocaml
    objectivec
    php
    perl
    prolog
    python
    ruby
    rust
    reasonml
    sql
    scala
    scheme
    shell
    swift
    tex
    vhdl
    verilog
    x86asm
  -> function
  | `ABNF -> abnf
  | `ARM_assembler -> arm
  | `Apache -> apache
  | `Awk -> awk
  | `Bash -> bash
  | `BNF -> bnf
  | `C_Sharp -> cs
  | `Cpp -> cpp
  | `Coq -> coq
  | `CSS -> css
  | `Cap_n_Proto -> capnproto
  | `Clojure -> clojure
  | `CoffeeScript -> coffeescript
  | `DNS_Zone_file -> dns
  | `Diff -> diff
  | `Dockerfile -> dockerfile
  | `EBNF -> ebnf
  | `Elm -> elm
  | `Erlang -> erlang
  | `F_Sharp -> fsharp
  | `Go -> go
  | `HTML -> html
  | `HTTP -> http
  | `Haskell -> haskell
  | `Haxe -> haxe
  | `Ini -> ini
  | `JSON -> json
  | `Java -> java
  | `JavaScript -> javascript
  | `Makefile -> makefile
  | `Markdown -> markdown
  | `Nginx -> nginx
  | `Nix -> nix
  | `OCaml -> ocaml
  | `Objective_C -> objectivec
  | `PHP -> php
  | `Perl -> perl
  | `Prolog -> prolog
  | `Python -> python
  | `Ruby -> ruby
  | `Rust -> rust
  | `ReasonML -> reasonml
  | `SQL -> sql
  | `Scala -> scala
  | `Scheme -> scheme
  | `Shell -> shell
  | `Swift -> swift
  | `TeX -> tex
  | `VHDL -> vhdl
  | `Verilog -> verilog
  | `x86 -> x86asm)
  |~ case0 "abnf" `ABNF
  |~ case0 "arm" `ARM_assembler
  |~ case0 "apache" `Apache
  |~ case0 "awk" `Awk
  |~ case0 "bash" `Bash
  |~ case0 "bnf" `BNF
  |~ case0 "cs" `C_Sharp
  |~ case0 "cpp" `Cpp
  |~ case0 "coq" `Coq
  |~ case0 "css" `CSS
  |~ case0 "capnproto" `Cap_n_Proto
  |~ case0 "clojure" `Clojure
  |~ case0 "coffeescript" `CoffeeScript
  |~ case0 "dns" `DNS_Zone_file
  |~ case0 "diff" `Diff
  |~ case0 "dockerfile" `Dockerfile
  |~ case0 "ebnf" `EBNF
  |~ case0 "elm" `Elm
  |~ case0 "erlang" `Erlang
  |~ case0 "fsharp" `F_Sharp
  |~ case0 "go" `Go
  |~ case0 "html" `HTML
  |~ case0 "http" `HTTP
  |~ case0 "haskell" `Haskell
  |~ case0 "haxe" `Haxe
  |~ case0 "ini" `Ini
  |~ case0 "json" `JSON
  |~ case0 "java" `Java
  |~ case0 "javascript" `JavaScript
  |~ case0 "makefile" `Makefile
  |~ case0 "markdown" `Markdown
  |~ case0 "nginx" `Nginx
  |~ case0 "nix" `Nix
  |~ case0 "ocaml" `OCaml
  |~ case0 "objectivec" `Objective_C
  |~ case0 "php" `PHP
  |~ case0 "perl" `Perl
  |~ case0 "prolog" `Prolog
  |~ case0 "python" `Python
  |~ case0 "ruby" `Ruby
  |~ case0 "rust" `Rust
  |~ case0 "reasonml" `ReasonML
  |~ case0 "sql" `SQL
  |~ case0 "scala" `Scala
  |~ case0 "scheme" `Scheme
  |~ case0 "shell" `Shell
  |~ case0 "swift" `Swift
  |~ case0 "tex" `TeX
  |~ case0 "vhdl" `VHDL
  |~ case0 "verilog" `Verilog
  |~ case0 "x86asm" `x86
  |> sealv
