type t =
 Bash
 | Armasm
 | Ebnf
 | Wasm
 | Vhdl
 | Diff
 | Swift
 | Lua
 | Xml
 | Twig
 | Ceylon
 | Scilab
 | Prolog
 | Mathematica
 | Ruby
 | Yaml
 | Awk
 | Go
 | Python
 | Elm
 | Coffeescript
 | Smalltalk
 | Haskell
 | Markdown
 | Sml
 | Cmake
 | Json
 | Scheme
 | Rust
 | Sql
 | X86asm
 | Bnf
 | Javascript
 | Node_repl
 | Objectivec
 | Fsharp
 | Haxe
 | Css
 | Scala
 | Cpp
 | Ada
 | Plaintext
 | R
 | Dns
 | Nim
 | Kotlin
 | Delphi
 | Vim
 | Java
 | Ini
 | Ocaml
 | Dart
 | Verilog
 | Crystal
 | Haml
 | Php
 | Coq
 | Python_repl
 | Dockerfile
 | Reasonml
 | Scss
 | C
 | Powershell
 | Shell
 | Q
 | Graphql
 | Fortran
 | Makefile
 | Vbnet
 | Typescript
 | Lisp
 | Perl
 | D
 | Php_template
 | Julia
 | Latex
 | Less
 | Llvm
 | Julia_repl
 | Erlang
 | Http
 | Nix
 | Erlang_repl
 | Csharp
 | Basic
let all =
 [Bash; Armasm; Ebnf; Wasm; Vhdl; Diff; Swift; Lua; Xml; Twig; Ceylon;
  Scilab; Prolog; Mathematica; Ruby; Yaml; Awk; Go; Python; Elm;
  Coffeescript; Smalltalk; Haskell; Markdown; Sml; Cmake; Json; Scheme; 
  Rust; Sql; X86asm; Bnf; Javascript; Node_repl; Objectivec; Fsharp; 
  Haxe; Css; Scala; Cpp; Ada; Plaintext; R; Dns; Nim; Kotlin; Delphi; 
  Vim; Java; Ini; Ocaml; Dart; Verilog; Crystal; Haml; Php; Coq; Python_repl;
  Dockerfile; Reasonml; Scss; C; Powershell; Shell; Q; Graphql; Fortran;
  Makefile; Vbnet; Typescript; Lisp; Perl; D; Php_template; Julia; Latex;
  Less; Llvm; Julia_repl; Erlang; Http; Nix; Erlang_repl; Csharp; Basic]
let to_string = function
 Bash -> "bash"
 | Armasm -> "armasm"
 | Ebnf -> "ebnf"
 | Wasm -> "wasm"
 | Vhdl -> "vhdl"
 | Diff -> "diff"
 | Swift -> "swift"
 | Lua -> "lua"
 | Xml -> "xml"
 | Twig -> "twig"
 | Ceylon -> "ceylon"
 | Scilab -> "scilab"
 | Prolog -> "prolog"
 | Mathematica -> "mathematica"
 | Ruby -> "ruby"
 | Yaml -> "yaml"
 | Awk -> "awk"
 | Go -> "go"
 | Python -> "python"
 | Elm -> "elm"
 | Coffeescript -> "coffeescript"
 | Smalltalk -> "smalltalk"
 | Haskell -> "haskell"
 | Markdown -> "markdown"
 | Sml -> "sml"
 | Cmake -> "cmake"
 | Json -> "json"
 | Scheme -> "scheme"
 | Rust -> "rust"
 | Sql -> "sql"
 | X86asm -> "x86asm"
 | Bnf -> "bnf"
 | Javascript -> "javascript"
 | Node_repl -> "node-repl"
 | Objectivec -> "objectivec"
 | Fsharp -> "fsharp"
 | Haxe -> "haxe"
 | Css -> "css"
 | Scala -> "scala"
 | Cpp -> "cpp"
 | Ada -> "ada"
 | Plaintext -> "plaintext"
 | R -> "r"
 | Dns -> "dns"
 | Nim -> "nim"
 | Kotlin -> "kotlin"
 | Delphi -> "delphi"
 | Vim -> "vim"
 | Java -> "java"
 | Ini -> "ini"
 | Ocaml -> "ocaml"
 | Dart -> "dart"
 | Verilog -> "verilog"
 | Crystal -> "crystal"
 | Haml -> "haml"
 | Php -> "php"
 | Coq -> "coq"
 | Python_repl -> "python-repl"
 | Dockerfile -> "dockerfile"
 | Reasonml -> "reasonml"
 | Scss -> "scss"
 | C -> "c"
 | Powershell -> "powershell"
 | Shell -> "shell"
 | Q -> "q"
 | Graphql -> "graphql"
 | Fortran -> "fortran"
 | Makefile -> "makefile"
 | Vbnet -> "vbnet"
 | Typescript -> "typescript"
 | Lisp -> "lisp"
 | Perl -> "perl"
 | D -> "d"
 | Php_template -> "php-template"
 | Julia -> "julia"
 | Latex -> "latex"
 | Less -> "less"
 | Llvm -> "llvm"
 | Julia_repl -> "julia-repl"
 | Erlang -> "erlang"
 | Http -> "http"
 | Nix -> "nix"
 | Erlang_repl -> "erlang-repl"
 | Csharp -> "csharp"
 | Basic -> "basic"
let of_string = function
 "bash" -> Bash
 | "armasm" -> Armasm
 | "ebnf" -> Ebnf
 | "wasm" -> Wasm
 | "vhdl" -> Vhdl
 | "diff" -> Diff
 | "swift" -> Swift
 | "lua" -> Lua
 | "xml" -> Xml
 | "twig" -> Twig
 | "ceylon" -> Ceylon
 | "scilab" -> Scilab
 | "prolog" -> Prolog
 | "mathematica" -> Mathematica
 | "ruby" -> Ruby
 | "yaml" -> Yaml
 | "awk" -> Awk
 | "go" -> Go
 | "python" -> Python
 | "elm" -> Elm
 | "coffeescript" -> Coffeescript
 | "smalltalk" -> Smalltalk
 | "haskell" -> Haskell
 | "markdown" -> Markdown
 | "sml" -> Sml
 | "cmake" -> Cmake
 | "json" -> Json
 | "scheme" -> Scheme
 | "rust" -> Rust
 | "sql" -> Sql
 | "x86asm" -> X86asm
 | "bnf" -> Bnf
 | "javascript" -> Javascript
 | "node-repl" -> Node_repl
 | "objectivec" -> Objectivec
 | "fsharp" -> Fsharp
 | "haxe" -> Haxe
 | "css" -> Css
 | "scala" -> Scala
 | "cpp" -> Cpp
 | "ada" -> Ada
 | "plaintext" -> Plaintext
 | "r" -> R
 | "dns" -> Dns
 | "nim" -> Nim
 | "kotlin" -> Kotlin
 | "delphi" -> Delphi
 | "vim" -> Vim
 | "java" -> Java
 | "ini" -> Ini
 | "ocaml" -> Ocaml
 | "dart" -> Dart
 | "verilog" -> Verilog
 | "crystal" -> Crystal
 | "haml" -> Haml
 | "php" -> Php
 | "coq" -> Coq
 | "python-repl" -> Python_repl
 | "dockerfile" -> Dockerfile
 | "reasonml" -> Reasonml
 | "scss" -> Scss
 | "c" -> C
 | "powershell" -> Powershell
 | "shell" -> Shell
 | "q" -> Q
 | "graphql" -> Graphql
 | "fortran" -> Fortran
 | "makefile" -> Makefile
 | "vbnet" -> Vbnet
 | "typescript" -> Typescript
 | "lisp" -> Lisp
 | "perl" -> Perl
 | "d" -> D
 | "php-template" -> Php_template
 | "julia" -> Julia
 | "latex" -> Latex
 | "less" -> Less
 | "llvm" -> Llvm
 | "julia-repl" -> Julia_repl
 | "erlang" -> Erlang
 | "http" -> Http
 | "nix" -> Nix
 | "erlang-repl" -> Erlang_repl
 | "csharp" -> Csharp
 | "basic" -> Basic
 | v -> Fmt.failwith "invalid language: %s" v
