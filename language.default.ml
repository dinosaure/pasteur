type t =
 Diff
 | Xml
 | Markdown
 | Rust
 | Css
 | Cpp
 | Lua
 | Java
 | Scss
 | Go
 | Typescript
 | Vbnet
 | Swift
 | Makefile
 | Javascript
 | Objectivec
 | Less
 | Bash
 | Shell
 | Ini
 | Php
 | Php_template
 | Json
 | R
 | Python
 | Python_repl
 | Csharp
 | Perl
 | Kotlin
 | Plaintext
 | Ruby
 | Yaml
 | Sql
 | C
let all =
 [Diff; Xml; Markdown; Rust; Css; Cpp; Lua; Java; Scss; Go; Typescript;
  Vbnet; Swift; Makefile; Javascript; Objectivec; Less; Bash; Shell; 
  Ini; Php; Php_template; Json; R; Python; Python_repl; Csharp; Perl; 
  Kotlin; Plaintext; Ruby; Yaml; Sql; C]
let to_string = function
 Diff -> "diff"
 | Xml -> "xml"
 | Markdown -> "markdown"
 | Rust -> "rust"
 | Css -> "css"
 | Cpp -> "cpp"
 | Lua -> "lua"
 | Java -> "java"
 | Scss -> "scss"
 | Go -> "go"
 | Typescript -> "typescript"
 | Vbnet -> "vbnet"
 | Swift -> "swift"
 | Makefile -> "makefile"
 | Javascript -> "javascript"
 | Objectivec -> "objectivec"
 | Less -> "less"
 | Bash -> "bash"
 | Shell -> "shell"
 | Ini -> "ini"
 | Php -> "php"
 | Php_template -> "php-template"
 | Json -> "json"
 | R -> "r"
 | Python -> "python"
 | Python_repl -> "python-repl"
 | Csharp -> "csharp"
 | Perl -> "perl"
 | Kotlin -> "kotlin"
 | Plaintext -> "plaintext"
 | Ruby -> "ruby"
 | Yaml -> "yaml"
 | Sql -> "sql"
 | C -> "c"
let of_string = function
 "diff" -> Diff
 | "xml" -> Xml
 | "markdown" -> Markdown
 | "rust" -> Rust
 | "css" -> Css
 | "cpp" -> Cpp
 | "lua" -> Lua
 | "java" -> Java
 | "scss" -> Scss
 | "go" -> Go
 | "typescript" -> Typescript
 | "vbnet" -> Vbnet
 | "swift" -> Swift
 | "makefile" -> Makefile
 | "javascript" -> Javascript
 | "objectivec" -> Objectivec
 | "less" -> Less
 | "bash" -> Bash
 | "shell" -> Shell
 | "ini" -> Ini
 | "php" -> Php
 | "php-template" -> Php_template
 | "json" -> Json
 | "r" -> R
 | "python" -> Python
 | "python-repl" -> Python_repl
 | "csharp" -> Csharp
 | "perl" -> Perl
 | "kotlin" -> Kotlin
 | "plaintext" -> Plaintext
 | "ruby" -> Ruby
 | "yaml" -> Yaml
 | "sql" -> Sql
 | "c" -> C
 | v -> Fmt.failwith "invalid language: %s" v
