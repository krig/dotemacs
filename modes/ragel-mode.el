(define-generic-mode 'ragel-mode
  '(?#) ;; comments
  '(
    ;; keywords
    "machine" "action" "access" "context" "include" "import" "export" "prepush" "postpop"
    "when" "inwhen" "outwhen" "err" "lerr" "eof" "from" "to"
    "alphtype" "getkey" "write"
    ;; rules
    "any" "ascii" "extend" "alpha" "digit" "alnum" "lower" "upper"
    "xdigit" "cntrl" "graph" "print" "punct" "space" "zlen" "empty"
    ;; inline code matching
    "fpc" "fc" "fcurs" "fbuf" "fblen" "ftargs" "fstack"
    "fhold" "fgoto" "fcall" "fret" "fentry" "fnext" "fexec" "fbreak"
    )
  '(
    ;; literals
    ;; ("\\([^\\)]*\\)" . font-lock-constant-face)
    ;; ("\\[[[^\\]]*\\]" . font-lock-constant-face)
    ("\(\"\\?'\"\'|\\?\"'\|'[^']*'\|\"[^\"]*\"\)" . font-lock-constant-face)

    ;; Numbers
    ("[0-9][0-9]*" . font-lock-constant-face)
    ("0x[0-9a-fA-F][0-9a-fA-F]*" . font-lock-constant-face)

    ;; Operators
    ("[>$%@]" . font-lock-constant-face)
    ("<>\|<" . font-lock-constant-face)
    ;; ("[>\<$%@][!\^/*~]" . font-lock-constant-face)
    ;; ("[>$%]?" . font-lock-constant-face)
    ;; ("<>[!\^/*~]" . font-lock-constant-face)
    ("=>" . font-lock-constant-face)
    ("->" . font-lock-constant-face)

    (":>" . font-lock-constant-face)
    (":>>" . font-lock-constant-face)
    ("<:" . font-lock-constant-face)
    )
  '(".rl\\'")
  nil
  "Generic mode for editing .rl files.")
