;;; scl-mode.el --- a major-mode for editing scl scripts

;; Author: 2011-2013 immerrr <immerrr+scl@gmail.com>
;;         2010-2011 Reuben Thomas <rrt@sc3d.org>
;;         2006 Juergen Hoetzel <juergen@hoetzel.info>
;;         2004 various (support for scl 5 and byte compilation)
;;         2001 Christian Vogler <cvogler@gradient.cis.upenn.edu>
;;         1997 Bret Mogilefsky <mogul-scl@gelatinous.com> starting from
;;              tcl-mode by Gregor Schmid <schmid@fb3-s7.math.tu-berlin.de>
;;              with tons of assistance from
;;              Paul Du Bois <pld-scl@gelatinous.com> and
;;              Aaron Smith <aaron-scl@gelatinous.com>.
;;
;; URL:         http://immerrr.github.com/scl-mode
;; Version:     20151025
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;; Keywords: languages, processes, tools

;; This field is expanded to commit SHA, date & associated heads/tags during
;; archive creation.
;; Revision: $Format:%h (%cD %d)$
;;

;;; Commentary:

;; scl-mode provides support for editing scl, including automatical
;; indentation, syntactical font-locking, running interactive shell,
;; interacting with `hs-minor-mode' and online documentation lookup.

;; The following variables are available for customization (see more via
;; `M-x customize-group scl`):

;; - Var `scl-indent-level':
;;   indentation offset in spaces
;; - Var `scl-indent-string-contents':
;;   set to `t` if you like to have contents of multiline strings to be
;;   indented like comments
;; - Var `scl-mode-hook':
;;   list of functions to execute when scl-mode is initialized
;; - Var `scl-documentation-url':
;;   base URL for documentation lookup
;; - Var `scl-documentation-function': function used to
;;   show documentation (`eww` is a viable alternative for Emacs 25)

;; These are variables/commands that operate on the scl process:

;; - Var `scl-default-application':
;;   command to start the scl process (REPL)
;; - Var `scl-default-command-switches':
;;   arguments to pass to the scl process on startup (make sure `-i` is there
;;   if you expect working with scl shell interactively)
;; - Cmd `scl-start-process': start new REPL process, usually happens automatically
;; - Cmd `scl-kill-process': kill current REPL process

;; These are variables/commands for interaction with the scl process:

;; - Cmd `scl-show-process-buffer': switch to REPL buffer
;; - Cmd `scl-hide-process-buffer': hide window showing REPL buffer
;; - Var `scl-always-show': show REPL buffer after sending something
;; - Cmd `scl-send-buffer': send whole buffer
;; - Cmd `scl-send-current-line': send current line
;; - Cmd `scl-send-defun': send current top-level function
;; - Cmd `scl-send-region': send active region
;; - Cmd `scl-restart-with-whole-file': restart REPL and send whole buffer

;; See "M-x apropos-command ^scl-" for a list of commands.
;; See "M-x customize-group scl" for a list of customizable variables.


;;; Code:
(eval-when-compile
  (require 'cl))

(require 'comint)
(require 'newcomment)
(require 'rx)


;; rx-wrappers for scl

(eval-when-compile
  ;; Silence compilation warning about `compilation-error-regexp-alist' defined
  ;; in compile.el.
  (require 'compile))

(eval-and-compile
  (defvar scl-rx-constituents)
  (defvar rx-parent)

  (defun scl-rx-to-string (form &optional no-group)
    "scl-specific replacement for `rx-to-string'.

See `rx-to-string' documentation for more information FORM and
NO-GROUP arguments."
    (let ((rx-constituents scl-rx-constituents))
      (rx-to-string form no-group)))

  (defmacro scl-rx (&rest regexps)
    "scl-specific replacement for `rx'.

See `rx' documentation for more information about REGEXPS param."
    (cond ((null regexps)
           (error "No regexp"))
          ((cdr regexps)
           (scl-rx-to-string `(and ,@regexps) t))
          (t
           (scl-rx-to-string (car regexps) t))))

  (defun scl--new-rx-form (form)
    "Add FORM definition to `scl-rx' macro.

FORM is a cons (NAME . DEFN), see more in `rx-constituents' doc.
This function enables specifying new definitions using old ones:
if DEFN is a list that starts with `:rx' symbol its second
element is itself expanded with `scl-rx-to-string'. "
    (let ((name (car form))
          (form-definition (cdr form)))
      (when (and (listp form-definition) (eq ':rx (car form-definition)))
        (setcdr form (scl-rx-to-string (cadr form-definition) 'nogroup)))
      (push form scl-rx-constituents)))

  (defun scl--rx-symbol (form)
    ;; form is a list (symbol XXX ...)
    ;; Skip initial 'symbol
    (setq form (cdr form))
    ;; If there's only one element, take it from the list, otherwise wrap the
    ;; whole list into `(or XXX ...)' form.
    (setq form (if (eq 1 (length form))
                   (car form)
                 (append '(or) form)))
    (rx-form `(seq symbol-start ,form symbol-end) rx-parent))

  (setq scl-rx-constituents (copy-sequence rx-constituents))

  (mapc #'scl--new-rx-form
        `((symbol scl--rx-symbol 1 nil)
          (ws . "[ \t]*") (ws+ . "[ \t]+")
          (scl-name :rx (symbol (regexp "[[:alpha:]_]+[[:alnum:]_]*")))
          (scl-funcname
           :rx (seq scl-name (* ws "." ws scl-name)
                    (opt ws ":" ws scl-name)))
          (scl-funcheader
           ;; Outer (seq ...) is here to shy-group the definition
           :rx (seq (or (seq (symbol "function") ws (group-n 1 scl-funcname))
                        (seq (group-n 1 scl-funcname) ws "=" ws
                             (symbol "function")))))
          (scl-number
           :rx (seq (or (seq (+ digit) (opt ".") (* digit))
                        (seq (* digit) (opt ".") (+ digit)))
                    (opt (regexp "[eE][+-]?[0-9]+"))))
          (scl-assignment-op
           :rx (seq "=" (or buffer-end (not (any "=")))))
          (scl-token
           :rx (or "+" "-" "*" "/" "%" "^" "#" "==" "~=" "<=" ">=" "<"
                   ">" "=" ";" ":" "," "." ".." "..."))
          (scl-keyword
           :rx (symbol "AND" "break" "do" "ELSE" "ELSIF" "END_IF"  "FOR" "function"
                       "goto" "IF" "in" "local" "NOT" "OR" "repeat" "return"
                       "THEN" "until" "WHILE")))
        ))


;; Local variables
(defgroup scl nil
  "Major mode for editing scl code."
  :prefix "scl-"
  :group 'languages)

(defcustom scl-indent-level 4
  "Amount by which scl subexpressions are indented."
  :type 'integer
  :group 'scl
  :safe #'integerp)

(defcustom scl-comment-start "// "
  "Default value of `comment-start'."
  :type 'string
  :group 'scl)

(defcustom scl-comment-start-skip "---*[ \t]*"
  "Default value of `comment-start-skip'."
  :type 'string
  :group 'scl)

(defcustom scl-default-application "scl"
  "Default application to run in scl process."
  :type '(choice (string)
                 (cons string integer))
  :group 'scl)

(defcustom scl-default-command-switches (list "-i")
  "Command switches for `scl-default-application'.
Should be a list of strings."
  :type '(repeat string)
  :group 'scl)
(make-variable-buffer-local 'scl-default-command-switches)

(defcustom scl-always-show t
  "*Non-nil means display scl-process-buffer after sending a command."
  :type 'boolean
  :group 'scl)

(defcustom scl-documentation-function 'browse-url
  "Function used to fetch the scl reference manual."
  :type `(radio (function-item browse-url)
                ,@(when (fboundp 'eww) '((function-item eww)))
                ,@(when (fboundp 'w3m-browse-url) '((function-item w3m-browse-url)))
                (function :tag "Other function"))
  :group 'scl)

(defcustom scl-documentation-url
  (or (and (file-readable-p "/usr/share/doc/scl/manual.html")
           "file:///usr/share/doc/scl/manual.html")
      "http://www.scl.org/manual/5.1/manual.html")
  "URL pointing to the scl reference manual."
  :type 'string
  :group 'scl)


(defvar scl-process nil
  "The active scl process")

(defvar scl-process-buffer nil
  "Buffer used for communication with the scl process")

(defun scl--customize-set-prefix-key (prefix-key-sym prefix-key-val)
  (cl-assert (eq prefix-key-sym 'scl-prefix-key))
  (set prefix-key-sym (if (and prefix-key-val (> (length prefix-key-val) 0))
                          ;; read-kbd-macro returns a string or a vector
                          ;; in both cases (elt x 0) is ok
                          (elt (read-kbd-macro prefix-key-val) 0)))
  (if (fboundp 'scl-prefix-key-update-bindings)
      (scl-prefix-key-update-bindings)))

(defcustom scl-prefix-key "\C-c"
  "Prefix for all scl-mode commands."
  :type 'string
  :group 'scl
  :set 'scl--customize-set-prefix-key
  :get '(lambda (sym)
          (let ((val (eval sym))) (if val (single-key-description (eval sym)) ""))))

(defvar scl-mode-menu (make-sparse-keymap "scl")
  "Keymap for scl-mode's menu.")

(defvar scl-prefix-mode-map
  (eval-when-compile
    (let ((result-map (make-sparse-keymap)))
      (mapc (lambda (key_defn)
              (define-key result-map (read-kbd-macro (car key_defn)) (cdr key_defn)))
            '(("C-l" . scl-send-buffer)
              ("C-f" . scl-search-documentation)))
      result-map))
  "Keymap that is used to define keys accessible by `scl-prefix-key'.

If the latter is nil, the keymap translates into `scl-mode-map' verbatim.")

(defvar scl--electric-indent-chars
  (mapcar #'string-to-char '("}" "]" ")")))


(defvar scl-mode-map
  (let ((result-map (make-sparse-keymap))
        prefix-key)
    (unless (boundp 'electric-indent-chars)
      (mapc (lambda (electric-char)
              (define-key result-map
                (read-kbd-macro
                 (char-to-string electric-char))
                #'scl-electric-match))
            scl--electric-indent-chars))
    (define-key result-map [menu-bar scl-mode] (cons "scl" scl-mode-menu))

    ;; FIXME: see if the declared logic actually works
    ;; handle prefix-keyed bindings:
    ;; * if no prefix, set prefix-map as parent, i.e.
    ;;      if key is not defined look it up in prefix-map
    ;; * if prefix is set, bind the prefix-map to that key
    (if (boundp 'scl-prefix-key)
        (define-key result-map (vector scl-prefix-key) scl-prefix-mode-map)
      (set-keymap-parent result-map scl-prefix-mode-map))
    result-map)
  "Keymap used in scl-mode buffers.")

(defvar scl-electric-flag t
  "If t, electric actions (like automatic reindentation) will happen when an electric
 key like `{' is pressed")
(make-variable-buffer-local 'scl-electric-flag)

(defcustom scl-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"
  "Regexp which matches the scl program's prompt."
  :type  'regexp
  :group 'scl)

(defcustom scl-traceback-line-re
  ;; This regexp skips prompt and meaningless "stdin:N:" prefix when looking
  ;; for actual file-line locations.
  "^\\(?:[\t ]*\\|.*>[\t ]+\\)\\(?:[^\n\t ]+:[0-9]+:[\t ]*\\)*\\(?:\\([^\n\t ]+\\):\\([0-9]+\\):\\)"
  "Regular expression that describes tracebacks and errors."
  :type 'regexp
  :group 'scl)

(defvar scl--repl-buffer-p nil
  "Buffer-local flag saying if this is a scl REPL buffer.")
(make-variable-buffer-local 'scl--repl-buffer-p)


(defadvice compilation-find-file (around scl--repl-find-file
                                         (marker filename directory &rest formats)
                                         activate)
  "Return scl REPL buffer when looking for \"stdin\" file in it."
  (if (and
       scl--repl-buffer-p
       (string-equal filename "stdin")
       ;; NOTE: this doesn't traverse `compilation-search-path' when
       ;; looking for filename.
       (not (file-exists-p (expand-file-name
                        filename
                        (when directory (expand-file-name directory))))))
      (setq ad-return-value (current-buffer))
    ad-do-it))


(defadvice compilation-goto-locus (around scl--repl-goto-locus
                                          (msg mk end-mk)
                                          activate)
  "When message points to scl REPL buffer, go to the message itself.
Usually, stdin:XX line number points to nowhere."
  (let ((errmsg-buf (marker-buffer msg))
        (error-buf (marker-buffer mk)))
    (if (and (with-current-buffer errmsg-buf scl--repl-buffer-p)
             (eq error-buf errmsg-buf))
        (progn
          (compilation-set-window (display-buffer (marker-buffer msg)) msg)
          (goto-char msg))
      ad-do-it)))


(defcustom scl-indent-string-contents nil
  "If non-nil, contents of multiline string will be indented.
Otherwise leading amount of whitespace on each line is preserved."
  :group 'scl
  :type 'boolean)

(defcustom scl-jump-on-traceback t
  "*Jump to innermost traceback location in *scl* buffer.  When this
variable is non-nil and a traceback occurs when running scl code in a
process, jump immediately to the source code of the innermost
traceback location."
  :type 'boolean
  :group 'scl)

(defcustom scl-mode-hook nil
  "Hooks called when scl mode fires up."
  :type 'hook
  :group 'scl)

(defvar scl-region-start (make-marker)
  "Start of special region for scl communication.")

(defvar scl-region-end (make-marker)
  "End of special region for scl communication.")

(defvar scl-emacs-menu
  '(["Restart With Whole File" scl-restart-with-whole-file t]
    ["Kill Process" scl-kill-process t]
    ["Hide Process Buffer" scl-hide-process-buffer t]
    ["Show Process Buffer" scl-show-process-buffer t]
    ["Beginning Of Proc" scl-beginning-of-proc t]
    ["End Of Proc" scl-end-of-proc t]
    ["Set scl-Region Start" scl-set-scl-region-start t]
    ["Set scl-Region End" scl-set-scl-region-end t]
    ["Send scl-Region" scl-send-scl-region t]
    ["Send Current Line" scl-send-current-line t]
    ["Send Region" scl-send-region t]
    ["Send Proc" scl-send-proc t]
    ["Send Buffer" scl-send-buffer t]
    ["Search Documentation" scl-search-documentation t])
  "Emacs menu for scl mode.")

;; the whole defconst is inside eval-when-compile, because it's later referenced
;; inside another eval-and-compile block
(eval-and-compile
  (defconst
    scl--builtins
    (let*
        ((modules
          '("_G" "_VERSION" "assert" "collectgarbage" "dofile" "error" "getfenv"
            "getmetatable" "ipairs" "load" "loadfile" "loadstring" "module"
            "next" "pairs" "pcall" "print" "rawequal" "rawget" "rawlen" "rawset"
            "require" "select" "setfenv" "setmetatable" "tonumber" "tostring"
            "type" "unpack" "xpcall" "self"
            ("bit32" . ("arshift" "band" "bnot" "bor" "btest" "bxor" "extract"
                        "lrotate" "lshift" "replace" "rrotate" "rshift"))
            ("coroutine" . ("create" "isyieldable" "resume" "running" "status"
                            "wrap" "yield"))
            ("debug" . ("debug" "getfenv" "gethook" "getinfo" "getlocal"
                        "getmetatable" "getregistry" "getupvalue" "getuservalue"
                        "setfenv" "sethook" "setlocal" "setmetatable"
                        "setupvalue" "setuservalue" "traceback" "upvalueid"
                        "upvaluejoin"))
            ("io" . ("close" "flush" "input" "lines" "open" "output" "popen"
                     "read" "stderr" "stdin" "stdout" "tmpfile" "type" "write"))
            ("math" . ("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "cosh"
                       "deg" "exp" "floor" "fmod" "frexp" "huge" "ldexp" "log"
                       "log10" "max" "maxinteger" "min" "mininteger" "modf" "pi"
                       "pow" "rad" "random" "randomseed" "sin" "sinh" "sqrt"
                       "tan" "tanh" "tointeger" "type" "ult"))
            ("os" . ("clock" "date" "difftime" "execute" "exit" "getenv"
                     "remove"  "rename" "setlocale" "time" "tmpname"))
            ("package" . ("config" "cpath" "loaded" "loaders" "loadlib" "path"
                          "preload" "searchers" "searchpath" "seeall"))
            ("string" . ("byte" "char" "dump" "find" "format" "gmatch" "gsub"
                         "len" "lower" "match" "pack" "packsize" "rep" "reverse"
                         "sub" "unpack" "upper"))
            ("table" . ("concat" "insert" "maxn" "move" "pack" "remove" "sort"
                        "unpack"))
            ("utf8" . ("char" "charpattern" "codepoint" "codes" "len"
                       "offset")))))

      (cl-labels
       ((module-name-re (x)
                        (concat "\\(?1:\\_<"
                                (if (listp x) (car x) x)
                                "\\_>\\)"))
        (module-members-re (x) (if (listp x)
                                   (concat "\\(?:[ \t]*\\.[ \t]*"
                                           "\\_<\\(?2:"
                                           (regexp-opt (cdr x))
                                           "\\)\\_>\\)?")
                                 "")))

       (concat
        ;; common prefix:
        ;; - beginning-of-line
        ;; - or neither of [ '.', ':' ] to exclude "foo.string.rep"
        ;; - or concatenation operator ".."
        "\\(?:^\\|[^:. \t]\\|[.][.]\\)"
        ;; optional whitespace
        "[ \t]*"
        "\\(?:"
        ;; any of modules/functions
        (mapconcat (lambda (x) (concat (module-name-re x)
                                       (module-members-re x)))
                   modules
                   "\\|")
        "\\)"))))

  "A regexp that matches scl builtin functions & variables.

This is a compilation of 5.1, 5.2 and 5.3 builtins taken from the
index of respective scl reference manuals.")

(eval-and-compile
  (defun scl-make-delimited-matcher (elt-regexp sep-regexp end-regexp)
    "Construct matcher function for `font-lock-keywords' to match a sequence.

It's supposed to match sequences with following EBNF:

ELT-REGEXP { SEP-REGEXP ELT-REGEXP } END-REGEXP

The sequence is parsed one token at a time.  If non-nil is
returned, `match-data' will have one or more of the following
groups set according to next matched token:

1. matched element token
2. unmatched garbage characters
3. misplaced token (i.e. SEP-REGEXP when ELT-REGEXP is expected)
4. matched separator token
5. matched end token

Blanks & comments between tokens are silently skipped.
Groups 6-9 can be used in any of argument regexps."
    (lexical-let*
        ((delimited-matcher-re-template
          "\\=\\(?2:.*?\\)\\(?:\\(?%s:\\(?4:%s\\)\\|\\(?5:%s\\)\\)\\|\\(?%s:\\(?1:%s\\)\\)\\)")
         ;; There's some magic to this regexp. It works as follows:
         ;;
         ;; A. start at (point)
         ;; B. non-greedy match of garbage-characters (?2:)
         ;; C. try matching separator (?4:) or end-token (?5:)
         ;; D. try matching element (?1:)
         ;;
         ;; Simple, but there's a trick: pt.C and pt.D are embraced by one more
         ;; group whose purpose is determined only after the template is
         ;; formatted (?%s:):
         ;;
         ;; - if element is expected, then D's parent group becomes "shy" and C's
         ;;   parent becomes group 3 (aka misplaced token), so if D matches when
         ;;   an element is expected, it'll be marked with warning face.
         ;;
         ;; - if separator-or-end-token is expected, then it's the opposite:
         ;;   C's parent becomes shy and D's will be matched as misplaced token.
         (elt-expected-re (format delimited-matcher-re-template
                                  3 sep-regexp end-regexp "" elt-regexp))
         (sep-or-end-expected-re (format delimited-matcher-re-template
                                         "" sep-regexp end-regexp 3 elt-regexp)))

      (lambda (end)
        (let* ((prev-elt-p (match-beginning 1))
               (prev-sep-p (match-beginning 4))
               (prev-end-p (match-beginning 5))

               (regexp (if prev-elt-p sep-or-end-expected-re elt-expected-re))
               (comment-start (scl-comment-start-pos (syntax-ppss)))
               (parse-stop end))

          ;; If token starts inside comment, or end-token was encountered, stop.
          (when (and (not comment-start)
                     (not prev-end-p))
            ;; Skip all comments & whitespace. forward-comment doesn't have boundary
            ;; argument, so make sure point isn't beyond parse-stop afterwards.
            (while (and (< (point) end)
                        (forward-comment 1)))
            (goto-char (min (point) parse-stop))

            ;; Reuse comment-start variable to store beginning of comment that is
            ;; placed before line-end-position so as to make sure token search doesn't
            ;; enter that comment.
            (setq comment-start
                  (scl-comment-start-pos
                   (save-excursion
                     (parse-partial-sexp (point) parse-stop
                                         nil nil nil 'stop-inside-comment)))
                  parse-stop (or comment-start parse-stop))

            ;; Now, let's match stuff.  If regular matcher fails, declare a span of
            ;; non-blanks 'garbage', and the next iteration will start from where the
            ;; garbage ends.  If couldn't match any garbage, move point to the end
            ;; and return nil.
            (or (re-search-forward regexp parse-stop t)
                (re-search-forward "\\(?1:\\(?2:[^ \t]+\\)\\)" parse-stop 'skip)
                (prog1 nil (goto-char end)))))))))


(defvar scl-font-lock-keywords
  `(;; highlight the hash-bang line "#!/foo/bar/scl" as comment
    ("^#!.*$" . font-lock-comment-face)

    ;; Builtin constants
    (,(scl-rx (symbol "TRUE" "FALSE" "nil"))
     . font-lock-constant-face)

    ;; Keywords
    (,(scl-rx scl-keyword)
     . font-lock-keyword-face)

    ;; Labels used by the "goto" statement
    ;; Highlights the following syntax:  ::label::
    (,(scl-rx "::" ws scl-name ws "::")
      . font-lock-constant-face)

    ;; Hightlights the name of the label in the "goto" statement like
    ;; "goto label"
    (,(scl-rx (symbol (seq "goto" ws+ (group-n 1 scl-name))))
      (1 font-lock-constant-face))

    ;; Highlight scl builtin functions and variables
    (,scl--builtins
     (1 font-lock-builtin-face) (2 font-lock-builtin-face nil noerror))

    ("^[ \t]*\\_<for\\_>"
     (,(scl-make-delimited-matcher (scl-rx scl-name) ","
                                   (scl-rx (or (symbol "in") scl-assignment-op)))
      nil nil
      (1 font-lock-variable-name-face nil noerror)
      (2 font-lock-warning-face t noerror)
      (3 font-lock-warning-face t noerror)))

    ;; Handle local variable/function names
    ;;  local blalba, xyzzy =
    ;;        ^^^^^^  ^^^^^
    ;;
    ;;  local function foobar(x,y,z)
    ;;                 ^^^^^^
    ;;  local foobar = function(x,y,z)
    ;;        ^^^^^^
    ("^[ \t]*\\_<local\\_>"
     (0 font-lock-keyword-face)

     ;; (* nonl) at the end is to consume trailing characters or otherwise they
     ;; delimited matcher would attempt to parse them afterwards and wrongly
     ;; highlight parentheses as incorrect variable name characters.
     (,(scl-rx point ws scl-funcheader (* nonl))
      nil nil
      (1 font-lock-function-name-face nil noerror))

     (,(scl-make-delimited-matcher (scl-rx scl-name) ","
                                   (scl-rx scl-assignment-op))
      nil nil
      (1 font-lock-variable-name-face nil noerror)
      (2 font-lock-warning-face t noerror)
      (3 font-lock-warning-face t noerror)))

    (,(scl-rx (or bol ";") ws scl-funcheader)
     (1 font-lock-function-name-face))

    (,(scl-rx (or (group-n 1
                           "@" (symbol "author" "copyright" "field" "release"
                                       "return" "see" "usage" "description"))
                  (seq (group-n 1 "@" (symbol "param" "class" "name")) ws+
                       (group-n 2 scl-name))))
     (1 font-lock-keyword-face t)
     (2 font-lock-variable-name-face t noerror)))

  "Default expressions to highlight in scl mode.")

(defvar scl-imenu-generic-expression
  `((nil ,(scl-rx (or bol ";") ws (opt (seq (symbol "local") ws)) scl-funcheader) 1))
  "Imenu generic expression for scl-mode.  See `imenu-generic-expression'.")

(defvar scl-sexp-alist '(("THEN" . "END_IF")
                      ("function" . "END_IF")
                      ("do" . "END_IF")
                      ("repeat" . "until")))

(defvar scl-mode-abbrev-table nil
  "Abbreviation table used in scl-mode buffers.")

(define-abbrev-table 'scl-mode-abbrev-table
  '(("END_IF"    "END_IF"    scl-indent-line :system t)
    ("ELSE"   "ELSE"   scl-indent-line :system t)
    ("ELSIF" "ELSIF" scl-indent-line :system t)))

(defvar scl-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; main comment syntax: begins with "--", ends with "\n"
    (modify-syntax-entry ?/ ". 12")
    (modify-syntax-entry ?\n ">")

    ;; main string syntax: bounded by ' or "
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?\" "\"")

    ;; single-character binary operators: punctuation
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?^ ".")
    (modify-syntax-entry ?% ".")
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?~ ".")

    (syntax-table))
  "`scl-mode' syntax table.")

;;;###autoload
(define-derived-mode scl-mode prog-mode "scl"
  "Major mode for editing scl code."
  :abbrev-table scl-mode-abbrev-table
  :syntax-table scl-mode-syntax-table
  :group 'scl
  (setq comint-prompt-regexp scl-prompt-regexp)


  (setq-local font-lock-defaults '(scl-font-lock-keywords ;; keywords
                                        nil                    ;; keywords-only
                                        nil                    ;; case-fold
                                        nil                    ;; syntax-alist
                                        nil                    ;; syntax-begin
                                        ))

  (setq-local syntax-propertize-function
              'scl--propertize-multiline-bounds)

  (setq-local parse-sexp-lookup-properties   t)
  (setq-local indent-line-function           'scl-indent-line)
  (setq-local beginning-of-defun-function    'scl-beginning-of-proc)
  (setq-local end-of-defun-function          'scl-end-of-proc)
  (setq-local comment-start                  scl-comment-start)
  (setq-local comment-start-skip             scl-comment-start-skip)
  (setq-local comment-use-syntax             t)
  (setq-local fill-paragraph-function        #'scl--fill-paragraph)
  (with-no-warnings
    (setq-local comment-use-global-state     t))
  (setq-local imenu-generic-expression       scl-imenu-generic-expression)
  (when (boundp 'electric-indent-chars)
    ;; If electric-indent-chars is not defined, electric indentation is done
    ;; via `scl-mode-map'.
    (setq-local electric-indent-chars
                  (append electric-indent-chars scl--electric-indent-chars)))


  ;; setup menu bar entry (XEmacs style)
  (if (and (featurep 'menubar)
           (boundp 'current-menubar)
           (fboundp 'set-buffer-menubar)
           (fboundp 'add-menu)
           (not (assoc "scl" current-menubar)))
      (progn
        (set-buffer-menubar (copy-sequence current-menubar))
        (add-menu nil "scl" scl-emacs-menu)))
  ;; Append scl menu to popup menu for Emacs.
  (if (boundp 'mode-popup-menu)
      (setq mode-popup-menu
            (cons (concat mode-name " Mode Commands") scl-emacs-menu)))

  ;; hideshow setup
  (unless (assq 'scl-mode hs-special-modes-alist)
    (add-to-list 'hs-special-modes-alist
                 `(scl-mode
                   ,(regexp-opt (mapcar 'car scl-sexp-alist) 'words) ;start
                   ,(regexp-opt (mapcar 'cdr scl-sexp-alist) 'words) ;end
                   nil scl-forward-sexp))))



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.scl\\'" . scl-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("scl" . scl-mode))

(defun scl-electric-match (arg)
  "Insert character and adjust indentation."
  (interactive "P")
  (let (blink-paren-function)
   (self-insert-command (prefix-numeric-value arg)))
  (if scl-electric-flag
      (scl-indent-line))
  (blink-matching-open))

;; private functions

(defun scl--fill-paragraph (&optional justify region)
  ;; Implementation of forward-paragraph for filling.
  ;;
  ;; This function works around a corner case in the following situations:
  ;;
  ;;     <>
  ;;     -- some very long comment ....
  ;;     some_code_right_after_the_comment
  ;;
  ;; If point is at the beginning of the comment line, fill paragraph code
  ;; would have gone for comment-based filling and done the right thing, but it
  ;; does not find a comment at the beginning of the empty line before the
  ;; comment and falls back to text-based filling ignoring comment-start and
  ;; spilling the comment into the code.
  (save-excursion
    (while (and (not (eobp))
                (progn (move-to-left-margin)
                       (looking-at paragraph-separate)))
      (forward-line 1))
    (let ((fill-paragraph-handle-comment t))
      (fill-paragraph justify region))))


(defun scl-prefix-key-update-bindings ()
  (let (old-cons)
    (if (eq scl-prefix-mode-map (keymap-parent scl-mode-map))
        ;; if prefix-map is a parent, delete the parent
        (set-keymap-parent scl-mode-map nil)
      ;; otherwise, look for it among children
      (if (setq old-cons (rassoc scl-prefix-mode-map scl-mode-map))
          (delq old-cons scl-mode-map)))

    (if (null scl-prefix-key)
        (set-keymap-parent scl-mode-map scl-prefix-mode-map)
      (define-key scl-mode-map (vector scl-prefix-key) scl-prefix-mode-map))))

(defun scl-set-prefix-key (new-key-str)
  "Changes `scl-prefix-key' properly and updates keymaps

This function replaces previous prefix-key binding with a new one."
  (interactive "sNew prefix key (empty string means no key): ")
  (scl--customize-set-prefix-key 'scl-prefix-key new-key-str)
  (message "Prefix key set to %S"  (single-key-description scl-prefix-key))
  (scl-prefix-key-update-bindings))

(defun scl-string-p (&optional pos)
  "Returns true if the point is in a string."
  (save-excursion (elt (syntax-ppss pos) 3)))

(defun scl-comment-start-pos (parsing-state)
  "Return position of comment containing current point.

If point is not inside a comment, return nil."
  (and parsing-state (nth 4 parsing-state) (nth 8 parsing-state)))

(defun scl-comment-or-string-p (&optional pos)
  "Returns true if the point is in a comment or string."
  (save-excursion (let ((parse-result (syntax-ppss pos)))
                    (or (elt parse-result 3) (elt parse-result 4)))))

(defun scl-comment-or-string-start-pos (&optional pos)
  "Returns start position of string or comment which contains point.

If point is not inside string or comment, return nil."
  (save-excursion (elt (syntax-ppss pos) 8)))

;; They're propertized as follows:
;; 1. generic-comment
;; 2. generic-string
;; 3. equals signs
(defconst scl-ml-begin-regexp
  "\\(?:\\(?1:-\\)-\\[\\|\\(?2:\\[\\)\\)\\(?3:=*\\)\\[")


(defun scl-try-match-multiline-end (end)
  "Try to match close-bracket for multiline literal around point.

Basically, detect form of close bracket from syntactic
information provided at point and re-search-forward to it."
  (let ((comment-or-string-start-pos (scl-comment-or-string-start-pos)))
    ;; Is there a literal around point?
    (and comment-or-string-start-pos
         ;; It is, check if the literal is a multiline open-bracket
         (save-excursion
           (goto-char comment-or-string-start-pos)
           (looking-at scl-ml-begin-regexp))

         ;; Yes it is, look for it matching close-bracket.  Close-bracket's
         ;; match group is determined by match-group of open-bracket.
         (re-search-forward
          (format "]%s\\(?%s:]\\)"
                  (match-string-no-properties 3)
                  (if (match-beginning 1) 1 2))
          end 'noerror))))


(defun scl-try-match-multiline-begin (limit)
  "Try to match multiline open-brackets.

Find next opening long bracket outside of any string/comment.
If none can be found before reaching LIMIT, return nil."

  (let (last-search-matched)
    (while
        ;; This loop will iterate skipping all multiline-begin tokens that are
        ;; inside strings or comments ending either at EOL or at valid token.
        (and (setq last-search-matched
                   (re-search-forward scl-ml-begin-regexp limit 'noerror))

             ;; Handle triple-hyphen '---[[' situation in which the multiline
             ;; opener should be skipped.
             ;;
             ;; In HYPHEN1-HYPHEN2-BRACKET1-BRACKET2 situation (match-beginning
             ;; 0) points to HYPHEN1, but if there's another hyphen before
             ;; HYPHEN1, standard syntax table will only detect comment-start
             ;; at HYPHEN2.
             ;;
             ;; We could check for comment-start at HYPHEN2, but then we'd have
             ;; to flush syntax-ppss cache to remove the result saying that at
             ;; HYPHEN2 there's no comment or string, because under some
             ;; circumstances that would hide the fact that we put a
             ;; comment-start property at HYPHEN1.
             (or (scl-comment-or-string-start-pos (match-beginning 0))
                 (and (eq ?- (char-after (match-beginning 0)))
                      (eq ?- (char-before (match-beginning 0)))))))

    last-search-matched))

(defun scl-match-multiline-literal-bounds (limit)
  ;; First, close any multiline literal spanning from previous block. This will
  ;; move the point accordingly so as to avoid double traversal.
  (or (scl-try-match-multiline-end limit)
      (scl-try-match-multiline-begin limit)))

(defun scl--propertize-multiline-bounds (start end)
  "Put text properties on beginnings and ends of multiline literals.

Intended to be used as a `syntax-propertize-function'."
  (save-excursion
    (goto-char start)
    (while (scl-match-multiline-literal-bounds end)
      (when (match-beginning 1)
        (put-text-property (match-beginning 1) (match-end 1)
                           'syntax-table (string-to-syntax "!")))
      (when (match-beginning 2)
        (put-text-property (match-beginning 2) (match-end 2)
                           'syntax-table (string-to-syntax "|"))))))


(defun scl-indent-line ()
  "Indent current line for scl mode.
Return the amount the indentation changed by."
  (let (indent
        (case-fold-search nil)
        ;; save point as a distance to eob - it's invariant w.r.t indentation
        (pos (- (point-max) (point))))
    (back-to-indentation)
    (if (scl-comment-or-string-p)
        (setq indent (scl-calculate-string-or-comment-indentation)) ;; just restore point position
      (setq indent (max 0 (scl-calculate-indentation nil))))

    (when (not (equal indent (current-column)))
      (delete-region (line-beginning-position) (point))
      (indent-to indent))

    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))

    indent))

(defun scl-calculate-string-or-comment-indentation ()
  "This function should be run when point at (current-indentation) is inside string"
  (if (and (scl-string-p) (not scl-indent-string-contents))
      ;; if inside string and strings aren't to be indented, return current indentation
      (current-indentation)

    ;; At this point, we know that we're inside comment, so make sure
    ;; close-bracket is unindented like a block that starts after
    ;; left-shifter.
    (let ((left-shifter-p (looking-at "\\s *\\(?://\\)?\\]\\(?1:=*\\)\\]")))
      (save-excursion
        (goto-char (scl-comment-or-string-start-pos))
        (+ (current-indentation)
           (if (and left-shifter-p
                    (looking-at (format "//\\[%s\\["
                                        (match-string-no-properties 1))))
               0
             scl-indent-level))))))

(defun scl-find-regexp (direction regexp &optional limit ignore-p)
  "Searches for a regular expression in the direction specified.
Direction is one of 'forward and 'backward.
By default, matches in comments and strings are ignored, but what to ignore is
configurable by specifying ignore-p. If the regexp is found, returns point
position, nil otherwise.
ignore-p returns true if the match at the current point position should be
ignored, nil otherwise."
  (let ((ignore-func (or ignore-p 'scl-comment-or-string-p))
        (search-func (if (eq direction 'forward)
                         're-search-forward 're-search-backward))
        (case-fold-search nil))
    (catch 'found
      (while (funcall search-func regexp limit t)
        (if (and (not (funcall ignore-func (match-beginning 0)))
                 (not (funcall ignore-func (match-end 0))))
            (throw 'found (point)))))))

(defconst scl-block-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "function" "repeat" "THEN"
                   "ELSE" "ELSIF" "END_IF" "until") t)
     "\\_>\\)\\|"
     (regexp-opt '("{" "(" "[" "]" ")" "}") t))))

(defconst scl-block-token-alist
  '(("do"       "\\_<end\\_>"   "\\_<for\\|while\\_>"                       middle-or-open)
    ("function" "\\_<end\\_>"   nil                                       open)
    ("repeat"   "\\_<until\\_>" nil                                       open)
    ("THEN"     "\\_<\\(E\\(LSE\\(IF\\)?\\|ND\\)\\)\\_>" "\\_<\\(ELSE\\)?IF\\_>" middle)
    ("{"        "}"           nil                                       open)
    ("["        "]"           nil                                       open)
    ("("        ")"           nil                                       open)
    ("IF"       "\\_<THEN\\_>"                                         open)
    ("FOR"      "\\_<DO\\_>"    nil                                       open)
    ("WHILE"    "\\_<DO\\_>"    nil                                       open)
    ("ELSE"     "\\_<END_IF\\_>"   "\\_<THEN\\_>"                              middle)
    ("ELSIF"   "\\_<then\\_>"  "\\_<then\\_>"                              middle)
    ("END_IF"      nil           "\\_<\\(ELSIF\\|IF\\|THEN\\|ELSE\\)\\_>" close)
    ("until"    nil           "\\_<repeat\\_>"                            close)
    ("}"        nil           "{"                                       close)
    ("]"        nil           "\\["                                     close)
    (")"        nil           "("                                       close))
    ;; '(
    ;; ("IF"       "\\_<THEN\\_>"   nil   open)
    ;; ("("        ")"              nil   open)
    ;; ("FOR"      "\\_<DO\\_>"     nil   open)
    ;; ("WHILE"    "\\_<DO\\_>"     nil   open)
    ;; ("ELSE"     "\\_<THEN\\_>"   nil   middle)
    ;; ("ELSIF"    "\\_<THEN\\_>"   nil   middle)
    ;; ("END_IF"  nil              nil   close)
    ;; (")"        nil              "("   close))
  "This is a list of block token information blocks.
Each token information entry is of the form:
  KEYWORD FORWARD-MATCH-REGEXP BACKWARDS-MATCH-REGEXP TOKEN-TYPE
KEYWORD is the token.
FORWARD-MATCH-REGEXP is a regexp that matches all possble tokens when going forward.
BACKWARDS-MATCH-REGEXP is a regexp that matches all possble tokens when going backwards.
TOKEN-TYPE determines where the token occurs on a statement. open indicates that the token appears at start, close indicates that it appears at end, middle indicates that it is a middle type token, and middle-or-open indicates that it can appear both as a middle or an open type.")

(defconst scl-indentation-modifier-regexp
  ;; The absence of else is deliberate, since it does not modify the
  ;; indentation level per se. It only may cause the line, in which the
  ;; else is, to be shifted to the left.
  (concat
   "\\(\\_<"
   (regexp-opt '("do" "function" "repeat" "THEN" "IF" "ELSE" "ELSIF" "FOR" "WHILE") t)
   "\\_>\\|"
   (regexp-opt '("{" "(" "["))
   "\\)\\|\\(\\_<"
   (regexp-opt '("END_IF" "until") t)
   "\\_>\\|"
   (regexp-opt '("]" ")" "}"))
   "\\)")
  )

(defun scl-get-block-token-info (token)
  "Returns the block token info entry for TOKEN from scl-block-token-alist"
  (assoc token scl-block-token-alist))

(defun scl-get-token-match-re (token-info direction)
  "Returns the relevant match regexp from token info"
  (cond
   ((eq direction 'forward) (cadr token-info))
   ((eq direction 'backward) (caddr token-info))
   (t nil)))

(defun scl-get-token-type (token-info)
  "Returns the relevant match regexp from token info"
   (cadddr token-info))

(defun scl-backwards-to-block-begin-or-end ()
  "Move backwards to nearest block begin or end.  Returns nil if not successful."
  (interactive)
  (scl-find-regexp 'backward scl-block-regexp))

(defun scl-find-matching-token-word (token &optional direction)
  (let* ((token-info (scl-get-block-token-info token))
         (match-type (scl-get-token-type token-info))
         ;; If we are on a middle token, go backwards. If it is a middle or open,
         ;; go forwards
         (search-direction (or direction
                               (if (or (eq match-type 'open)
                                       (eq match-type 'middle-or-open))
                                   'forward
                                 'backward)
                               'backward))
         (match (scl-get-token-match-re token-info search-direction))
         maybe-found-pos)
    ;; if we are searching forward from the token at the current point
    ;; (i.e. for a closing token), need to step one character forward
    ;; first, or the regexp will match the opening token.
    (if (eq search-direction 'forward) (forward-char 1))
    (catch 'found
      ;; If we are attempting to find a matching token for a terminating token
      ;; (i.e. a token that starts a statement when searching back, or a token
      ;; that ends a statement when searching forward), then we don't need to look
      ;; any further.
      (if (or (and (eq search-direction 'forward)
                   (eq match-type 'close))
              (and (eq search-direction 'backward)
                   (eq match-type 'open)))
          (throw 'found nil))
      (while (scl-find-regexp search-direction scl-indentation-modifier-regexp)
        ;; have we found a valid matching token?
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (let ((found-type (scl-get-token-type
                             (scl-get-block-token-info found-token))))
            (if (not (and match (string-match match found-token)))
                ;; no - then there is a nested block. If we were looking for
                ;; a block begin token, found-token must be a block end
                ;; token; likewise, if we were looking for a block end token,
                ;; found-token must be a block begin token, otherwise there
                ;; is a grammatical error in the code.
                (if (not (and
                          (or (eq match-type 'middle)
                              (eq found-type 'middle)
                              (eq match-type 'middle-or-open)
                              (eq found-type 'middle-or-open)
                              (eq match-type found-type))
                          (goto-char found-pos)
                          (scl-find-matching-token-word found-token
                                                        search-direction)))
                    (when maybe-found-pos
                      (goto-char maybe-found-pos)
                      (throw 'found maybe-found-pos)))
              ;; yes.
              ;; if it is a not a middle kind, report the location
              (when (not (or (eq found-type 'middle)
                             (eq found-type 'middle-or-open)))
                (throw 'found found-pos))
              ;; if it is a middle-or-open type, record location, but keep searching.
              ;; If we fail to complete the search, we'll report the location
              (when (eq found-type 'middle-or-open)
                (setq maybe-found-pos found-pos))
              ;; Cannot use tail recursion. too much nesting on long chains of
              ;; if/elseif. Will reset variables instead.
              (setq token found-token)
              (setq token-info (scl-get-block-token-info token))
              (setq match (scl-get-token-match-re token-info search-direction))
              (setq match-type (scl-get-token-type token-info))))))
      maybe-found-pos)))

(defun scl-goto-matching-block-token (&optional parse-start direction)
  "Find block begion/end token matching the one at the point.
This function moves the point to the token that matches the one
at the current point. Returns the point position of the first character of
the matching token if successful, nil otherwise."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil))
    (if (looking-at scl-indentation-modifier-regexp)
        (let ((position (scl-find-matching-token-word (match-string 0)
                                                      direction)))
          (and position
               (goto-char position))))))

(defun scl-goto-matching-block (&optional noreport)
  "Go to the keyword balancing the one under the point.
If the point is on a keyword/brace that starts a block, go to the
matching keyword that ends the block, and vice versa."
  (interactive)
  ;; search backward to the beginning of the keyword if necessary
  (if (eq (char-syntax (following-char)) ?w)
      (re-search-backward "\\_<" nil t))
  (let ((position (scl-goto-matching-block-token)))
    (if (and (not position)
             (not noreport))
        (error "Not on a block control keyword or brace")
      position)))

(defun scl-forward-line-skip-blanks (&optional back)
  "Move 1 line forward (back if BACK is non-nil) skipping blank lines.

Moves point 1 line forward (or backward) skipping lines that contain
no scl code besides comments. The point is put to the beginning of
the line.

Returns final value of point as integer or nil if operation failed."
  (catch 'found
    (while t
      (unless (eql (forward-line (if back -1 1)) 0)    ;; 0 means success
        (throw 'found nil))
      (unless (or (looking-at "\\s *\\(--.*\\)?$")
                  (scl-comment-or-string-p))
        (throw 'found (point))))))

(eval-when-compile
  (defconst scl-operator-class
    "-+*/^.=<>~:&|"))

(defconst scl-cont-eol-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("and" "or" "not" "in" "FOR" "WHILE"
                   "local" "function" "IF" "until" "ELSIF" "return")
                 t)
     "\\_>\\|"
     "\\(^\\|[^" scl-operator-class "]\\)"
     (regexp-opt '("+" "-" "*" "/" "%" "^" ".." "=="
                   "=" "<" ">" "<=" ">=" "~=" "." ":"
                   "&" "|" "~" ">>" "<<" "~")
                 t)
     "\\)"
     "\\s *\\="))
  "Regexp that matches the ending of a line that needs continuation

This regexp starts from eol and looks for a binary operator or an unclosed
block intro (i.e. 'for' without 'do' or 'if' without 'then') followed by
an optional whitespace till the end of the line.")

(defconst scl-cont-bol-regexp
  (eval-when-compile
    (concat
     "\\=\\s *"
     "\\(\\_<"
     (regexp-opt '("and" "or" "not") t)
     "\\_>\\|"
     (regexp-opt '("+" "-" "*" "/" "%" "^" ".." "=="
                   "=" "<" ">" "<=" ">=" "~=" "." ":"
                   "&" "|" "~" ">>" "<<" "~")
                 t)
     "\\($\\|[^" scl-operator-class "]\\)"
     "\\)"))
  "Regexp that matches a line that continues previous one

This regexp means, starting from point there is an optional whitespace followed
by scl binary operator. scl is very liberal when it comes to continuation line,
so we're safe to assume that every line that starts with a binop continues
previous one even though it looked like an end-of-statement.")

(defun scl-last-token-continues-p ()
  "Returns true if the last token on this line is a continuation token."
  (let ((line-begin (line-beginning-position))
        (line-end (line-end-position)))
    (save-excursion
      (end-of-line)
      ;; we need to check whether the line ends in a comment and
      ;; skip that one.
      (while (scl-find-regexp 'backward "-" line-begin 'scl-string-p)
        (if (looking-at "//")
            (setq line-end (point))))
      (goto-char line-end)
      (re-search-backward scl-cont-eol-regexp line-begin t))))

(defun scl-first-token-continues-p ()
  "Returns true if the first token on this line is a continuation token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      ;; if first character of the line is inside string, it's a continuation
      ;; if strings aren't supposed to be indented, `scl-calculate-indentation' won't even let
      ;; the control inside this function
      (re-search-forward scl-cont-bol-regexp line-end t))))

(defconst scl-block-starter-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "WHILE" "repeat" "until" "IF" "THEN"
                   "ELSE" "ELSIF" "END_IF" "FOR" "local") t)
     "\\_>\\)")))

(defun scl-first-token-starts-block-p ()
  "Returns true if the first token on this line is a block starter token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (re-search-forward (concat "\\s *" scl-block-starter-regexp) line-end t))))

(defun scl-is-continuing-statement-p (&optional parse-start)
  "Return non-nil if the line continues a statement.
More specifically, return the point in the line that is continued.
The criteria for a continuing statement are:

* the last token of the previous line is a continuing op,
  OR the first token of the current line is a continuing op

"
  (let ((prev-line nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (save-excursion (setq prev-line (scl-forward-line-skip-blanks 'back)))
      (and prev-line
           (not (scl-first-token-starts-block-p))
           (or (scl-first-token-continues-p)
               (and (goto-char prev-line)
                    ;; check last token of previous nonblank line
                    (scl-last-token-continues-p)))))))

(defun scl-make-indentation-info-pair (found-token found-pos)
  "This is a helper function to scl-calculate-indentation-info. Don't
use standalone."
  (cond
   ;; function is a bit tricky to indent right. They can appear in a lot ot
   ;; different contexts. Until I find a shortcut, I'll leave it with a simple
   ;; relative indentation.
   ;; The special cases are for indenting according to the location of the
   ;; function. i.e.:
   ;;       (cons 'absolute (+ (current-column) scl-indent-level))
   ;; TODO: Fix this. It causes really ugly indentations for in-line functions.
   ((string-equal found-token "function")
    (cons 'relative scl-indent-level))

   ;; block openers
   ((member found-token (list "{" "(" "["))
    (save-excursion
      (let ((found-bol (line-beginning-position)))
        (forward-comment (point-max))
        ;; If the next token is on this line and it's not a block opener,
        ;; the next line should align to that token.
        (if (and (zerop (count-lines found-bol (line-beginning-position)))
                 (not (looking-at scl-indentation-modifier-regexp)))
            (cons 'absolute (current-column))
          (cons 'relative scl-indent-level)))))

   ;; These are not really block starters. They should not add to indentation.
   ;; The corresponding "THEN" and "do" handle the indentation.
   ((member found-token (list "IF" "FOR" "WHILE"))
    (cons 'relative 0))
   ;; closing tokens follow: These are usually taken care of by
   ;; scl-calculate-indentation-override.
   ;; elseif is a bit of a hack. It is not handled separately, but it needs to
   ;; nullify a previous then if on the same line.
   ((member found-token (list "until" "ELSIF"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (if (and (scl-goto-matching-block-token found-pos 'backward)
                 (= line (line-number-at-pos)))
            (cons 'remove-matching 0)
          (cons 'relative 0)))))

   ;; else is a special case; if its matching block token is on the same line,
   ;; instead of removing the matching token, it has to replace it, so that
   ;; either the next line will be indented correctly, or the end on the same
   ;; line will remove the effect of the else.
   ((string-equal found-token "ELSE")
     (save-excursion
       (let ((line (line-number-at-pos)))
         (if (and (scl-goto-matching-block-token found-pos 'backward)
                  (= line (line-number-at-pos)))
             (cons 'replace-matching (cons 'relative scl-indent-level))
                   (cons 'relative scl-indent-level)))))

   ;; Block closers. If they are on the same line as their openers, they simply
   ;; eat up the matching indentation modifier. Otherwise, they pull
   ;; indentation back to the matching block opener.
   ((member found-token (list ")" "}" "]" "END_IF"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (scl-goto-matching-block-token found-pos 'backward)
        (if (/= line (line-number-at-pos))
            (scl-calculate-indentation-info (point))
          (cons 'remove-matching 0)))))

   ;; Everything else. This is from the original code: If opening a block
   ;; (match-data 1 exists), then push indentation one level up, if it is
   ;; closing a block, pull it one level down.
   ('other-indentation-modifier
    (cons 'relative (if (nth 2 (match-data))
                        ;; beginning of a block matched
                        scl-indent-level
                      ;; end of a block matched
                      (- scl-indent-level))))))

(defun  scl-add-indentation-info-pair (pair info)
  "Add the given indentation info pair to the list of indentation information.
This function has special case handling for two tokens: remove-matching,
and replace-matching. These two tokens are cleanup tokens that remove or
alter the effect of a previously recorded indentation info.

When a remove-matching token is encountered, the last recorded info, i.e.
the car of the list is removed. This is used to roll-back an indentation of a
block opening statement when it is closed.

When a replace-matching token is seen, the last recorded info is removed,
and the cdr of the replace-matching info is added in its place. This is used
when a middle-of the block (the only case is 'else') is seen on the same line
the block is opened."
  (cond
   ( (eq 'remove-matching (car pair))
     ; Remove head of list
     (cdr info))
   ( (eq 'replace-matching (car pair))
     ; remove head of list, and add the cdr of pair instead
     (cons (cdr pair) (cdr info)))
   ( (listp (cdr-safe pair))
     (nconc pair info))
   ( t
     ; Just add the pair
     (cons pair info))))

(defun scl-calculate-indentation-info-1 (indentation-info bound)
  "Helper function for `scl-calculate-indentation-info'.

Return list of indentation modifiers from point to BOUND."
  (while (scl-find-regexp 'forward scl-indentation-modifier-regexp
                          bound)
    (let ((found-token (match-string 0))
          (found-pos (match-beginning 0))
          (found-end (match-end 0))
          (data (match-data)))
      (setq indentation-info
            (scl-add-indentation-info-pair
             (scl-make-indentation-info-pair found-token found-pos)
             indentation-info))))
  indentation-info)


(defun scl-calculate-indentation-info (&optional parse-end)
  "For each block token on the line, computes how it affects the indentation.
The effect of each token can be either a shift relative to the current
indentation level, or indentation to some absolute column. This information
is collected in a list of indentation info pairs, which denote absolute
and relative each, and the shift/column to indent to."
  (let ((combined-line-end (line-end-position))
        indentation-info)

    (while (scl-is-continuing-statement-p)
      (scl-forward-line-skip-blanks 'back))

    ;; calculate indentation modifiers for the line itself
    (setq indentation-info (list (cons 'absolute (current-indentation))))

    (back-to-indentation)
    (setq indentation-info
          (scl-calculate-indentation-info-1
           indentation-info (min parse-end (line-end-position))))

    ;; and do the following for each continuation line before PARSE-END
    (while (and (eql (forward-line 1) 0)
                (<= (point) parse-end))

      ;; handle continuation lines:
      (if (scl-is-continuing-statement-p)
          ;; if it's the first continuation line, add one level
          (unless (eq (car (car indentation-info)) 'continued-line)
            (push (cons 'continued-line scl-indent-level) indentation-info))

        ;; if it's the first non-continued line, subtract one level
        (when (eq (car (car indentation-info)) 'continued-line)
          (pop indentation-info)))

      ;; add modifiers found in this continuation line
      (setq indentation-info
            (scl-calculate-indentation-info-1
             indentation-info (min parse-end (line-end-position)))))

    indentation-info))


(defun scl-accumulate-indentation-info (info)
  "Accumulates the indentation information previously calculated by
scl-calculate-indentation-info. Returns either the relative indentation
shift, or the absolute column to indent to."
  (let ((info-list (reverse info))
        (type 'relative)
        (accu 0))
    (mapc (lambda (x)
            (setq accu (if (eq 'absolute (car x))
                           (progn (setq type 'absolute)
                                  (cdr x))
                         (+ accu (cdr x)))))
          info-list)
    (cons type accu)))

(defun scl-calculate-indentation-block-modifier (&optional parse-end)
  "Return amount by which this line modifies the indentation.
Beginnings of blocks add scl-indent-level once each, and endings
of blocks subtract scl-indent-level once each. This function is used
to determine how the indentation of the following line relates to this
one."
  (let (indentation-info)
    (save-excursion
      ;; First go back to the line that starts it all
      ;; scl-calculate-indentation-info will scan through the whole thing
      (let ((case-fold-search nil))
        (setq indentation-info
              (scl-accumulate-indentation-info
               (scl-calculate-indentation-info parse-end)))))

    (if (eq (car indentation-info) 'absolute)
        (- (cdr indentation-info) (current-indentation))
      (cdr indentation-info))))


(eval-when-compile
  (defconst scl--function-name-rx
    '(seq symbol-start
          (+ (any alnum "_"))
          (* "." (+ (any alnum "_")))
          (? ":" (+ (any alnum "_")))
          symbol-end)
    "scl function name regexp in `rx'-SEXP format."))


(defconst scl--left-shifter-regexp
  (eval-when-compile
    (rx
     ;; This regexp should answer the following questions:
     ;; 1. is there a left shifter regexp on that line?
     ;; 2. where does block-open token of that left shifter reside?
     (or (seq (group-n 1 symbol-start "local" (+ blank)) "function" symbol-end)

         (seq (group-n 1 (eval scl--function-name-rx) (* blank)) (any "{("))
         (seq (group-n 1 (or
                          ;; assignment statement prefix
                          (seq (* nonl) (not (any "<=>~")) "=" (* blank))
                          ;; return statement prefix
                          (seq word-start "return" word-end (* blank))))
              ;; right hand side
              (or "{"
                  "function"
                  (seq (group-n 1 (eval scl--function-name-rx) (* blank))
                       (any "({")))))))

  "Regular expression that matches left-shifter expression.

Left-shifter expression is defined as follows.  If a block
follows a left-shifter expression, its contents & block-close
token should be indented relative to left-shifter expression
indentation rather then to block-open token.

For example:
   -- 'local a = ' is a left-shifter expression
   -- 'function' is a block-open token
   local a = function()
      -- block contents is indented relative to left-shifter
      foobarbaz()
   -- block-end token is unindented to left-shifter indentation
   end

The following left-shifter expressions are currently handled:
1. local function definition with function block, begin-end
2. function call with arguments block, () or {}
3. assignment/return statement with
   - table constructor block, {}
   - function call arguments block, () or {} block
   - function expression a.k.a. lambda, begin-end block.")


(defun scl-point-is-after-left-shifter-p ()
  "Check if point is right after a left-shifter expression.

See `scl--left-shifter-regexp' for description & example of
left-shifter expression. "
  (save-excursion
    (let ((old-point (point)))
      (back-to-indentation)
      (and
       (/= (point) old-point)
       (looking-at scl--left-shifter-regexp)
       (= old-point (match-end 1))))))



(defun scl-calculate-indentation-override (&optional parse-start)
  "Return overriding indentation amount for special cases.

If there's a sequence of block-close tokens starting at the
beginning of the line, calculate indentation according to the
line containing block-open token for the last block-close token
in the sequence.

If not, return nil."
  (let (case-fold-search token-info block-token-pos)
    (save-excursion
      (if parse-start (goto-char parse-start))

      (back-to-indentation)
      (unless (scl-comment-or-string-p)
        (while
            (and (looking-at scl-indentation-modifier-regexp)
                 (setq token-info (scl-get-block-token-info (match-string 0)))
                 (not (eq 'open (scl-get-token-type token-info))))
          (setq block-token-pos (match-beginning 0))
          (goto-char (match-end 0))
          (skip-syntax-forward " " (line-end-position)))

        (when (scl-goto-matching-block-token block-token-pos 'backward)
          ;; Exception cases: when the start of the line is an assignment,
          ;; go to the start of the assignment instead of the matching item
          (if (scl-point-is-after-left-shifter-p)
              (current-indentation)
            (current-column)))))))

(defun scl-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as scl code."
  (save-excursion
    (let ((continuing-p (scl-is-continuing-statement-p))
          (cur-line-begin-pos (line-beginning-position)))
      (or
       ;; when calculating indentation, do the following:
       ;; 1. check, if the line starts with indentation-modifier (open/close brace)
       ;;    and if it should be indented/unindented in special way
       (scl-calculate-indentation-override)

       (when (scl-forward-line-skip-blanks 'back)
         ;; the order of function calls here is important. block modifier
         ;; call may change the point to another line
         (let* ((modifier
                 (scl-calculate-indentation-block-modifier cur-line-begin-pos)))
           (+ (current-indentation) modifier)))

       ;; 4. if there's no previous line, indentation is 0
       0))))

(defvar scl--beginning-of-defun-re
  (scl-rx-to-string '(: bol (? (symbol "local") ws+) scl-funcheader))
  "scl top level (matches only at the beginning of line) function header regex.")


(defun scl-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a scl proc (or similar).

With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.

Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg (setq arg 1))

  (while (and (> arg 0)
              (re-search-backward scl--beginning-of-defun-re nil t))
    (setq arg (1- arg)))

  (while (and (< arg 0)
              (re-search-forward scl--beginning-of-defun-re nil t))
    (beginning-of-line)
    (setq arg (1+ arg)))

  (zerop arg))

(defun scl-end-of-proc (&optional arg)
  "Move forward to next end of scl proc (or similar).
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of proc.

This function just searches for a `end' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
        (ret t))
    (if (and (< arg 0)
             (not (bolp))
             (save-excursion
               (beginning-of-line)
               (eq (following-char) ?})))
        (forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^end" nil t)
          (setq arg (1- arg)
                found t)
        (setq ret nil
              arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^end" nil t)
          (setq arg (1+ arg)
                found t)
        (setq ret nil
              arg 0)))
    (if found
        (progn
          (beginning-of-line)
          (forward-line)))
    ret))

(defvar scl-process-init-code
  (mapconcat
   'identity
   '("local loadstring = loadstring or load"
     "function sclmode_loadstring(str, displayname, lineoffset)"
     "  if lineoffset > 1 then"
     "    str = string.rep('\\n', lineoffset - 1) .. str"
     "  end"
     ""
     "  local x, e = loadstring(str, '@'..displayname)"
     "  if e then"
     "    error(e)"
     "  end"
     "  return x()"
     "END_IF")
   " "))

(defun scl-make-scl-string (str)
  "Convert string to scl literal."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "[\"'\\\t\\\n]" nil t)
        (cond
	 ((string= (match-string 0) "\n")
	  (replace-match "\\\\n"))
	 ((string= (match-string 0) "\t")
	  (replace-match "\\\\t"))
	 (t
          (replace-match "\\\\\\&" t))))
      (concat "'" (buffer-string) "'"))))

;;;###autoload
(defalias 'run-scl #'scl-start-process)

;;;###autoload
(defun scl-start-process (&optional name program startfile &rest switches)
  "Start a scl process named NAME, running PROGRAM.
PROGRAM defaults to NAME, which defaults to `scl-default-application'.
When called interactively, switch to the process buffer."
  (interactive)
  (or switches
      (setq switches scl-default-command-switches))
  (setq name (or name (if (consp scl-default-application)
                          (car scl-default-application)
                        scl-default-application)))
  (setq program (or program scl-default-application))
  (setq scl-process-buffer (apply 'make-comint name program startfile switches))
  (setq scl-process (get-buffer-process scl-process-buffer))
  (set-process-query-on-exit-flag scl-process nil)
  (with-current-buffer scl-process-buffer
    ;; wait for prompt
    (while (not (scl-prompt-line))
      (accept-process-output (get-buffer-process (current-buffer)))
      (goto-char (point-max)))
    ;; send initialization code
    (scl-send-string scl-process-init-code)

    ;; enable error highlighting in stack traces
    (require 'compile)
    (setq scl--repl-buffer-p t)
    (make-local-variable 'compilation-error-regexp-alist)
    (setq compilation-error-regexp-alist
          (cons (list scl-traceback-line-re 1 2)
                compilation-error-regexp-alist))
    (compilation-shell-minor-mode 1))

  ;; when called interactively, switch to process buffer
  (if (called-interactively-p 'any)
      (switch-to-buffer scl-process-buffer)))

(defun scl-get-create-process ()
  "Return active scl process creating one if necessary."
  (unless (comint-check-proc scl-process-buffer)
    (scl-start-process))
  scl-process)

(defun scl-kill-process ()
  "Kill scl process and its buffer."
  (interactive)
  (when (buffer-live-p scl-process-buffer)
    (kill-buffer scl-process-buffer)
    (setq scl-process-buffer nil)))

(defun scl-set-scl-region-start (&optional arg)
  "Set start of region for use with `scl-send-scl-region'."
  (interactive)
  (set-marker scl-region-start (or arg (point))))

(defun scl-set-scl-region-end (&optional arg)
  "Set end of region for use with `scl-send-scl-region'."
  (interactive)
  (set-marker scl-region-end (or arg (point))))

(defun scl-send-string (str)
  "Send STR plus a newline to the scl process.

If `scl-process' is nil or dead, start a new process first."
  (unless (string-equal (substring str -1) "\n")
    (setq str (concat str "\n")))
  (process-send-string (scl-get-create-process) str))

(defun scl-send-current-line ()
  "Send current line to the scl process, found in `scl-process'.
If `scl-process' is nil or dead, start a new process first."
  (interactive)
  (scl-send-region (line-beginning-position) (line-end-position)))

(defun scl-send-defun (pos)
  "Send the function definition around point to the scl process."
  (interactive "d")
  (save-excursion
    (let ((start (if (save-match-data (looking-at "^function[ \t]"))
                     ;; point already at the start of "function".
                     ;; We need to handle this case explicitly since
                     ;; scl-beginning-of-proc will move to the
                     ;; beginning of the _previous_ function.
                     (point)
                   ;; point is not at the beginning of function, move
                   ;; there and bind start to that position
                   (scl-beginning-of-proc)
                   (point)))
          (end (progn (scl-end-of-proc) (point))))

      ;; make sure point is in a function definition before sending to
      ;; the process
      (if (and (>= pos start) (< pos end))
          (scl-send-region start end)
        (error "Not on a function definition")))))

(defun scl-maybe-skip-shebang-line (start)
  "Skip shebang (#!/path/to/interpreter/) line at beginning of buffer.

Return a position that is after scl-recognized shebang line (1st
character in file must be ?#) if START is at its beginning.
Otherwise, return START."
  (save-restriction
    (widen)
    (if (and (eq start (point-min))
             (eq (char-after start) ?#))
        (save-excursion
          (goto-char start)
          (forward-line)
          (point))
      start)))

(defun scl-send-region (start end)
  (interactive "r")
  (setq start (scl-maybe-skip-shebang-line start))
  (let* ((lineno (line-number-at-pos start))
         (scl-file (or (buffer-file-name) (buffer-name)))
         (region-str (buffer-substring-no-properties start end))
         (command
          ;; Print empty line before executing the code so that the first line
          ;; of output doesn't end up on the same line as current prompt.
          (format "print(''); sclmode_loadstring(%s, %s, %s);\n"
                  (scl-make-scl-string region-str)
                  (scl-make-scl-string scl-file)
                  lineno)))
    (scl-send-string command)
    (when scl-always-show (scl-show-process-buffer))))

(defun scl-prompt-line ()
  (save-excursion
    (save-match-data
      (forward-line 0)
      (if (looking-at comint-prompt-regexp)
          (match-end 0)))))

(defun scl-send-scl-region ()
  "Send preset scl region to scl process."
  (interactive)
  (unless (and scl-region-start scl-region-end)
    (error "scl-region not set"))
  (scl-send-region scl-region-start scl-region-end))

(defalias 'scl-send-proc 'scl-send-defun)

(defun scl-send-buffer ()
  "Send whole buffer to scl process."
  (interactive)
  (scl-send-region (point-min) (point-max)))

(defun scl-restart-with-whole-file ()
  "Restart scl process and send whole file as input."
  (interactive)
  (scl-kill-process)
  (scl-send-buffer))

(defun scl-show-process-buffer ()
  "Make sure `scl-process-buffer' is being displayed.
Create a scl process if one doesn't already exist."
  (interactive)
  (display-buffer (process-buffer (scl-get-create-process))))


(defun scl-hide-process-buffer ()
  "Delete all windows that display `scl-process-buffer'."
  (interactive)
  (when (buffer-live-p scl-process-buffer)
    (delete-windows-on scl-process-buffer)))

(defun scl-funcname-at-point ()
  "Get current Name { '.' Name } sequence."
  ;; FIXME: copying/modifying syntax table for each call may incur a penalty
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?. "_")
    (current-word t)))

(defun scl-search-documentation ()
  "Search scl documentation for the word at the point."
  (interactive)
  (let ((url (concat scl-documentation-url "#pdf-" (scl-funcname-at-point))))
    (funcall scl-documentation-function url)))

(defun scl-toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (let ((num_arg (prefix-numeric-value arg)))
    (setq scl-electric-flag (cond ((or (null arg)
                                       (zerop num_arg)) (not scl-electric-flag))
                                  ((< num_arg 0) nil)
                                  ((> num_arg 0) t))))
  (message "%S" scl-electric-flag))

(defun scl-forward-sexp (&optional count)
  "Forward to block end"
  (interactive "p")
  ;; negative offsets not supported
  (assert (or (not count) (>= count 0)))
  (save-match-data
    (let* ((count (or count 1))
           (block-start (mapcar 'car scl-sexp-alist))
           (block-end (mapcar 'cdr scl-sexp-alist))
           (block-regex (regexp-opt (append  block-start block-end) 'words))
           current-exp)
      (while (> count 0)
        ;; skip whitespace
        (skip-chars-forward " \t\n")
        (if (looking-at (regexp-opt block-start 'words))
            (let ((keyword (match-string 1)))
              (scl-find-matching-token-word keyword 'forward))
          ;; If the current keyword is not a "begin" keyword, then just
          ;; perform the normal forward-sexp.
          (forward-sexp 1))
        (setq count (1- count))))))


;; menu bar

(define-key scl-mode-menu [restart-with-whole-file]
  '("Restart With Whole File" .  scl-restart-with-whole-file))
(define-key scl-mode-menu [kill-process]
  '("Kill Process" . scl-kill-process))

(define-key scl-mode-menu [hide-process-buffer]
  '("Hide Process Buffer" . scl-hide-process-buffer))
(define-key scl-mode-menu [show-process-buffer]
  '("Show Process Buffer" . scl-show-process-buffer))

(define-key scl-mode-menu [end-of-proc]
  '("End Of Proc" . scl-end-of-proc))
(define-key scl-mode-menu [beginning-of-proc]
  '("Beginning Of Proc" . scl-beginning-of-proc))

(define-key scl-mode-menu [send-scl-region]
  '("Send scl-Region" . scl-send-scl-region))
(define-key scl-mode-menu [set-scl-region-end]
  '("Set scl-Region End" . scl-set-scl-region-end))
(define-key scl-mode-menu [set-scl-region-start]
  '("Set scl-Region Start" . scl-set-scl-region-start))

(define-key scl-mode-menu [send-current-line]
  '("Send Current Line" . scl-send-current-line))
(define-key scl-mode-menu [send-region]
  '("Send Region" . scl-send-region))
(define-key scl-mode-menu [send-proc]
  '("Send Proc" . scl-send-proc))
(define-key scl-mode-menu [send-buffer]
  '("Send Buffer" . scl-send-buffer))
(define-key scl-mode-menu [search-documentation]
  '("Search Documentation" . scl-search-documentation))


(provide 'scl-mode)

;;; scl-mode.el ends here
