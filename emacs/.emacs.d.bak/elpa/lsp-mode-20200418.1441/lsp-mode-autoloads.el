;;; lsp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "lsp-cpp" "lsp-cpp.el" (24221 26217 589613
;;;;;;  873000))
;;; Generated autoloads from lsp-cpp.el

(autoload 'lsp-cpp-flycheck-clang-tidy-error-explainer "lsp-cpp" "\
Explain a clang-tidy ERROR by scraping documentation from llvm.org.

\(fn ERROR)" nil nil)

;;;***

;;;### (autoloads nil "lsp-fsharp" "lsp-fsharp.el" (24221 26217 625613
;;;;;;  676000))
;;; Generated autoloads from lsp-fsharp.el

(autoload 'lsp-fsharp--workspace-load "lsp-fsharp" "\
Load all of the provided PROJECTS.

\(fn PROJECTS)" nil nil)

;;;***

;;;### (autoloads nil "lsp-mode" "lsp-mode.el" (24221 26217 629613
;;;;;;  654000))
;;; Generated autoloads from lsp-mode.el
(put 'lsp-enable-file-watchers 'safe-local-variable #'booleanp)
(put 'lsp-file-watch-threshold 'safe-local-variable (lambda (i) (or (numberp i) (not i))))

(autoload 'lsp "lsp-mode" "\
Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start.

\(fn &optional ARG)" t nil)

(autoload 'lsp-deferred "lsp-mode" "\
Entry point that defers server startup until buffer is visible.
`lsp-deferred' will wait until the buffer is visible before invoking `lsp'.
This avoids overloading the server with many files when starting Emacs.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("lsp-clients.el" "lsp-clojure.el" "lsp-csharp.el"
;;;;;;  "lsp-css.el" "lsp-elm.el" "lsp-erlang.el" "lsp-eslint.el"
;;;;;;  "lsp-gdscript.el" "lsp-go.el" "lsp-haxe.el" "lsp-html.el"
;;;;;;  "lsp-intelephense.el" "lsp-json.el" "lsp-metals.el" "lsp-mode-pkg.el"
;;;;;;  "lsp-perl.el" "lsp-pwsh.el" "lsp-pyls.el" "lsp-rust.el" "lsp-solargraph.el"
;;;;;;  "lsp-terraform.el" "lsp-verilog.el" "lsp-vetur.el" "lsp-vhdl.el"
;;;;;;  "lsp-xml.el" "lsp-yaml.el" "lsp.el") (24221 26217 649613
;;;;;;  544000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lsp-mode-autoloads.el ends here
