;;; side-notes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "side-notes" "side-notes.el" (0 0 0 0))
;;; Generated autoloads from side-notes.el

(autoload 'side-notes-toggle-notes "side-notes" "\
Pop up a side window containing `side-notes-file'.

When prefixed with...

  1. \\[universal-argument], locate `side-notes-secondary-file' instead.
  2. \\[universal-argument] \\[universal-argument], force visiting `side-notes-file' within current directory.
  3. \\[universal-argument] \\[universal-argument] \\[universal-argument], force visiting `side-notes-secondary-file' within
     current directory.

See `side-notes-display-alist' for options concerning displaying
the notes buffer.

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "side-notes" '("side-notes-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; side-notes-autoloads.el ends here
