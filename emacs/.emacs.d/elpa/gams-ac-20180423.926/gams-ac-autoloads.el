;;; gams-ac-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gams-ac" "gams-ac.el" (0 0 0 0))
;;; Generated autoloads from gams-ac.el

(autoload 'gams-ac-setup "gams-ac" "\
Set up `auto-complete' for GAMS mode.

\(fn)" nil nil)

(autoload 'gams-ac-after-init-setup "gams-ac" "\
A function that should be executed in the init file.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gams-ac" '("gams-ac-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gams-ac-autoloads.el ends here
