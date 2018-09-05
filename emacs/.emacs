;; Packages
(setq default-directory "~")
(add-to-list 'load-path "~/.emacs.d/lisp")

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(package-initialize)

;; coding system utf-8
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburnMod)))
 '(custom-safe-themes
   (quote
    ("662fafdb5c320c7efdd4828c08ea59af3dae9d0d42d2c2e45b4a5d990296c4be" "5adf37164c02e899698a36dd1887a9584d816bc99face38621514aecdc9c4eef" "afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" default)))
 '(fci-rule-color "#383838")
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(matlab-comment-column 8)
 '(matlab-fill-fudge-hard-maximum 80)
 '(matlab-indent-function-body nil)
 '(matlab-return-add-semicolon t)
 '(matlab-show-mlint-warnings t)
 '(matlab-show-periodic-code-details-flag nil)
 '(menu-bar-mode nil)
 '(minimap-hide-fringes t)
 '(minimap-highlight-line nil)
 '(minimap-window-location (quote right))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (xresources-theme company-lua diffview matlab-mode ace-mc sr-speedbar 2048-game smex hlinum tabbar ace-jump-mode multiple-cursors yascroll idomenu fill-column-indicator ws-butler undo-tree zenburn-theme yasnippet auctex)))
 '(reftex-toc-split-windows-fraction 0.21)
 '(reftex-toc-split-windows-horizontally t)
 '(scroll-bar-mode nil)
 '(size-indication-mode nil)
 '(tabbar-background-color "black")
 '(tabbar-separator (quote (0.2)))
 '(tool-bar-mode nil))


;; Window options
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 85))
(add-to-list 'default-frame-alist '(top . 0))

;; Font size
(set-face-attribute 'default nil :height 110)

;; Alpha
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 85))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 85)))
 ;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
 ;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; (defun incrementAlpha ()
;;   (interactive)
;;   (let ((alpha (frame-parameter nil 'alpha)))
;;     (if (and alpha (< alpha 100)) ;; check for alpha not nil
;;         (set-frame-parameter (selected-frame) 'alpha (+ alpha 5))
;;       (set-frame-parameter (selected-frame) 'alpha 100))))

;; (defun decrementAlpha ()
;;   (interactive)
;;   (let ((alpha (frame-parameter nil 'alpha)))
;;     (if (and alpha (> alpha 0)) ;; check for alpha not nil
;;         (set-frame-parameter (selected-frame) 'alpha (- alpha 5))
;;       (set-frame-parameter (selected-frame) 'alpha 0))))

(global-set-key (kbd "<nil> <wheel-down>") 'decrementAlpha)
(global-set-key (kbd "<nil> <wheel-up>") 'incrementAlpha)

;;; Coding system
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;;; Scratch buffer
(setq initial-scratch-message nil)

;;; Backup files
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t       ; backup of a file the first time it is saved.
      backup-by-copying t       ; don't clobber symlinks
      version-control t         ; version numbers for backup files
      delete-old-versions t     ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6       ; oldest versions to keep when a new numbered
                                        ; backup is made (default: 2)
      kept-new-versions 9       ; newest versions to keep when a new numbered
                                        ; backup is made (default: 2)
      auto-save-default t       ; auto-save every buffer that visits a file
      auto-save-timeout 20      ; number of seconds idle time before auto-save
                                        ; (default: 30)
      auto-save-interval 200    ; number of keystrokes between auto-saves
                                        ; (default: 300)
      vc-make-backup-files t    ; backup versioned files
      )

;; CUA bindings
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t)

;; Key bindings
(setq mac-command-modifier 'control)
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))
;; (global-unset-key (kbd "<prior>"))
;; (global-unset-key (kbd "<next>"))
(global-unset-key (kbd "C-b"))
(global-set-key (kbd "C-r") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-o")  'mode-line-other-buffer)
(global-set-key (kbd "M-S-<backspace>") 'kill-whole-line)
(global-set-key (kbd "M-q") 'fill-paragraph)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'write-file)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-yank-kill)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(global-set-key (kbd "C-M-w") 'ispell-word)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-m") 'iy-go-to-char)
(global-set-key (kbd "M-C-m") 'iy-go-to-char-backward)
(global-set-key (kbd "C--") 'idomenu)
(global-set-key (kbd "C-S-o") 'recentf-ido-find-file)
(global-set-key (kbd "M-n") 'forward-list) ;;Hitta klamrar, brackets etc.
(global-set-key (kbd "M-p") 'backward-list)
(global-set-key (kbd "M-t") 'transpose-words)
(global-set-key (kbd "<f10>") 'linum-mode)
(global-set-key (kbd "C-<f10>") 'global-linum-mode)
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key [f12] 'menu-bar-mode)
(global-set-key (kbd "<f5>") (lambda ()
			       (interactive)
			       (setq-local compilation-read-command nil)
			       (call-interactively 'compile)))
(global-set-key (kbd "<prior> ") 'move-beginning-of-line)
(global-set-key (kbd "<next> ") 'move-end-of-line)
(global-set-key (kbd "C-<up>") 'backward-sentence)
(global-set-key (kbd "C-<down>") 'forward-sentence)
(global-set-key (kbd "M-<up>") 'scroll-down-command)
(global-set-key (kbd "M-<down>") 'scroll-up-command)
(global-set-key (kbd "C-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-t") 'split-window-right)
(global-set-key (kbd "C-<next>") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-<prior>") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-w") 'delete-window)
(global-set-key (kbd "C-,") 'ace-jump-mode)
(global-set-key (kbd "C-.") 'ace-jump-char-mode)
(global-set-key (kbd "C-/") 'ace-jump-line-mode)
(global-set-key (kbd "C-'") 'ace-mc-add-multiple-cursors)
(global-set-key (kbd "M-a") 'align)
(global-set-key (kbd "C-e") 'next-error)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "M-+") 'text-scale-decrease)
(global-set-key (kbd "C-SPC") 'cua-set-mark)
(global-set-key (kbd "C-b j") 'pop-to-mark-command)
(global-set-key (kbd "M-b") 'ibuffer)
(global-set-key (kbd "C-<down>")  (lambda () (interactive) (scroll-up 5)) )
(global-set-key (kbd "C-<up>")  (lambda () (interactive) (scroll-down 5)) )
(global-set-key (kbd "C-M-r") 'isearch-forward-regexp)


(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; (global-set-key (kbd "M-v p") 'highlight-phrase)
;; (global-set-key (kbd "M-v r") 'highlight-regexp)
;; (global-set-key (kbd "M-v u") 'unhighlight-regexp)


;; xresources (to read from pywal output)
;; (require 'xresources-theme)

;; Speed bar
(require 'sr-speedbar)
(setq speedbar-show-unknown-files t ; show all files
      speedbar-use-images nil ; use text for buttons
      sr-speedbar-right-side nil ; put on left side
      sr-speedbar-width 35)
(global-set-key [f9] (lambda()
		       (interactive)
		       (sr-speedbar-toggle)
		       (visual-line-mode)
		       (if (eq (cdr (assoc 'fullscreen (frame-parameters))) 'nil)
			   (if (string= (buffer-name) "*SPEEDBAR*") (set-frame-width (selected-frame)
			 135) (set-frame-width (selected-frame) 100)) )
		       (other-window 1)
		       ))
(set-face-foreground 'speedbar-directory-face "#F0DFAF")
(set-face-foreground 'speedbar-file-face "#DCDCCC")
(set-face-foreground 'speedbar-button-face "#7F9F7F")
(set-face-foreground 'speedbar-highlight-face "#000000")
(set-face-background 'speedbar-highlight-face "#94BFF3")
(set-face-foreground 'speedbar-selected-face "#CC9393")

(sr-speedbar-open)
(with-current-buffer sr-speedbar-buffer-name
  (setq window-size-fixed 'width))
(sr-speedbar-close)

;; Tab bar
(tabbar-mode 1)
(setq tabbar-use-images nil)
(dolist (func '(tabbar-mode tabbar-forward-tab
			    tabbar-forward-group
			    tabbar-backward-tab
			    tabbar-backward-group))
  (autoload func "tabbar" "Tabs at the top of buffers and easy
  control-tab navigation"))
(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  `(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
	 ,on-no-prefix
       ,on-prefix)))
(defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group)
  (tabbar-mode 1))
(defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group)
  (tabbar-mode 1))
(global-set-key (kbd "C-<right>") 'shk-tabbar-next)
(global-set-key (kbd "C-<left>") 'shk-tabbar-prev)
(global-set-key (kbd "C-S-<right>") 'tabbar-forward-group)
(global-set-key (kbd "C-S-<left>") 'tabbar-backward-group)

;; (global-set-key (kbd "C-<next>") 'shk-tabbar-next)
;; (global-set-key (kbd "C-<prior>") 'shk-tabbar-prev)
;; (global-set-key (kbd "C-S-<next>") 'tabbar-forward-group)
;; (global-set-key (kbd "C-S-<prior>") 'tabbar-backward-group)

(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
		    (format "[%s]  " (tabbar-tab-tabset tab))
		  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
	label
      (tabbar-shorten
       label (max 1 (/ (window-width)
		       (length (tabbar-view
                        (tabbar-current-tabset)))))))))

;;move tabbar
(defun move-current-tab-to-top ()
    "Move current tab to top."
    (interactive)
    (let* ((bufset (tabbar-current-tabset t))
           (bufs (tabbar-tabs bufset))
           (car-bufs (list))
           (cdr-bufs (list)))
      (dolist (buf bufs)
        (if (string= (buffer-name) (format "%s" (car buf)))
            (add-to-list 'car-bufs buf)
          (add-to-list 'cdr-bufs buf)))
      (setq cdr-bufs (reverse cdr-bufs))
      (set bufset (append car-bufs cdr-bufs))
      (tabbar-set-template bufset nil)
      (tabbar-display-update)))

;; (defun move-current-tab-to-top ()
;;     "Move current tab to top."
;;     (interactive)
;;     (let* ((bufset (tabbar-current-tabset t))
;;            (bufs (tabbar-tabs bufset))
;;            (newbufset (list)))
;;       (dolist (buf bufs)
;;         (if (string= (buffer-name) (format "%s" (car buf)))
;;             (message (buffer-name))
;;             (add-to-list 'car-bufs buf)
;;           (add-to-list 'cdr-bufs buf)))
;;       (setq cdr-bufs (reverse cdr-bufs))
;;       (set bufset (append car-bufs cdr-bufs))
;;       (tabbar-set-template bufset nil)
;;       (tabbar-display-update)))

(global-set-key (kbd "C-S-<next>") 'move-current-tab-to-top)

;;tabbar appearence
(set-face-attribute
 'tabbar-default nil
 :background "black"
 :foreground "#F0DFAF"
 :box '(:line-width 1 :color "black" :style nil))

(set-face-attribute
 'tabbar-unselected nil
 :background "black"
 :foreground "#426fbc"
 :weight 'normal
 :box '(:line-width 1 :color "black" :style nil))

(set-face-attribute
 'tabbar-selected nil
 :background "black"
 :foreground "#699aef"
 :weight 'bold
 :box '(:line-width 1 :color "black" :style nil))

(set-face-attribute
 'tabbar-selected-modified nil
 :background "black"
 :foreground "#ef6969"
 :weight 'bold
 :box '(:line-width 1 :color "black" :style nil))

(set-face-attribute
 'tabbar-modified nil
 :background "black"
 :foreground "#bc4242"
 :weight 'normal
 :box '(:line-width 1 :color "black" :style nil))

(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))

(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)





;; Latex view
;; (custom-set-variables
;; ;; default warning comments from the customize-variable command
;;   '(TeX-output-view-style (quote (("^pdf$" "." "Document Viewer --fullscreen %o")
;;    )
;;   '(TeX-view-program-selection (quote ((output-pdf "Document Viewer"))))
;; )))

;; RefTeX
(add-hook 'LaTeX-mode-hook 'reftex-mode)

;; LaTeX Preview Pane
;; (latex-preview-pane-enable)
;; (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq-local fill-column 120)
            (setq-local fci-rule-column fill-column)))

;; Autofill
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

;; Custom Macros
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (TeX-add-symbols "lstinputlisting")
	    (TeX-add-symbols "section")
	    (LaTeX-add-environments "bar")))

;; ispell in latex
(setq ispell-tex-skip-alists
      (list
       (append
    (car ispell-tex-skip-alists)
    '(
      ("\\\\autoref" ispell-tex-arg-end)
      ("\\\\eqref" ispell-tex-arg-end)
      ("\\\\si" ispell-tex-arg-end)
      ("\\\\citep" ispell-tex-arg-end)
      ("\\\\begin{tikzpicture}" "\\\\end{tikzpicture}")
      ("\\$" "\\$") ; inline math
      ("\\\\\\[" "\\]") ; line space (conflict with display math below)
      ("\\\\\\[" "\\\\\\]") ; display math
		    ;("_{" ispell-tex-arg-end) ; math indexes
      ("\\\\bibitem{" "\\\\emph{") ; Authors names
		    ; Temporary

      ))
       (cadr ispell-tex-skip-alists)))


(defun waqar-LaTeX-hook ()
  (local-set-key (kbd "<f5>") (lambda ()
		(interactive)
		(TeX-save-document (TeX-master-file))
		(TeX-command "LaTeX" 'TeX-master-file -1)))
  (local-set-key [f9] (lambda ()
	    (interactive)
	    (if (get-buffer "*toc*")
		(progn
		  (switch-to-buffer-other-window "*toc*")
		    ;(set-window-dedicated-p (selected-window) nil)
		  (if (eq (cdr (assoc 'fullscreen (frame-parameters))) 'nil)
		    (if (string= (buffer-name) "*toc*") (set-frame-width (selected-frame)
		    121)))
		  (reftex-toc-quit-and-kill))
	      (progn
		(reftex-toc)
		(visual-line-mode)
		    ;(set-window-dedicated-p (selected-window) t)
		(tabbar-local-mode 1)
		(if (eq (cdr (assoc 'fullscreen (frame-parameters))) 'nil)
		    (if (string= (buffer-name) "*toc*") (set-frame-width (selected-frame)
		    155)))
		(other-window 1)))))
  )
(add-hook 'LaTeX-mode-hook 'waqar-LaTeX-hook)

;;Tabs are evil
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Smooth mouse
(setq mouse-wheel-scroll-amount '(0.001))

;; Delete selection mode
(delete-selection-mode 1)

;; ;; Word wrap
;; (global-visual-line-mode t)

;; Undo-Tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)

;; ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)
(add-hook 'LaTeX-mode-hook 'ws-butler-mode)

;; Fill Column Indicator
(column-number-mode t)
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq fci-handle-truncate-lines nil)
(setq fci-rule-width 1)
(setq fci-rule-color "#1e3256")
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'nxml-mode-hook 'fci-mode)
(add-hook 'LaTeX-mode-hook 'fci-mode)


;; ido-menu
(require 'ido)
(ido-mode t)
(setq ido-max-prospects 50)
;;(setq ido-confirm-unique-completion t)
(setq confirm-nonexistent-file-or-buffer nil)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Recently opened files: " recentf-list nil t)))
    (when file
      (find-file file))))

;; ido-vertical-mode
(setq ido-decorations (quote ("\n-> " "" "\n " "\n ..." "[" "]" "
[No match]" " [Matched]" " [Not readable]" " [Too big]" "
[Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable
'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
    (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
    (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))
  (add-hook 'ido-setup-hook 'ido-define-keys)

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 20)

;; Multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-<") 'mc/mark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-<") 'mc/mark-all-like-this)
;; (define-key mc/keymap (kbd "<next>")
;;   (lambda () (interactive)
;;     (move-end-of-line nil)
;;     (next-line)
;;     (left-char)))

;; Highlight current line
(require 'hlinum)
(hlinum-activate)

;; smex
(global-set-key (kbd "M-x") 'smex)

;; YASnippet
(require 'yasnippet)
(yas-global-mode 1)
(add-hook 'term-mode-hook (lambda()
			    (yas-minor-mode -1)))

;; Make Message buffer always scroll to the bottom
(defadvice message (after message-tail activate)
  "goto point max after a message"
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (walk-windows (lambda (window)
		    (if (string-equal (buffer-name (window-buffer window)) "*Messages*")
			(set-window-point window (point-max))))
		  nil
		  t)))


;; Paranethes
(require 'paren)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#a60000")
(set-face-background 'show-paren-mismatch "#a60000")
(set-face-foreground 'show-paren-mismatch (face-background 'default))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
		echo area. Has no effect if the character before point is not of
		the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
	 (matching-text (and cb
			     (char-equal (char-syntax cb) ?\) )
			     (blink-matching-open))))
    (when matching-text (message matching-text))))

;; Diff
(setq ediff-split-window-function 'split-window-horizontally)


;; Mode line
(require 'uniquify)

;; Show buffer file name
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name) ; Or any other key you want



;;; C++
(require 'cc-mode)
(setq c-default-style "java")
                                        ;(define-key c-mode-base-map
                                        ;(kbd "RET")
                                        ;'newline-and-indent)
(c-set-offset 'access-label -4)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.qss\\'" . c++-mode))
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "M-O") 'ff-find-other-file)))



                                        ; Syntax highlighting
(add-hook 'c++-mode-hook
          '(lambda()
             (font-lock-add-keywords
              nil '(
                    ;; add the new C++11 keywords
                    ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|
noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>"
                     . font-lock-keyword-face)
                    ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
                    ;; PREPROCESSOR_CONSTANT
                                        ;("\\<[A-Z]+[A-Z_]+\\>"
                                        ;. font-lock-constant-face)
                    ;; hexadecimal numbers
                    ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
                    ;; integer/float/scientific numbers
             ;("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>"
                                        ;. font-lock-constant-face)
                    ;; user-types (customize!)
                    ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>"
                     . font-lock-type-face)
                    ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
                    ))
             ) t)



;;; Desktop save
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name ".emacs.desktop")

(setq desktop-buffers-not-to-save
      (concat "\\("
              "\\.log\\|^tags\\|^TAGS\\|\\.emacs.*"
              "\\)"))

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
          '(lambda ()
             ;; desktop-remove clears desktop-dirname§
             (setq desktop-dirname-tmp desktop-dirname)
             (desktop-remove)
             (setq desktop-dirname desktop-dirname-tmp)))

(defun has-saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (has-saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (has-saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save desktop-dirname t t)
        (message "Session not saved."))
    (desktop-save desktop-dirname t t)))

;; save desktop when exiting
(global-set-key (kbd "C-S-q") (lambda ()
                                   (interactive)
                                   (session-save)
                                   (save-buffers-kill-emacs)
                                   ))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
          '(lambda ()
             (if (has-saved-session)
                 (if (y-or-n-p "Restore desktop? ")
                     (session-restore)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:background "#510b0b"))))
 '(tabbar-separator ((t (:inherit tabbar-default)))))

;;qt pro mode
(require 'qt-pro-mode)
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

;;; Auto-complete
(require 'ac-c-headers)
(require 'auto-complete-c-headers)
(add-hook 'c++-mode-hook
          (lambda()
            (add-to-list 'ac-sources 'ac-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols)))
(add-hook 'c-mode-hook
          (lambda()
            (add-to-list 'ac-sources 'ac-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols)))


;;; Org mode
(require 'org)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(cua-mode 1)
(setq org-support-shift-select t)
;; (setq org-clock-continuously t)
(eval-after-load "org"
  '(progn
     (eval-after-load "cua-base"
       '(progn
          (defadvice org-call-for-shift-select (before org-call-for-shift-select-cua activate)
            (if (and cua-mode
                     org-support-shift-select
                     (not (use-region-p)))
                (cua-set-mark)))))))

;; Unbind key
(define-key org-mode-map (kbd "C-S-<left>") nil)
(define-key org-mode-map (kbd "C-S-<right>") nil)
(define-key org-mode-map (kbd "M-<up>") nil)
(define-key org-mode-map (kbd "M-<down>") nil)
(define-key org-mode-map (kbd "C-,") nil)
(define-key org-mode-map (kbd "C-'") nil)

;TODO: fixa key bindings
;; C-c C-n     (org-next-visible-heading)
;; Next heading.
;; C-c C-p     (org-previous-visible-heading)
;; Previous heading.
;; C-c C-f     (org-forward-same-level)
;; Next heading same level.
;; C-c C-b     (org-backward-same-level)
;; Previous heading same level.
;; C-c C-u     (outline-up-heading)
;; Backward to higher level heading.
;; C-c C-j     (org-goto)


;;; Duplicate words
(defun show-unique-words (&optional alphabetical)
  "Collect all of the unique words in the current buffer and
display them in a new buffer.  With prefix, alphabetize the
list."
  (interactive "P")
  (let ((buf (buffer-name))
        (new (get-buffer-create "*Unique Words*"))
        (txt (delete-dups (mapcar #'downcase
                                  (split-string (buffer-string)
                                                nil nil
                                                "[^[:alnum:]]+")))))
    (with-current-buffer new
      (delete-region (point-min) (point-max))
      (insert (format "%d unique words in the <%s> buffer:\n\n"
                      (length txt) buf))
      (cl-dolist (word (if alphabetical (sort txt #'string<) txt))
        (insert (concat word "\n"))))
    (pop-to-buffer new)))


;;; Python
;; Jedi
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
