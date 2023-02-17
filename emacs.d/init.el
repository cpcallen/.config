;;;
;;; First turn off menu and tool bars, to reduce X starup jitter
;;;

;; Disabled since this is now taken care of by customization:
;;
;; ;; Turn off menu and toolbars, blinking cursor
;; (menu-bar-mode -1)
;; (if (>= emacs-major-version 21)
;;     (progn (tool-bar-mode -1)
;; 	   (blink-cursor-mode -1)))
;;
;; ;; Nice big window.
;; (set-frame-height (selected-frame) 40)
;; (set-frame-width (selected-frame) 80)
;; (set-frame-position (selected-frame) 100 0 )

;;;
;;; Local Configuration
;;;

(setq load-path
      (append (list "~/lib/emacs")
              load-path))

(load "~/.site-emacs" 'noerror 'nomessage)
(load "~/.emacs.d/variable-tabs" 'noerror 'nomessage)

;;;
;;; Package Manager (& managed package) initialisation:
;;;

(defvar required-packages
  '(exec-path-from-shell go-mode js2-mode xterm-color)
  "A list of packages needed by cpcallen's init.el")

(defun required-packages-installed-p ()
  "Check to see if all packages in required-packages are installed."
  (cl-loop for p in required-packages
	   when (not (package-installed-p p)) do (cl-return nil)
	   finally (cl-return t)))

(when (>= emacs-major-version 24)
  (require 'package)
  (require 'cl-lib)			; for cl-loop

  ;; MELPA:
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)

  ;; Initialize:
  (package-initialize)

  ;; Update/install:
  (unless (required-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Updating package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p required-packages)
      (when (not (package-installed-p p))
	(package-install p)))))

;;;
;;; General Configuration
;;;

;; Keyboard mappings:
;; Fix ^H to be delete, and use C-x ? for help..  Not great, but it works.
;;(global-set-key "\C-h" 'backward-delete-char-untabify)
;;(global-set-key "\C-x?" 'help-command) ; always useful as backup option

;; Fix up other obvious problems
;;(global-set-key [home] 'beginning-of-line)
;;(global-set-key [end] 'end-of-line)

;; Make X key 'Delete' do what C-d usually does.
;;(global-set-key [delete] 'delete-char)

;; Make f1 key on vt102 get help:
;; (global-set-key [kp-f1] 'help-command)
;; (global-set-key [kp-f2] 'set-mark-command)

;; Other Preferences:
;; (global-set-key "\C-c\t" 'hippie-expand)
(global-set-key "\C-c\C-c" 'compile)
(global-set-key "\M-3" (lambda () (interactive) (insert ?\£))) ; GBP symbol
(global-set-key "\M-6" (lambda () (interactive) (insert ?\§))) ; section symbol
(global-set-key "\M-_" (lambda () (interactive) (insert ?\—))) ; em dash
(global-set-key "\M--" (lambda () (interactive) (insert ?\–))) ; en dash

;; Confirm quit when running in a window:
(when window-system
  (setq confirm-kill-emacs 'yes-or-no-p))

(setq backup-by-copying-when-linked     t
      backup-by-copying-when-mismatch   t
;;      baud-rate                         2400
      ;; For Waterloo, ON:
;;      calendar-latitude                 43.4555555555
;;      calendar-longitude                -80.5436111110
;;      compile-command			"nmake"
;;      default-tab-width			8
;;      inferior-lisp-program 		"clisp"
      kill-read-only-ok			t
;;      lpr-command                       "lpr"
;;      lpr-switches                      '("-Pljp_3016")
      mpuz-silent                       t
;;      prolog-program-name		"pl"
      query-replace-highlight           t
;;      scheme-program-name		"scm"
      vc-follow-symlinks		t
      ;; default to unified diffs
;;      diff-switches "-u"
      )

(setq-default comment-column 80
              adaptive-fill-regexp "[ 	]*\\([-|#;>*]+[ 	]*\\|(?[0-9]+[.)][ 	]*\\)*")

;; Set exec-path to same value used by shell on OS X:
(when (memq window-system '(mac ns))
  (when (require 'exec-path-from-shell nil 'noerror)
    (exec-path-from-shell-initialize)))

;;;
;;; Hook, Major-, and Minor-Mode initialization
;;;

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; BBDB

;; BBDB may or may not be installed.  If it is installed put the
;; following line in ~/.site-emacs:
;; (require 'bbdb)

;; Now, configure it if it is installed:
(if (functionp 'bbdb-initialize)
    (progn (bbdb-initialize)
	   (add-hook 'bbdb-mode-hook 'view-mode)
	   (setq-default bbdb-default-area-code 519
			 bbdb-north-american-phone-numbers-p nil)))

;;; Diary Mode

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'include-other-diary-files)
(add-hook 'mark-diary-entries-hook 'mark-included-diary-files)
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)

; (setq diary-list-include-blanks t)

;;; CPerl Mode

;(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(add-hook 'cperl-mode-hook
	  (function (lambda ()
		      (cperl-toggle-autohelp)
		      (setq cperl-lazy-help-time		 5
			    cperl-tab-always-indent		t
			    comment-column			80
			    cperl-tab-always-indent		t
			    ; cperl-tab-to-comment		t
			    cperl-indent-level			 4
			    cperl-continued-statement-offset	 4
			    cperl-continued-brace-offset	-4
			    cperl-brace-offset			 0
			    cperl-brace-imaginary-offset	 0
			    cperl-label-offset			 0))))

;;; Perl Mode

(add-hook 'perl-mode-hook
	  (function (lambda ()
		      (setq perl-tab-always-indent		t
			    comment-column			80
			    perl-tab-to-comment			t
			    perl-indent-level			 4
			    perl-continued-statment-offset	 4
			    perl-continued-brace-offset		-4
			    perl-brace-offset			 0
			    perl-brace-imaginary-offset		 0
			    perl-label-offset			 0))))

;;; CC Mode

(defconst cpca-style
  '((c-basic-offset . 8)
    (tab-width . 8)
    (comment-column . 80)
    (font-lock-mark-block-function . (function mark-c-function))
    (c-comment-only-line-offset . 0)
    (c-hanging-comment-ender-p . nil)
    (c-offsets-alist . ((statement-block-intro . +)
			(substatement-open . 0)
			(label . 0)
			(statement-cont . +)))))

(defconst cs452-style
  '((c-basic-offset . 4)
    (tab-width . 8)
    (comment-column . 80)
    (font-lock-mark-block-function . (function mark-c-function))
    (c-comment-only-line-offset . 0)
    (c-hanging-comment-ender-p . nil)
    (c-offsets-alist . ((statement-block-intro . +)
			(substatement-open . 0)
			(label . 0)
			(statement-cont . +)))))

(defconst dsl-style
  '("java"
    (c-basic-offset . 2)
    ; (tab-width . 2)
    (c-hanging-comment-ender-p . nil)
    (indent-tabs-mode . nil)))

;(c-enable-//-in-c-mode)
(add-hook 'c-mode-common-hook
	  (function (lambda ()
		      (c-add-style "cpca" cpca-style t)
		      (c-add-style "cs452" cs452-style)
		      (c-add-style "dsl" dsl-style))))

(add-hook 'java-mode-hook
	  (function (lambda ()
		      (c-set-style "dsl"))))

;;; Scheme Mode

(add-hook 'scheme-mode-hook
	  (function (lambda ()
		      (setq comment-column			80))))

;;; Ada Mode

(add-hook 'ada-mode-hook
	  (function (lambda ()
		      (setq comment-column			80
			    ada-auto-case			nil
			    ada-indent				4))))

;;; Tex Mode

(add-hook 'tex-mode-hook
	  (function (lambda ()
		      (setq tex-dvi-view-command		"xdvi"))))

;;; Intercal Mode

;; tell emacs where to find intercal mode.
(autoload 'intercal-mode "intercal")


;;; Go Mode (golang)

(add-hook 'go-mode-hook
          (function (lambda ()
		      ;; Call Gofmt before saving
		      (add-hook 'before-save-hook 'gofmt-before-save)

		      ;; Customize compile command to run go build
		      (if (not (string-match "go" compile-command))
			  (set (make-local-variable 'compile-command)
			       "go build -v && go test -v && go vet"))

		      ;; Use actual tabs, indent by four spaces:
		      (setq tab-width 4)
		      (setq indent-tabs-mode nil)

		      ;; Go can have long lines.  Don't wrap them.
		      (setq truncate-lines t)

		      ;; Godef jump key binding
		      ;; default: (local-set-key (kbd "C-cC-j") 'godef-jump)
		      (local-set-key (kbd "M-*") 'pop-tag-mark))))

;; tell emacs where to find go source
(setenv "GOPATH" "/Users/cpcallen/src/go")

;;; JavaScript Mode

(add-hook 'js-mode-hook
	  (function (lambda ()
		      (setq indent-tabs-mode nil))))

(add-hook 'js-mode-hook #'js2-minor-mode)

;;; Markdown Mode

(add-hook 'markdown-mode-hook
	  (function (lambda ()
		      (setq indent-tabs-mode nil))))

;;; Compile Mode

;; Add NodeJS error formats for stack traces & SyntaxError; see
;; https://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/
(defun my-recognise-node-errors ()
  (add-to-list 'compilation-error-regexp-alist-alist
	       '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
		      1 ;; file
		      2 ;; line
		      3 ;; column
		      ))
  (add-to-list 'compilation-error-regexp-alist-alist
	       '(node-syn "^\\(/[^:]+\\):\\([0-9]+\\)\)?$"
			  1 ;; file
			  2 ;; line
			  ))
  (add-to-list 'compilation-error-regexp-alist 'node)
  (add-to-list 'compilation-error-regexp-alist 'node-syn))
(add-hook 'compilation-mode-hook 'my-recognise-node-errors)

;; Handle ANSI colour escape sequences.

;; Version from https://stackoverflow.com/questions/13397737
;; seems to fail with error "Marker does not point anywhere".
;;
;; (when (require 'ansi-color nil t)
;;   (defun my/colourise-compile-buffer ()
;;     (ansi-color-apply-on-region compilation-filter-start (point)))
;;   (add-hook 'compilation-mode-hook 'my/colourise-compile-buffer))

;; Version from https://stackoverflow.com/questions/3072648#63710493
(when (require 'xterm-color nil t)
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;;; Font Lock Mode

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t
      lazy-lock-defer-time 0.75)

;(setq font-lock-face-attributes
;      '((font-lock-comment-face nil nil nil t nil)
;        (font-lock-string-face nil nil nil nil t)
;        (font-lock-keyword-face nil nil t nil nil)
;        (font-lock-function-name-face "white" "black" t nil nil)
;        (font-lock-variable-name-face nil nil t t nil)
;        (font-lock-type-face nil nil t nil t)
;        (font-lock-reference-face nil nil t nil t)))

;;; Line Number Mode
(line-number-mode t)

;;; Show Paren Mode
;; Fun, but not *that* much fun.
;; (show-paren-mode)

;;; Auto Mode Alist:

(setq auto-mode-alist
      (append '(;; Temporary files belonging to various mailers, etc.:
		("^/tmp/mail\." . text-mode)
                ("^/tmp/snd\\.[0-9]" . text-mode)
                ("^/tmp/mutt" . text-mode)
                ("^/tmp/post.[0-9]" . text-mode)
		("^/tmp/bug" . text-mode)
		("^/tmp/reportbug" . text-mode)
		("^/tmp/cvs" . text-mode)
		("/\.w3m/w3mtmp" . text-mode)
		("^/tmp/SLRN" . text-mode)

		;; Various source code extensions:
		("\\.st$" . smalltalk-mode)
		;; ("\\.pl$" . perl-mode)
                ("\\.S$" . asm-mode)
		("\\.m$" . indented-text-mode)
		("\\.i$" . intercal-mode)
                ("\\.[2-7]i$" . intercal-mode)
		)
	      auto-mode-alist))


;;;
;;; End Of Mode-Specific Configuration
;;;

(when window-system	       ; Only start servers if running under X
  (if (fboundp 'gnuserv-start)
      (gnuserv-start))
  (server-start))

;; Save/restore emacs state.
(load "desktop")

;;; Mouse wheel support.
(if (< emacs-major-version 21)
    (progn
      (defun up-slightly () (interactive) (scroll-up 5))
      (defun down-slightly () (interactive) (scroll-down 5))
      (global-set-key [mouse-4] 'down-slightly)
      (global-set-key [mouse-5] 'up-slightly)))

; (defun up-one () (interactive) (scroll-up 1))
; (defun down-one () (interactive) (scroll-down 1))
; (global-set-key [S-mouse-4] 'down-one)
; (global-set-key [S-mouse-5] 'up-one)


; (defun up-a-lot () (interactive) (scroll-up))
; (defun down-a-lot () (interactive) (scroll-down))
; (global-set-key [C-mouse-4] 'down-a-lot)
; (global-set-key [C-mouse-5] 'up-a-lot)


;;;
;;; Customized / auto-added settings
;;;

(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bbdb-csv-export-type 'outlook)
 '(bbdb-vcard-export-dir "~/")
 '(desktop-restore-in-current-display nil)
 '(desktop-restore-reuses-frames nil)
 '(desktop-save-mode t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((width . 132) (height . 50) (top . 100) (left . 300)))
 '(js-expr-indent-offset 2)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(kill-read-only-ok t)
 '(line-move-visual nil)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(package-selected-packages '(exec-path-from-shell go-mode js2-mode markdown-mode typo))
 '(scroll-bar-mode 'left)
 '(search-whitespace-regexp nil)
 '(tex-default-mode 'latex-mode)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "black" :foreground "white" :box (:line-width -1 :style released-button))))))
