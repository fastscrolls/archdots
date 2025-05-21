;;; config.el -*- lexical-binding: t; coding: utf-8; -*-

(setq-default load-prefer-newer t)

;; a hack
(add-function :after after-focus-change-function
              (defun garbage-collect-maybe ()
                (unless (frame-focus-state)
                  (garbage-collect))))
;; or just (add-hook 'focus-out-hook #'garbage-collect)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(straight-use-package '(use-package :type built-in))

(let ((local-file (expand-file-name "local.el" user-emacs-directory))) 
(when (file-exists-p local-file) (load local-file 'noerror)))

(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

(set-charset-priority 'unicode)
(set-language-environment 'UTF-8)
(set-default-coding-systems 'utf-8)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(straight-use-package '(bind-key :type built-in))
(straight-use-package '(use-package :type built-in))

(use-package auto-compile
  :straight t
  :defer nil
  :config (auto-compile-on-load-mode))

(use-package font-lock+
  :load-path "lisp")

(font-lock-add-keywords nil '(("\t" . 'extra-whitespace-face)))

(setq-default font-lock-maximum-decoration t)
(global-font-lock-mode t)

(set-face-font 'default  (font-spec :family "DejaVu Sans Mono" :weight 'light :size 16 :height 158))

(set-face-font 'fixed-pitch  (font-spec :family "DejaVu Sans Mono" :weight 'light :size 16 :height 158))
(set-face-font 'fixed-pitch-serif (font-spec :family "DejaVu Serif" :weight 'light :size 16 :height 158))
(set-face-font 'variable-pitch (font-spec :family "DejaVu Sans" :weight 'light :size 16 :height 158))

(set-face-attribute 'font-lock-constant-face nil :weight 'normal)
(set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-comment-face nil :italic t)
(set-face-attribute 'font-lock-doc-face nil :italic t)

(use-package mixed-pitch
  :straight t
  :hook ((text-mode . mixed-pitch-mode)
         (help-mode . mixed-pitch-mode)
         (org-mode . mixed-pitch-mode)
         (html-mode . mixed-pitch-mode)
         (latex-mode . mixed-pitch-mode)
         (markdown-mode . mixed-pitch-mode)
         (gfm-mode . mixed-pitch-mode)
         (nov-mode . mixed-pitch-mode)
         (info-mode . mixed-pitch-mode))
  :config
  (variable-pitch-mode t))


(defvar mixed-pitch-modes '(text-mode help-mode org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")

(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the m
odes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'after-init-hook #'init-mixed-pitch-h)

(defvar mixed-pitch-modes '(text-mode help-mode org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")

(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the m
odes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'after-init-hook #'init-mixed-pitch-h)

(defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)

(defcustom variable-pitch-serif-font (font-spec :family "DejaVu Serif")
  "The font face used for `variable-pitch-serif'."
  :group 'basic-faces
  :set (lambda (symbol value)
	   (set-face-attribute 'variable-pitch-serif nil :font value)
	   (set-default-toplevel-value symbol value)))


(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)
(global-set-key (kbd "RET") 'newline-and-indent)

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq inhibit-splash-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(setq-default font-use-system-font t)
(setq-default font-lock-maximum-decoration t)

(setq sentence-end-double-space nil)

(setq-default word-wrap t)

(setq-default truncate-lines nil)

;; lets be explicit and re-enable everythig important
(add-hook 'prog-mode-hook (lambda ()
    		            (setq show-trailing-whitespace t)
    		            (setq indicate-empty-lines t)

                    (setq tab-width 4)
                    (setq fill-column 72)
                    (set-fill-column 72)
                    (auto-fill-mode t)))

(setq-default tab-width 4)
(setq-default fill-column 72)
(set-fill-column 72)
(auto-fill-mode t)
(setopt display-fill-column-indicator-column 72)
(setq-default display-fill-column-indicator-column 72)
(global-highlight-changes-mode 1)

(global-display-fill-column-indicator-mode 1)

(global-visual-line-mode t)
(global-hl-line-mode t)

(global-subword-mode t)

(show-paren-mode t)
(transient-mark-mode t)

(setq-default electric-indent-chars '(?\n ?\^?))
(electric-pair-mode t)
(electric-indent-mode t)

(delete-selection-mode t)

(abbrev-mode t)
(setq save-abbrevs 'silently)
(bind-key "M-/" 'hippie-expand)

  (setq whitespace-style '(face spaces tabs newline space-mark tab-mark))
  (global-whitespace-mode t)

  ;; this is a nice subtle hack
  (add-hook 'diff-mode-hook 'whitespace-mode)

(use-package whitespace-cleanup-mode
  :config (whitespace-cleanup-mode t))


(use-package emacs
:custom
(auto-save-default t)
(make-backup-files -1)
(backup-by-copying t)
(version-control t)
(vc-make-backup-files t)
(delete-old-versions -1)
(create-lockfiles t)
(auto-save-visited-mode t)
:config
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(auto-save-visited-mode t)
(add-hook 'focus-out-hook #'save-all))

(use-package beacon
  :config (beacon-mode t))

(use-package guru-mode
  :config
  (guru-global-mode t))


(use-package emacs
  :custom
  (x-underline-at-descent-line t)
  (underline-minimum-offset 1)
  (use-file-dialog nil)
  (use-dialog-box nil)
  (inhibit-splash-screen t)
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode -1))


(use-package ef-themes)

(load-theme 'ef-melissa-light t)

;; html
(use-package sgml-mode
  :hook
  ((html-mode . sgml-electric-tag-pair-mode)
   (html-mode . sgml-name-8bit-mode))
  :custom
  (sgml-basic-offset 2)
  :config
  (setq sgml-xml-mode t)
  (setq sgml-transformation-function 'upcase))

(use-package tidy
    :config
    (setq sgml-validate-command "tidy"))

(use-package tagedit
  :hook (sgml-mode . tagedit-mode )
  :config
  (with-eval-after-load 'sgml-mode
    (tagedit-add-paredit-like-keybindings)
    (define-key tagedit-mode-map (kbd "M-?") nil)
    (define-key tagedit-mode-map (kbd "M-s") nil)))


(use-package nxml
  :config
  (fset 'xml-mode 'nxml-mode)
  (fset 'html-mode 'nxml-mode)
  (setq nxml-child-indent 2)
  (setq nxml-attribute-indent 2)
  (setq nxml-auto-insert-xml-declaration-flag nil)
  (setq nxml-bind-meta-tab-to-complete-flag t)
  (setq nxml-slash-auto-complete-flag t))

(defun tidy-html ()
  "Tidies the HTML content in the buffer using `tidy'"
  (interactive)
  (shell-command-on-region
   ;; beginning and end of buffer
   (point-min)
   (point-max)
   ;; command and parameters
   "tidy -i -w 120 -q"
   ;; output buffer
   (current-buffer)
   ;; replace?
   t
   ;; name of the error buffer
   "*Tidy Error Buffer*"
   ;; show error buffer?
   t))

;; org

  ;; (straight-use-package '(org	:type built-in))
(straight-use-package 'flx)

(use-package ido
  :straight (:type built-in)
  :config (ido-everywhere 1))

(use-package flx-ido
  :after ido
  :config (flx-ido-mode t))

(use-package ido-completing-read+
  :after ido
  :config (ido-ubiquitous-mode 1))

(use-package vertico
  ;; Special recipe to load extensions conveniently
  :straight '(vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :bind  (:map vertico-map
  ( "<tab>" . vertico-insert)    ; Choose selected candidate
   ("<escape>" . minibuffer-keyboard-quit) ; Close minibuffer
   ("?" . minibuffer-completion-help)
   ("C-M-n" . vertico-next-group)
   ("C-M-p" . vertico-previous-group))
  :custom
  (vertico-count 17)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil)
  :config
  (vertico-mode 1))

(setq completions-format 'vertical)
(setq completion-styles '(flex basic partial-completion emacs22))


  (use-package which-key
  :straight t
  :config
  (which-key-mode t))

(use-package which-key-posframe
  :straight t
  :init (which-key-posframe-mode 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("835d934a930142d408a50b27ed371ba3a9c5a30286297743b0d488e94b225c5f"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
