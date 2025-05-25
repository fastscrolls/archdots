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


(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "Menlo" (font-family-list))
    (set-face-font 'default  (font-spec :family "Menlo" :weight 'light :size 22 :height 158))
    (set-face-font 'fixed-pitch  (font-spec :family "Menlo" :weight 'light :size 22 :height 158))
    (set-face-font 'fixed-pitch-serif (font-spec :family "Times New Roman" :weight 'light :size 22 :height 158))
    (set-face-font 'variable-pitch (font-spec :family "Arial" :weight 'light :size 22 :height 158))))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-face-font 'default  (font-spec :family "DejaVu Sans Mono" :weight 'light :size 22 :height 158))
    (set-face-font 'fixed-pitch  (font-spec :family "DejaVu Sans Mono" :weight 'light :size 22 :height 158))
    (set-face-font 'fixed-pitch-serif (font-spec :family "Serif" :weight 'light :size 22 :height 158))
    (set-face-font 'variable-pitch (font-spec :family "DejaVu Sans" :weight 'light :size 22 :height 158))
    )))


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
(cond
 ((eq system-type 'gnu/linux)
(defcustom variable-pitch-serif-font (font-spec :family "DejaVu Serif")
  "The font face used for `variable-pitch-serif'."
  :group 'basic-faces
  :set (lambda (symbol value)
	   (set-face-attribute 'variable-pitch-serif nil :font value)
	   (set-default-toplevel-value symbol value)))))
(cond
 ((eq system-type 'darwin)
(defcustom variable-pitch-serif-font (font-spec :family "Times New Roman")
  "The font face used for `variable-pitch-serif'."
  :group 'basic-faces
  :set (lambda (symbol value)
	   (set-face-attribute 'variable-pitch-serif nil :font value)
	   (set-default-toplevel-value symbol value)))))

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

(setq-default truncate-lines t)

;; lets be explicit and re-enable everythig important
(setq show-trailing-whitespace t)
(setq indicate-empty-lines t)


(setq tab-width 4)
(setq fill-column 72)
(set-fill-column 72)
(auto-fill-mode t)
(setq-default auto-fill-function 'do-auto-fill)

(setopt display-fill-column-indicator-column 72)
(setq-default display-fill-column-indicator-column 72)
(global-highlight-changes-mode -1)

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
(auto-save-default nil)
(make-backup-files nil)
(backup-by-copying -1)
(version-control -1)
(vc-make-backup-files -1)
(delete-old-versions -1)
(create-lockfiles t)
(auto-save-visited-mode t)
(setq backup-directory-alist `(("." . "~/.saves")))
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

(when (eq system-type 'darwin)
  (menu-bar-mode 1))

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

(use-package vertico
  :straight '(vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :bind  (:map vertico-map
               ("<tab>" . vertico-insert)
               ("<escape>" . minibuffer-keyboard-quit)
               ("?" . minibuffer-completion-help)
               ("C-M-n" . vertico-next-group)
               ("C-M-p" . vertico-previous-group))
  :custom
  (vertico-count 17)
  (vertico-resize t)
  (vertico-cycle nil)
  :init
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)))


(use-package marginalia
  :init
  (marginalia-mode 1))


(use-package vertico
  :config
  (vertico-multiform-mode 1))

(use-package which-key
  :straight t
  :config
  (which-key-mode t))

(use-package which-key-posframe
  :straight t
  :init (which-key-posframe-mode 1))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))
(setq ns-use-proxy-icon nil)

(defvar xah-open-file-at-cursor-pre-hook nil "Hook for `xah-open-file-at-cursor'.
Functions in the hook is called in order, each given the raw input text (path) as arg.
The first return non-nil, its value is given to `xah-open-file-at-cursor' as input. rest functions in hook is ignored.
This is useful for transforming certain url into file path. e.g. change
http://xahlee.info/emacs/index.html
to
C:/Users/xah/web/xahlee_info/emacs/index.html
, so instead of opening in browser, it opens in emacs as file.")

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.

• If there is selection, use it for path.
• Path can be {relative, full path, URL}.
• If the path starts with 「https*://」, open the URL in browser.
• Path may have a trailing 「:‹n›」 that indicates line number, or 「:‹n›:‹m›」 with line and column number. If so, jump to that line number.

If path does not have a file extension, automatically try with .el for elisp files.

See also `xah-open-file-at-cursor-pre-hook'.

This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html'
Created: 2020-10-17
Version: 2024-05-20"
  (interactive)
  (let (xinput xinput2 xpath)
    (setq xinput (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (let ((xp0 (point)) xp1 xp2
                         (xpathStops "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                     (skip-chars-backward xpathStops)
                     (setq xp1 (point))
                     (goto-char xp0)
                     (skip-chars-forward xpathStops)
                     (setq xp2 (point))
                     (goto-char xp0)
                     (buffer-substring-no-properties xp1 xp2))))
    (setq xinput2 (if (> (length xah-open-file-at-cursor-pre-hook) 0)
                      (let ((xprehook (run-hook-with-args-until-success 'xah-open-file-at-cursor-pre-hook xinput)))
                        (if xprehook xprehook xinput))
                    xinput))

    (setq xpath
          (cond
           ((string-match "^file:///[A-Za-z]:/" xinput2) (substring xinput2 8))
           ((string-match "^file://[A-Za-z]:/" xinput2) (substring xinput2 7))
           (t xinput2)))

    (if (string-match-p "\\`https?://" xpath)
        (browse-url xpath)
      (let ((xpathNoQ
             (let ((xHasQuery (string-match "\?[a-z]+=" xpath)))
               (if xHasQuery
                   (substring xpath 0 xHasQuery)
                 xpath))))
        (cond
         ((string-match "#" xpathNoQ)
          (let ((xfpath (substring xpathNoQ 0 (match-beginning 0)))
                (xfractPart (substring xpathNoQ (1+ (match-beginning 0)))))
            (if (file-exists-p xfpath)
                (progn
                  (find-file xfpath)
                  (goto-char (point-min))
                  (search-forward xfractPart))
              (progn
                (message "File does not exist. Created at\n%s" xfpath)
                (find-file xfpath)))))
         ((string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" xpathNoQ)
          (let ((xfpath (match-string-no-properties 1 xpathNoQ))
                (xlineNum (string-to-number (match-string-no-properties 2 xpathNoQ))))
            (if (file-exists-p xfpath)
                (progn
                  (find-file xfpath)
                  (goto-char (point-min))
                  (forward-line (1- xlineNum)))
              (progn
                (message "File does not exist. Created at\n%s" xfpath)
                (find-file xfpath)))))
         ((file-exists-p xpathNoQ)
          (progn ; open f.ts instead of f.js
            (let ((xext (file-name-extension xpathNoQ))
                  (xfnamecore (file-name-sans-extension xpathNoQ)))
              (if (and (string-equal xext "js")
                       (file-exists-p (concat xfnamecore ".ts")))
                  (progn
                    (find-file (concat xfnamecore ".ts"))
                    (warn "Typescript file .ts exist, opening it"))

                (find-file xpathNoQ)))))
         ((file-exists-p (concat xpathNoQ ".el"))
          (find-file (concat xpathNoQ ".el")))
         (t (progn
              (message "File does not exist. Created at\n%s" xpathNoQ)
              (find-file xpathNoQ))))))))


(setq treesit-language-source-alist
 '((bash "https://github.com/tree-sitter/tree-sitter-bash")
   (cmake "https://github.com/uyha/tree-sitter-cmake")
   (css "https://github.com/tree-sitter/tree-sitter-css")
   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
   (go "https://github.com/tree-sitter/tree-sitter-go")
   (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
   (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
   (html "https://github.com/tree-sitter/tree-sitter-html")
   (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
 "master" "src")
   
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (make "https://github.com/alemuller/tree-sitter-make")
   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (toml "https://github.com/tree-sitter/tree-sitter-toml")
   (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
   "master" "tsx/src")
   
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
   "master" "typescript/src")
   
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package lsp-mode
  :straight t
  :defer t
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :hook (prog-mode . lsp)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-prefer-flymake nil)
  (lsp-prefer-capf t)
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.9)
  (lsp-signature-auto-activate t)
  (lsp-enable-symbol-highlighting t))

(use-package lsp-ui
  :straight t
  :defer t
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-modeline-code-actions-enable nil)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-enable-symbol-highlighting t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-doc-enable t)
  (lsp-eldoc-enable-hover t)
  (lsp-ui-doc-show-with-cursor t))

(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode t)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-pos-tip
  :straight t
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-inline
  :straight t
  :hook (flycheck-mode . flycheck-inline-mode))

  (use-package avy-flycheck
    :straight t
    :config
    (avy-flycheck-setup))


(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode))


(use-package magit
  :straight t
  :hook (magit-post-refresh  . diff-hl-magit-post-refresh)
  :config
  (set-default 'magit-push-always-verify nil) 
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes unstage-all-changes)))

(use-package ghub
  :straight t
  :defer t
  :after magit)

(use-package forge
  :straight t
  :defer t
  :after magit)

(use-package git-modes
  :straight t
  :defer t)

(use-package go-mode)
(use-package go-ts-mode
  :ensure t)

(add-hook 'go-mode-hook #'go-ts-mode)
(when (fboundp 'go-ts-mode)
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))


(defun my-go-mode-setup ()
  (add-hook 'go-ts-mode-hook 'flymake-mode 8)
  (add-hook 'go-ts-mode-hook 'flymake-show-buffer-diagnostics 9))

(add-hook 'go-ts-mode-hook #'my-go-mode-setup)

;;corfuuuu
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-preview-current nil)
  :init
  (corfu-popupinfo-mode)
  (global-corfu-mode))

(use-package denote
  :config
  (setq denote-file-type 'text)
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (denote-rename-buffer-mode 1))


(setq eldoc-echo-area-use-multiline-p t)

(with-eval-after-load 'lsp
  (define-key go-ts-mode-map (kbd "<f3>") #'xref-find-definitions)
  (define-key go-ts-mode-map (kbd "M-,") #'xref-pop-marker-stack)
  (define-key go-ts-mode-map (kbd "C-c h") #'eldoc))
(add-to-list 'exec-path (expand-file-name "~/go/bin"))

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
