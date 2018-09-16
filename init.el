
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(setq
 font-lock-support-mode 'jit-lock-mode
 jit-lock-chunk-size   5000
 jit-lock-stealth-time 0.1
 jit-lock-defer-time   0.1
 jit-lock-context-time 0.1)

(setq gc-cons-threshold 10000000) ;;; default 800,000

;;; Packages
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1))

(setq key-chord-two-keys-delay .05 ;; default: 0.1
      key-chord-one-key-delay .075) ;; default: 0.2

(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "")
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          ;; (agenda . 5)
                          (registers . 5)))
  ;; (dashboard-setup-startup-hook)
)

(use-package which-key
  :config (which-key-mode))

;; (use-package hl-todo-mode
;;   :config
;;   :bind (:map hl-todo-mode-map
;;               ("C-c p" . hl-todo-previous)
;;               ("C-c n" . hl-todo-next)
;;               ("C-c o" . hl-todo-occur)))

(set-default 'truncate-lines t)

(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 120))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq visible-bell nil)
(setq indent-tabs-mode nil)
(setq mouse-autoselect-window t)
(setq mouse-drag-copy-region nil)
(global-unset-key [mouse-2])
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))

(show-paren-mode t)
(setq show-paren-delay 0.01)
(setq show-paren-style 'parenthesis)
(setq show-paren-highlight-openparen nil)

(column-number-mode)
(line-number-mode)
(setq fill-column 80)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq completions-format 'vertical)

(setq indent-tabs-mode nil)
(setq tab-width 2)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Reload changed files
(global-auto-revert-mode 1)
;; Delete selected region when inserting text
(delete-selection-mode 1)

;; Word navigation for camelCase
(global-subword-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(save-place-mode 1)
(setq save-place-file "~/.emacs.d/saved-places")

;;; Font
(defun font-exists-p (n)
  (if (null (find-font (font-spec :name n)))
      nil t))
;; TODO: find a font check that works for linux server/client and osx

(let ((f (if (> (display-pixel-height) 2000)
             "Fira Code-13:weight=light"
           "Fira Code-10:weight=light")))
  (add-to-list 'default-frame-alist `(font . ,f))
  (set-default-font f))

;;; Disable graphical dialog boxes
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent 'yes-or-no-p' from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent 'y-or-n-p' from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))

;; (let ((auto-installed-packages
;;        '(autopair
;;          browse-kill-ring
;;          zencoding-mode
;;          skewer-mode
;;          buffer-move
;;          )))
;;   (unless (every 'package-installed-p auto-installed-packages)
;;     (package-refresh-contents)
;;     (mapc (lambda (x) (unless (package-installed-p x)
;;                         (package-install x)))
;;           auto-installed-packages)))


(use-package magit)
(use-package git-timemachine)

;; http://peach-melpa.org/ -- theme gallery
(use-package darktooth-theme :config (load-theme 'darktooth t))
(global-hl-line-mode +1)
;; (use-package paper-theme :config (load-theme 'paper t))

(use-package smartparens
  :config (require 'smartparens-config))

(use-package goto-chg
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package move-text
  :config (move-text-default-bindings))

;; C-x u
(use-package undo-tree
  :config (global-undo-tree-mode 1))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))


;; M-y
(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

;; M-t
(use-package ace-jump-mode
  :bind (("M-t" . ace-jump-mode)))

;; M-0 - M-9
(use-package window-numbering
  :config (window-numbering-mode))

;; Shift + arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; M-S + arrows
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)

(add-hook 'js-mode-hook
          (lambda ()
	    (define-key makefile-mode-map (kbd "M-n") nil)
	    (define-key makefile-mode-map (kbd "M-p") nil)
	    ))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package string-inflection
  :bind (("C-c u" . string-inflection-underscore)))

(use-package smart-mode-line
  :config (setq sml/no-confirm-load-theme t)
          (setq sml/theme 'respectful)
          (sml/setup))

;; (column-enforce-mode)

;; Set PATH
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; Fix Shift-Up on iTerm2
(global-set-key (kbd "<select>") 'windmove-up)

(use-package dtrt-indent
  :config (dtrt-indent-mode 1))
;; (add-to-list 'dtrt-indent-hook-mapping-list
;;              '(jsx-mode c/c++/java jsx-indent-level))

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("M-n" . nil)
              ("M-p" . nil)
              ))

(add-to-list 'auto-mode-alist '("\\.codex\\'" . js-mode))
(add-hook 'js-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq js-indent-level 2)
            (modify-syntax-entry ?` "-" js-mode-syntax-table)))
;;(setq js-mode-hook nil)

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent 4)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))
;; (setq python-mode-hook nil)

(setq browse-url-browser-function 'eww-browse-url)

;; (require 'ido)
;; (setq ido-enable-flex-matching t)

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package projectile
  :config (projectile-mode)
  :chords (("pb" . projectile-compile-project)))

(use-package helm
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 100)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)

  :bind (("M-o" . helm-M-x)
         ("C-x C-r" . helm-recentf)

         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))

  :chords (("xf" . helm-find-files)
           ("xb" . helm-mini)
           ("hx" . helm-M-x)
           ("hj" . helm-resume)))

;; For some reason, these :chords blocks aren't working in some cases..
(use-package helm-projectile
  :config
  (helm-projectile-on)
  (key-chord-define-global "pf" 'helm-projectile-find-file))
  ;; :chords (("pf" . helm-projectile-find-file)))

(use-package helm-ag
  :config (key-chord-define-global "pg" 'helm-ag-project-root))
  ;; :chords (("pg" . helm-ag-project-root)))

(use-package helm-swoop
  ;; :bind (("C-s" . helm-swoop))
  :config
  (key-chord-define-global "hf" 'helm-swoop))
  ;; :chords (("hf" . helm-swoop)))

(use-package helm-ls-git
  :bind (("C-x C-d" . helm-browse-project)))

(use-package shackle
  :config (setq shackle-rules
                '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4))))

(key-chord-define-global "xs" 'save-buffer)
(key-chord-define-global "xk" 'kill-this-buffer)
(key-chord-define-global "x0" 'delete-window)

;; (use-package flycheck
;;   :chords (("fc" . flycheck-buffer)
;;            ("fv" . flycheck-next-error)
;;            ("fb" . flycheck-previous-error)))

;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (require 'space-chord)

(use-package phi-search)
(use-package multiple-cursors
  :bind (("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)
         ("C-S-c C-S-c" . 'mc/edit-lines)
         :map mc/keymap
         ("C-s" . phi-search)
         ("C-r" . phi-search-backward)))

(global-set-key (kbd "C-'") 'switch-to-buffer)
(global-set-key (kbd "C-;") 'other-window) ;; TODO: doesn't work in iTerm2
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-c h") 'help-command)

(global-set-key (kbd "M-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "M-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-<down>")  'shrink-window)
(global-set-key (kbd "M-C-<up>")    'enlarge-window)

(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))


(flet ((comment-or-uncomment-region-or-line
        ()
        (interactive)
        (let (beg end)
          (if (region-active-p)
              (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
          (comment-or-uncomment-region beg end)
          (forward-line))))
  (global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line))


(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(global-set-key (kbd "C-y") 'yank-and-indent)

;; (eval-after-load "haskell-mode" (lambda () (add-hook 'haskell-mode-hook ...)))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; (require 'haskell-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-c-mode))

(eval-after-load "sql"
  '(progn (sql-set-product 'postgres)
          (add-to-list 'sql-postgres-login-params '(port :default 54327))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key emacs-lisp-mode-map
              "\r" 'reindent-then-newline-and-indent)))

;; (rtags-enable-standard-keybindings)
;; (setq rtags-autostart-diagnostics t)
;; (setq rtags-completions-enabled t)

(use-package company
  :config
  ;; (global-company-mode)
  ;; (push 'company-rtags company-backends)
  :bind (:map c-mode-base-map
        ("<C-tab>" . company-complete)))

;; (require 'flycheck-rtags)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)
            (setq c-basic-offset 2)
            (c-set-offset 'innamespace 0)
            (c-set-offset 'inlambda 2)
            (setq tab-width 2)
            (setq indent-tabs-mode nil)
            ))

;; (defun inside-class-enum-p (pos)
;;   "Checks if POS is within the braces of a C++ \"enum class\"."
;;   (ignore-errors
;;     (save-excursion
;;       (goto-char pos)
;;       (up-list -1)
;;       (backward-sexp 1)
;;       (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

;; (defun align-enum-class (langelem)
;;   (if (inside-class-enum-p (c-langelem-pos langelem))
;;       0
;;     (c-lineup-topmost-intro-cont langelem)))

;; (defun align-enum-class-closing-brace (langelem)
;;   (if (inside-class-enum-p (c-langelem-pos langelem))
;;       '-
;;     '+))

;; (defun fix-enum-class ()
;;   "Setup `c++-mode' to better handle \"class enum\"."
;;   (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
;;   (add-to-list 'c-offsets-alist
;;                '(statement-cont . align-enum-class-closing-brace)))

;; (add-hook 'c++-mode-hook 'fix-enum-class)

;; (use-package eglot
;;   :config
;;   (key-chord-define-global "fv" 'flymake-goto-next-error)
;;   (key-chord-define-global "fb" 'flymake-goto-prev-error))

;; (use-package flycheck
;;   :config
;;   (global-flycheck-mode)
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               ;; (setq-local flycheck-highlighting-mode nil)
;;               ;; (flycheck-select-checker 'lsp-ui)
;;               ;; (setq-local flycheck-check-syntax-automatically nil)
;;               ;; (setq rtags-autostart-diagnostics t)
;;               ;; (rtags-diagnostics)
;;               ;; (setq rtags-completions-enabled t)
;;               ;; (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;;               ;; (rtags-start-process-unless-running)
;;               )))


;;   :config
;;   )

;; (use-package rtags
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (flycheck-select-checker 'rtags)
;;               (setq-local flycheck-highlighting-mode nil)
;;               (setq-local flycheck-check-syntax-automatically nil)
;;               (setq rtags-autostart-diagnostics t)
;;               (rtags-diagnostics)
;;               (setq rtags-completions-enabled t)
;;               (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;;               (rtags-start-process-unless-running)
;;               )))


;; cquery:
;;   brew install cquery
;;   pip install compiledb
;;   compiledb -n make debug
;; see also: https://github.com/MaskRay/ccls
;; (use-package cquery
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               ;; (lsp-cquery-enable)
;;               ;; (flycheck-select-checker 'lsp-ui)
;;               ;; (setq-local flycheck-highlighting-mode nil)
;;               ;; (setq-local flycheck-check-syntax-automatically nil)
;;               ;; (setq rtags-autostart-diagnostics t)
;;               ;; (rtags-diagnostics)
;;               ;; (setq rtags-completions-enabled t)
;;               (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;;               ;; (rtags-start-process-unless-running)
;;               )))


;; (use-package company-lsp
;;   :config (push 'company-lsp company-backends))

;; (use-package lsp-ui
;;   :config
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-ui-sideline-enable nil)
;;   (setq lsp-ui-flycheck-enable t)
;;   (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1)))
;;   )

;; (use-package yasnippet
;;   :config (yas-global-mode 1))

;; (setq cquery-executable "/Users/sbillig/local/bin/cquery")
;; (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))


;; (setq c-mode-common-hook nil)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c o") 'ff-find-other-file)
;;             (setq c-basic-offset 2)
;;             (c-set-offset 'innamespace 0)
;;             (setq tab-width 2)
;;             (setq indent-tabs-mode nil)


;;             ;; (flycheck-select-checker 'rtags)
;;             (setq-local flycheck-highlighting-mode nil)
;;             (setq-local flycheck-check-syntax-automatically nil)

;;             ;; (setq rtags-autostart-diagnostics t)
;;             ;; (rtags-diagnostics)
;;             ;; (setq rtags-completions-enabled t)
;;             (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;;             ;; (rtags-start-process-unless-running)
;;             ))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-auto-complete-chars nil)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#52676f")
 '(dtrt-indent-min-quality 40.0)
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(dtrt-indent-verbosity 2)
 '(fci-rule-color "#282a2e")
 '(flymake-gui-warnings-enabled nil)
 '(foreground-color "#52676f")
 '(helm-ag-insert-at-point (quote symbol))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(ido-enable-flex-matching t)
 '(js-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(markdown-edit-code-block-default-mode nil)
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-gfm-additional-languages nil)
 '(mc/always-run-for-all t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (markdown-mode json-mode string-inflection zencoding-mode yasnippet yaml-mode window-numbering which-key web-mode volatile-highlights use-package-chords unfill undo-tree spacemacs-theme smartparens smart-tabs-mode smart-mode-line skewer-mode shackle rtags rainbow-mode rainbow-delimiters phi-search multiple-cursors move-text magit lush-theme lsp-ui keyfreq helm-xref helm-swoop helm-projectile helm-make helm-ls-git helm-flymake helm-ag haskell-mode goto-chg git-timemachine expand-region exec-path-from-shell eglot dtrt-indent doom-themes dashboard darktooth-theme cquery company-lsp column-enforce-mode buffer-move browse-kill-ring beacon badger-theme avy autopair auto-package-update ample-theme ace-jump-mode)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(python-indent-offset 2)
 '(rm-blacklist
   (quote
    (" hl-p" " Projectile[dex]" " Undo-Tree" " AC" " pair")))
 '(shackle-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2)))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
