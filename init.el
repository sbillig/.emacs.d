(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))



(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 120))

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

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;;; Font
(defun font-exists-p (n)
  (if (null (find-font (font-spec :name n)))
      nil t))
;; TODO: find a font check that works for linux server/client and osx
(let ((f "Inconsolata-11"))
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

(setq rcirc-default-nick "sbillig")

;;; Packages
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(require 'cl)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(let ((auto-installed-packages
       '(autopair
         company
         keyfreq
         magit
         multiple-cursors
         rainbow-mode
         haskell-mode
         helm
         helm-ag
         helm-ls-git
         helm-make
         helm-projectile
         helm-swoop
         phi-search
         shackle
         lush-theme
         projectile
         flycheck
         undo-tree
         smart-mode-line
         column-enforce-mode
         browse-kill-ring
         ace-jump-mode
         window-numbering
         key-chord
         zencoding-mode
         web-mode
         skewer-mode
         expand-region
         exec-path-from-shell
         smart-tabs-mode
         buffer-move
         dtrt-indent
         yaml-mode
         rtags
         )))
  (unless (every 'package-installed-p auto-installed-packages)
    (package-refresh-contents)
    (mapc (lambda (x) (unless (package-installed-p x)
                        (package-install x)))
          auto-installed-packages)))

(load-theme 'lush t)

(require 'helm-config)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; C-x u
(require 'undo-tree)
(global-undo-tree-mode 1)

;; M-y
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; M-t
(require 'ace-jump-mode)
(define-key global-map (kbd "M-t") 'ace-jump-mode)

;; M-0 - M-9
(window-numbering-mode)

;; Shift + arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; M-S + arrows
(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)

;; C-j
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;
(require 'web-mode)
;(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(sml/setup)
(column-enforce-mode)

;(require 'julia-mode)

(require 'flycheck)

;; Set PATH
(exec-path-from-shell-initialize)

;; Fix Shift-Up on iTerm2
(global-set-key (kbd "<select>") 'windmove-up)

(add-to-list 'auto-mode-alist '("\\.jsx$" . jsx-mode))
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(add-to-list 'dtrt-indent-hook-mapping-list
             '(jsx-mode c/c++/java jsx-indent-level))

(setenv "GOPATH" (expand-file-name "~/code/go"))
;; (add-to-list 'load-path "~/.emacs.d/external/auto-complete-clang-async")
;; (require 'auto-complete-clang-async)
;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/external/auto-complete-clang-async/clang-complete")
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process)
;; )
;; (defun my-ac-config ()
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))
;; (my-ac-config)

(add-to-list 'auto-mode-alist '("\\.codex\\'" . js-mode))
(add-hook 'js-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq js-indent-level 2)
            ;; (modify-syntax-entry ?` " " js-mode-syntax-table)
            ;; (smart-tabs-mode-enable)
            ;; (smart-tabs-advice js-indent-line js-indent-level)
            ))
;;(setq js-mode-hook nil)


(add-hook 'jsx-mode-hook
          (lambda ()
            (setq tab-width 2)))

(add-hook 'julia-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq julia-basic-offset 4)
            ))

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent 4)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))
;; (setq python-mode-hook nil)

(setq browse-url-browser-function 'eww-browse-url)
(require 'skewer-mode)

;; (require 'ido)
;; (setq ido-enable-flex-matching t)
(require 'projectile)
(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action)

(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "xs" 'save-buffer)
(key-chord-define-global "xd" 'save-buffer)
(key-chord-define-global "xk" 'kill-this-buffer)
(key-chord-define-global "x0" 'delete-window)
(key-chord-define-global "xf" 'helm-find-files)
(key-chord-define-global "xb" 'helm-buffers-list)

(key-chord-define-global "pf" 'helm-projectile-find-file)
(key-chord-define-global "pg" 'helm-ag-project-root)
(key-chord-define-global "hx" 'helm-M-x)
(key-chord-define-global "hf" 'helm-swoop)
(key-chord-define-global "hj" 'helm-resume)


(key-chord-define-global "fc" 'flycheck-buffer)
(key-chord-define-global "fv" 'flycheck-next-error)
(key-chord-define-global "fb" 'flycheck-previous-error)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; (require 'space-chord)

(require 'multiple-cursors)
(require 'phi-search)
(define-key mc/keymap (kbd "C-s") 'phi-search)
(define-key mc/keymap (kbd "C-r") 'phi-search-backward)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
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


;; (eval-after-load "haskell-mode" (lambda () (add-hook 'haskell-mode-hook ...)))

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-to-list 'auto-mode-alist '("\\.chs\\'" . haskell-c-mode))

(eval-after-load "sql"
  '(progn (sql-set-product 'postgres)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key emacs-lisp-mode-map
              "\r" 'reindent-then-newline-and-indent)))

(rtags-enable-standard-keybindings)
;; (setq rtags-autostart-diagnostics t)
;; (setq rtags-completions-enabled t)
(require 'company)
(global-company-mode)
(push 'company-rtags company-backends)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(require 'flycheck-rtags)

;; (setq c-mode-common-hook nil)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)
            (setq c-basic-offset 2)
            (c-set-offset 'innamespace 0)
            (setq tab-width 2)
            (setq indent-tabs-mode nil)

            (flycheck-select-checker 'rtags)
            (setq-local flycheck-highlighting-mode nil)
            (setq-local flycheck-check-syntax-automatically nil)

            (setq rtags-autostart-diagnostics t)
            (rtags-diagnostics)
            (setq rtags-completions-enabled t)
            (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
            ;; (rtags-start-process-unless-running)
            ))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(background-color "#fcf4dc")
 '(background-mode light)
 '(company-auto-complete-chars nil)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#52676f")
 '(custom-enabled-themes (quote (lush)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "ab0950f92dc5e6b667276888cb0cdbc35fd1c16f667170a62c15bd3ed5ae5c5a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "756ec68798410a2e705dd719c7328af9ecbb782c94130d489b6b3109841833eb" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" "27b7d32fa83dc83ce3034e2a1fe31174c9abff70c1121e4a42b2ce08cc791aec" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(dtrt-indent-min-quality 40.0)
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(dtrt-indent-verbosity 2)
 '(fci-rule-color "#282a2e")
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(flycheck-clang-include-path
   (quote
    ("/Users/seanbillig/local/include" "/Users/seanbillig/local/include/libxml2" "/Users/seanbillig/code/modeler_new/external" "/Users/seanbillig/code/core/dex_ka/external" "/Users/seanbillig/code/core/dex_ka/src" "/Users/seanbillig/code/core/dex_ka/build/debug" "/Users/seanbillig/code/core/dex_ka/build/msgpack/include" "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include")))
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
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(js3-auto-indent-p t)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-curly-indent-offset 0)
 '(js3-enter-indents-newline t)
 '(js3-global-externs (quote ("require" "console")))
 '(js3-idle-timer-delay 0.4)
 '(js3-indent-dots t)
 '(js3-indent-on-enter-key t)
 '(js3-lazy-dots t)
 '(js3-missing-semi-one-line-override t)
 '(js3-pretty-vars nil)
 '(js3-strict-trailing-comma-warning nil)
 '(jsx-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(mc/always-run-for-all t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (flycheck-swift3 swift-mode tide rudel toml-mode warm-night-theme phi-search rtags zencoding-mode yaml-mode xml+ window-numbering websocket web-mode undo-tree terraform-mode switch-window string-inflection solarized-theme smartparens smart-tabs-mode smart-mode-line slime skewer-mode shackle scss-mode request rainbow-mode ppd-sr-speedbar powerline php-mode opencl-mode oauth2 nginx-mode multiple-cursors multi-web-mode markdown-mode magit lush-theme llvm-mode keyfreq key-chord jsx-mode json-mode irony helm-swoop helm-projectile helm-make helm-ls-git helm-ag haskell-mode graphene-meta-theme google-this google go-mode go-autocomplete flycheck feature-mode f expand-region exec-path-from-shell emojify dtrt-indent disaster company column-enforce-mode color-theme-solarized color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-project cmake-ide cmake-font-lock circe buffer-move browse-kill-ring autopair auto-complete-clang-async alert ag ack-and-a-half ace-jump-mode)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(python-indent-offset 2)
 '(rm-blacklist
   (quote
    (" hl-p" " Projectile[dex]" " Undo-Tree" " AC" " pair")))
 '(shackle-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/theme (quote respectful))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(terraform-indent-level 2)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-warning ((t (:underline "orange"))))
 '(js3-external-variable-face ((t (:foreground "SlateBlue1" :underline t)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
