(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(global-set-key (kbd "C-y") 'yank-and-indent)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-'") 'switch-to-buffer)
(global-set-key (kbd "C-;") 'other-window) ;; TODO: doesn't work in iTerm2
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-c h") 'help-command)

(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

(set-default 'truncate-lines t)

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

(setq indent-tabs-mode nil)
(setq tab-width 2)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Reload changed files
(global-auto-revert-mode 1) ;; TODO: on is slow.
;; Delete selected region when inserting text
(delete-selection-mode 1)

;; Word navigation for camelCase
(global-subword-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

;; (setq auto-save-default nil)
;; (setq auto-save-file-name-transforms `(("." ,"~/.saves/" t)))

;; (setq delete-old-versions t
      ;; kept-new-versions 6
      ;; kept-old-versions 2
      ;; version-control t)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")


;;; Font
(defun font-exists-p (n)
  (if (null (find-font (font-spec :name n)))
      nil t))
;; TODO: find a font check that works for linux server/client and osx
;; (let ((f "Inconsolata-11"))
  ;; (add-to-list 'default-frame-alist `(font . ,f))
  ;; (set-default-font f))

;;; Disable graphical dialog boxes
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

(setq rcirc-default-nick "sbillig")

;;; Packages
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(require 'cl)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq auto-installed-packages
      '(ag
        autopair
        multiple-cursors
        go-mode
        go-autocomplete
        rainbow-mode
        haskell-mode
        ack-and-a-half
        helm
        projectile
        flycheck
        undo-tree
        smart-mode-line
        column-enforce-mode
        browse-kill-ring
        ace-jump-mode
        key-chord
        zencoding-mode
        web-mode
        js3-mode
        skewer-mode
        expand-region
        exec-path-from-shell
        smart-tabs-mode
        buffer-move
        dtrt-indent
        yaml-mode
        ))

(unless (every 'package-installed-p auto-installed-packages)
  (package-refresh-contents)
  (mapc (lambda (x) (unless (package-installed-p x)
                      (package-install x)))
        auto-installed-packages))

;; (load-theme 'lush t)

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; C-x u
(require 'undo-tree)
(global-undo-tree-mode 1)

;; M-y
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; C-c SPC
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

;; (setq sml/no-confirm-load-theme t)
;; (setq sml/theme 'dark)
(sml/setup)
(column-enforce-mode)

;(require 'julia-mode)

(require 'flycheck)

;; Set PATH
(exec-path-from-shell-initialize)

;; Fix Shift-Up on iTerm2
(global-set-key (kbd "<select>") 'windmove-up)

(ac-config-default)
(setq ac-auto-show-menu 0.8)
(setq ac-quick-help-delay 0.3)

(setq ac-modes '(emacs-lisp-mode lisp-mode lisp-interaction-mode go-mode))

(setq-default tab-width 2)

(add-to-list 'auto-mode-alist '("\\.codex\\'" . js-mode))
(add-hook 'js-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq js-indent-level 2)
            ;; (smart-tabs-mode-enable)
            ;; (smart-tabs-advice js-indent-line js-indent-level)
            ))
;;(setq js-mode-hook nil)

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

(setq browse-url-browser-function 'browse-url-chromium)
(require 'skewer-mode)

(require 'ido)
(setq ido-enable-flex-matching t)
(require 'projectile)
(projectile-global-mode)
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "xd" 'save-buffer)
(key-chord-define-global "xs" 'save-buffer)
(key-chord-define-global "xk" 'kill-this-buffer)
(key-chord-define-global "x0" 'delete-window)

;; (key-chord-define-global "kk" 'kill-line)
(key-chord-define-global "yk" 'yank)
(key-chord-define-global "xf" 'find-file)
(key-chord-define-global "xb" 'switch-to-buffer)
;(key-chord-define-global "fg" 'keyboard-quit)
(key-chord-define-global "df" 'delete-char)

(key-chord-define-global "pf" 'projectile-find-file)
(key-chord-define-global "pg" 'ag-project)
(key-chord-define-global "0o" "\C-u\C-\ ")

(key-chord-define-global "fc" 'flycheck-buffer)
;; (key-chord-define-global "fn" 'flycheck-next-error)
(key-chord-define-global "fm" 'flycheck-previous-error)
(key-chord-define-global "cg" 'keyboard-quit)

;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (require 'space-chord)


(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

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

;; (setq c-mode-common-hook nil)
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; (defconst my-cc-style
;;   '("gnu"
;;     (c-basic-offset . 2)
;;     (c-offsets-alist . ((innamespace . [0])))))
;; (c-add-style "my-cc-style" my-cc-style)

;; (setq c++-mode-hook nil)
(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 2)
            (c-set-offset 'innamespace 0)
            (setq tab-width 2)
            (setq indent-tabs-mode nil)
            (setq flycheck-check-syntax-automatically
                  '(mode-enabled new-line save))
            (setq flycheck-clang-language-standard "c++1y")
            (setq flycheck-clang-standard-library "libc++")
            ))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "~/src/rust/src") ;; Rust source code PATH

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (warm-night)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(dtrt-indent-min-quality 40.0)
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(dtrt-indent-verbosity 2)
 '(fci-rule-color "#373b41")
 '(flycheck-clang-include-path
   (list
    (expand-file-name "~/code/core/dex/external")
    (expand-file-name "~/local/include")
    (expand-file-name "~/local/include/libxml2")
    (expand-file-name "~/code/modeler_new/external")
    (expand-file-name "~/code/core/dex/external")
    (expand-file-name "~/code/core/newparser/external")
    (expand-file-name "~/code/core/dex_newparser/src")
    (expand-file-name "~/code/core/dex_newparser/external")
    (expand-file-name "~/code/core/dex_newparser/build/debug")
    (expand-file-name "~/code/core/dex/OSX/deps")
    (expand-file-name "~/code/core/dex/build/debug")
    (expand-file-name "~/code/core/dex/build/msgpack/include")))
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
 '(js3-auto-indent-p t)
 '(js3-cleanup-whitespace nil)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-curly-indent-offset 0)
 '(js3-enter-indents-newline t)
 '(js3-global-externs (quote ("require" "console" "Buffer")))
 '(js3-idle-timer-delay 0.4)
 '(js3-indent-dots t)
 '(js3-indent-on-enter-key nil)
 '(js3-indent-tabs-mode nil)
 '(js3-lazy-dots t)
 '(js3-missing-semi-one-line-override t)
 '(js3-pretty-vars nil)
 '(js3-strict-trailing-comma-warning nil)
 '(jsx-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (zencoding-mode yaml-mode window-numbering web-mode warm-night-theme undo-tree terraform-mode solarized-theme smart-tabs-mode smart-mode-line skewer-mode rainbow-mode racer projectile powerline multiple-cursors markdown-mode magit lush-theme key-chord jsx-mode js3-mode jazz-theme jade-mode helm haskell-mode go-mode go-autocomplete flymake-python-pyflakes flycheck-rust expand-region exec-path-from-shell ensime dtrt-indent column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme cargo buffer-move browse-kill-ring autopair auto-complete-clang-async ample-zen-theme ample-theme ag ack-and-a-half ace-jump-mode)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(rm-blacklist
   (quote
    (" hl-p" " Projectile[dex]" " Undo-Tree" " AC" " pair")))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(terraform-indent-level 2)
 '(vc-annotate-background nil)
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

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#202020" :foreground "#E0E0E0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Menlo"))))
;;  '(ensime-implicit-highlight ((t (:underline "gray20"))))
;;  '(font-lock-function-name-face ((t (:foreground "RoyalBlue3"))))
;;  '(js3-external-variable-face ((t (:foreground "purple3" :underline t)))))
;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
