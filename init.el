(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

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
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;;; Packages
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(require 'cl)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq auto-installed-packages
      '(auto-complete
        autopair
        multiple-cursors
        go-mode
        go-autocomplete
        rainbow-mode
        haskell-mode
        ack-and-a-half
        helm
        projectile
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

;;; Theme
(add-to-list 'load-path "~/.emacs.d/external/")
(require 'color-theme)
(require 'color-theme-tomorrow)
;;; (color-theme-tomorrow-night-eighties)
(color-theme-initialize)
(color-theme-goldenrod)


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
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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

(setq sml/theme 'dark)
(sml/setup)
(column-enforce-mode)

(require 'julia-mode)

(require 'flycheck)

;; Set PATH
(exec-path-from-shell-initialize)

;; Fix Shift-Up on iTerm2
(global-set-key (kbd "<select>") 'windmove-up)

(when (memq window-system '(mac ns))
  (global-set-key
   (kbd "s-R")
   (lambda ()
     (interactive)
     (ns-do-applescript
      "tell application \"Chrome\"
       set winref to a reference to (first window whose title does not start with \"Developer Tools - \")
       set winref's index to 1
       reload active tab of winref
       activate
       end tell
       tell application \"Emacs\"
       activate
       end tell"
      ))))

(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)
(setq ac-auto-show-menu 0.8)
(setq ac-quick-help-delay 0.3)
;; NOTE: cd ~/.emacs.d/external/tern && npm install
(add-to-list 'load-path "~/.emacs.d/external/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)
     (setq tern-ac-on-dot t)))

(setq ac-modes '(emacs-lisp-mode lisp-mode lisp-interaction-mode go-mode))

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
            (setq python-indent 2)
            (setq indent-tabs-mode nil)
            (setq tab-width 2)))
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
(key-chord-define-global "xs" 'save-buffer)
(key-chord-define-global "xk" 'kill-this-buffer)
(key-chord-define-global "x0" 'delete-window)
(key-chord-define-global "kk" 'kill-line)
(key-chord-define-global "yk" 'yank)
(key-chord-define-global "yk" 'yank)
(key-chord-define-global "xf" 'find-file)
(key-chord-define-global "xb" 'switch-to-buffer)
;(key-chord-define-global "fg" 'keyboard-quit)
(key-chord-define-global "df" 'delete-char)

(key-chord-define-global "pf" 'projectile-find-file)
(key-chord-define-global "pg" 'projectile-ack)
(key-chord-define-global "jk" 'move-beginning-of-line)
(key-chord-define-global "kl" 'move-end-of-line)
(key-chord-define-global "9i" 'set-mark-command)
(key-chord-define-global "0o" "\C-u\C-\ ")

(key-chord-define-global "fc" 'flycheck-buffer)
(key-chord-define-global "fn" 'flycheck-next-error)
(key-chord-define-global "fm" 'flycheck-previous-error)
(key-chord-define-global "cg" 'keyboard-quit)

(add-hook 'after-init-hook #'global-flycheck-mode)

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

(require 'go-autocomplete)
(eval-after-load "go-mode"
  ;; Kill the electric indent.
  '(progn
     (define-key go-mode-map "}" nil)
     (define-key go-mode-map ")" nil)
     (define-key go-mode-map "," nil)
     (define-key go-mode-map ":" nil)
     (define-key go-mode-map "=" nil)
     ))

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
            (setq flycheck-clang-include-path
                  '("/usr/lib/c++/v1"
                    "/Users/seanbillig/local/include"
                    "/Users/seanbillig/local/include/libxml2"
                    "/Users/seanbillig/code/modeler_new/external"
                    "/Users/seanbillig/code/core/dex/external"
                    "/Users/seanbillig/code/core/newparser/external"
                    "/Users/seanbillig/code/core/dex_newparser/src"
                    "/Users/seanbillig/code/core/dex_newparser/external"
                    "/Users/seanbillig/code/core/dex_newparser/build/debug"
                    "/Users/seanbillig/code/core/dex/OSX/deps"
                    "/Users/seanbillig/code/core/dex/build/debug"
                    "/Users/seanbillig/code/core/dex/build/msgpack/include"
                    "/Users/seanbillig/code/core/spirit_x3/include"))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dtrt-indent-min-quality 40.0)
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(dtrt-indent-verbosity 2)
 '(ido-enable-flex-matching t)
 '(js3-auto-indent-p t)
 '(js3-consistent-level-indent-inner-bracket nil)
 '(js3-curly-indent-offset 0)
 '(js3-enter-indents-newline t)
 '(js3-global-externs (quote ("require" "console")))
 '(js3-idle-timer-delay 0.4)
 '(js3-indent-on-enter-key t)
 '(js3-pretty-vars nil)
 '(js3-strict-trailing-comma-warning nil)
 '(sml/hidden-modes (quote (" hl-p" " Projectile[dex]" " Undo-Tree" " AC" " pair"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js3-external-variable-face ((t (:foreground "VioletRed2"))) t))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
