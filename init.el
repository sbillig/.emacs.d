(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

(set-default 'truncate-lines t)

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

;; (load-theme 'lush t)

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

(add-to-list 'auto-mode-alist '("\\.jsx$" . jsx-mode))
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(add-to-list 'dtrt-indent-hook-mapping-list
             '(jsx-mode c/c++/java jsx-indent-level))

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
(key-chord-define-global "xd" 'save-buffer)
(key-chord-define-global "xs" 'save-buffer)
(key-chord-define-global "xd" 'save-buffer)
(key-chord-define-global "xk" 'kill-this-buffer)
(key-chord-define-global "x0" 'delete-window)

(key-chord-define-global "0o" "\C-u\C-\ ")
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

;; (add-hook 'after-init-hook #'global-flycheck-mode)

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

(setq custom-file (expand-file-name "./custom.el" user-emacs-directory))
(load custom-file)
