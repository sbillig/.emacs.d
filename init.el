(global-set-key (kbd "C-'") 'switch-to-buffer)
(global-set-key (kbd "C-;") 'other-window)
;; TODO: join-line

(setq mouse-autoselect-window t)
(setq mouse-drag-copy-region nil)
(global-unset-key [mouse-2])

(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq show-paren-style 'parenthesis)
(setq show-paren-highlight-openparen nil)

(column-number-mode)
(line-number-mode)
(setq fill-column 80)
(tool-bar-mode -1)
(setq default-tab-width 4)
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

;;; Theme
(add-to-list 'load-path "~/.emacs.d/external/")
(require 'color-theme-tomorrow)
(color-theme-tomorrow-night-eighties)

;;; Font
(defun font-exists-p (n)
  (if (null (find-font (font-spec :name n)))
      nil t))

;; TODO: find a font check that works for linux server/client and osx
(let ((f "Inconsolata-10"))
  (add-to-list 'default-frame-alist `(font . ,f))
  (set-default-font f))

;;; Packages
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(require 'cl)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq auto-installed-packages
      '(auto-complete
        multiple-cursors
        go-autocomplete
        js3-mode
        rainbow-mode
        haskell-mode
        ack-and-a-half
        projectile
        undo-tree
        powerline
        browse-kill-ring
        ace-jump-mode
        switch-window
        zencoding-mode
        web-mode
        ))

(unless (every 'package-installed-p auto-installed-packages)
  (package-refresh-contents)
  (mapc (lambda (x) (unless (package-installed-p x)
                      (package-install x)))
        auto-installed-packages))

;; C-x u
(require 'undo-tree)
(global-undo-tree-mode 1)

;; M-y
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; C-c SPC
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; C-x o
(require 'switch-window)

;; C-j
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;
(require 'web-mode)
;(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(require 'powerline)
(powerline-default-theme)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu 0.8)

(projectile-global-mode)
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

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

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key emacs-lisp-mode-map
              "\r" 'reindent-then-newline-and-indent)))
