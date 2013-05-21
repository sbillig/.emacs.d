(global-set-key (kbd "C-'") 'switch-to-buffer)
(global-set-key (kbd "C-;") 'other-window)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(setq mouse-autoselect-window t)
(setq mouse-drag-copy-region nil)
(global-unset-key [mouse-2])
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))

(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq show-paren-style 'parenthesis)
(setq show-paren-highlight-openparen nil)

(column-number-mode)
(line-number-mode)
(setq fill-column 80)
(tool-bar-mode -1)
(scroll-bar-mode -1)
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
        go-mode
        go-autocomplete
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
        js2-mode
        skewer-mode
        expand-region
        exec-path-from-shell
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

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(require 'powerline)
(powerline-default-theme)

(when (memq window-system '(mac ns))
  ;; Work-around for 10.8's lack of non-admin-settable PATH for gui apps
  ;; (~/.launchd.conf isn't supported, as of 10.8.3)
  (exec-path-from-shell-initialize)
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

;(add-hook 'js-mode-hook (lambda () (tern-mode t)))

;(let ((f (lambda () (tern-mode t))))
  ;(add-hook 'js3-mode-hook 'f)
;  (add-hook 'js-mode-hook 'f))

(require 'js2-mode)
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-strict-trailing-comma-warning nil)
             (setq js2-highlight-level 3)
             (setq js2-basic-offset 2)
             (setq js2-idle-timer-delay 0.4)
             (setq indent-tabs-mode nil)
             (setq default-tab-width 2)))

(setq browse-url-browser-function 'browse-url-chromium)
(require 'skewer-mode)



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
