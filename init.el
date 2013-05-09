(global-set-key (kbd "C-'") 'switch-to-buffer)
(global-set-key (kbd "C-;") 'other-window)

(setq mouse-autoselect-window t)
(setq mouse-drag-copy-region nil)
(global-unset-key [mouse-2])

(show-paren-mode t)
(setq show-paren-delay 0.0)
(setq show-paren-style 'parenthesis)
(setq show-paren-highlight-openparen nil)

(column-number-mode)
(line-number-mode)
(tool-bar-mode -1)
(setq default-tab-width 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;; Font
(defun font-exists-p (n)
  (if (null (find-font (font-spec :name n)))
	  nil t))

(let ((f "Inconsolata-10"))
  (when (font-exists-p f)
	;; without the add-to-list here, emacsclients won't pick up the default font
	(add-to-list 'default-frame-alist `(font . ,f))
	(set-default-font f)))

;;; Packages
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(require 'cl)

(setq auto-installed-packages
	  '(auto-complete
		multiple-cursors
		go-autocomplete
		js3-mode
		rainbow-mode
		haskell-mode
		))

(unless (every 'package-installed-p auto-installed-packages)
  (package-refresh-contents)
  (mapc (lambda (x) (if (not (package-installed-p x))
						(package-install x)))
		auto-installed-packages)
)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu 0.8)

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
	 )
  )
