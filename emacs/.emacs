;; .emacs
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

(scroll-bar-mode -1) ;; Putting this after (menu-bar-mode -1) causes visual glitches where the menu bar would be until you resize the window
(menu-bar-mode -1)
(tool-bar-mode -1)

;; (setq leerzeichen-line-feed-glyph nil)

(use-package smartparens-config
  :commands smartparens-mode)
(smartparens-global-mode 1)

(use-package hungry-delete)
(global-hungry-delete-mode 1)
(setq hungry-delete-chars-to-skip " \t")

(use-package company
  :commands company-mode)
(global-company-mode 1)

(use-package git-gutter)
(global-git-gutter-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(winner-mode 1)

(setq-default display-line-numbers-width 3)

(setq column-number-mode t
      show-paren-delay 0
      scroll-conservatively 101
      global-prettify-symbols-mode t
      display-line-numbers-type t
      sp-base-key-bindings 'paredit
      sp-autodelete-closing-pair 'always
      vc-follow-symlinks t
      custom-file "~/.emacs.d/custom.el"
      whitespace-style (quote (face trailing space-before-tab newline indentation empty space-after-tab tab-mark)))

(show-paren-mode 1)
(sp-use-paredit-bindings)

(defun init-c-c++ ()
  (setq indent-tabs-mode nil)
  (google-set-c-style)
  (whitespace-mode))

(add-hook 'c-mode-hook 'init-c-c++)
(add-hook 'c++-mode-hook 'init-c-c++)

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (setq linum-format "%3d ")
  (global-linum-mode))

(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.saves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(add-hook 'after-init-hook
          (lambda ()
            (load-theme 'moe-dark t)))

(load custom-file)
