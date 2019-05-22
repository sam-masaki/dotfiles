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

(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :defer nil
  :config (smart-hungry-delete-add-default-hooks))

;; (setq leerzeichen-line-feed-glyph nil)

(use-package smartparens-config
  :commands smartparens-mode)
(smartparens-global-mode 1)

(use-package company
  :commands company-mode)
(global-company-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq column-number-mode t
      show-paren-delay 0
      scroll-conservatively 101
      global-prettify-symbols-mode t
      display-line-numbers-type 'visual
      custom-file "~/.emacs.d/custom.el"
      whitespace-style (quote (face trailing space-before-tab newline indentation empty space-after-tab tab-mark)))
(show-paren-mode 1)

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
