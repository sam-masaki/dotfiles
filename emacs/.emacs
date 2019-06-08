;; .emacs
(setq gc-cons-threshold 64000000)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

(use-package smartparens
  :init (use-package smartparens-config)
  :commands smartparens-mode)

(use-package hungry-delete
  :config
  (global-hungry-delete-mode 1)
  (setq hungry-delete-chars-to-skip " \t"))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode 1))

(use-package company
  :commands company-mode)

(use-package git-gutter
  :config (global-git-gutter-mode 1))

(use-package projectile
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package org
  :init
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c c") 'org-capture)
  :config
  (setq
   org-agenda-files '("~/notes/org/main.org")
   org-archive-location "~/notes/org/archive.org::* From %s"
   org-capture-templates '(("t" "To Do"
                            entry
                            (file+olp "~/notes/org/main.org"
                                      "Things to do")
                            "* TODO %?\n")
                           ("p" "Programming Ideas"
                            entry
                            (file+olp "~/notes/org/dev-ideas.org"
                                      "To File")
                            "* %?\n")
                           ("c" "Templates for things to watch, read, etc")
                           ("cl" "To Listen"
                            entry
                            (file+olp "~/notes/org/to-cons.org"
                                      "Music" "Try")
                            "* %^{Artist} - %^{Album Name}\n")
                           ("cr" "To Read"
                            entry
                            (file+olp "~/notes/org/to-cons.org"
                                      "Books" "General")
                            "* %^{Author} - %^{Title}\n")
                           ("cw" "To Watch"
                            entry
                            (file+olp "~/notes/org/to-cons.org"
                                      "Movies")
                            "* %^{Title} - (%^{Year})\n"))))

(use-package org-bullets
  :commands org-bullets)

(use-package subword
  :config (global-subword-mode 1))

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
      inhibit-startup-screen t
      initial-scratch-message " ;; hey lol\n\n"
      whitespace-style (quote (face trailing space-before-tab newline indentation empty space-after-tab tab-mark)))


(use-package ag)
(use-package dumb-jump)

(show-paren-mode 1)
(global-hl-line-mode 1)

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

(add-hook 'js-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  js-indent-level 2)))

(setq lisp-family-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook lisp-family-mode-hooks)
  (add-hook hook (lambda ()
                   (smartparens-mode)
                   (smartparens-strict-mode)
                   (rainbow-delimiters-mode))))

(add-hook 'lisp-mode-hook
          (lambda ()
            (setq lisp-indent-function 'common-lisp-indent-function)))

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode)))

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
           (load-theme 'moe-dark t)
           (scroll-bar-mode -1)
           (menu-bar-mode -1)
           (tool-bar-mode -1)
           (global-company-mode 1)
           (setq gc-cons-threshold 800000)))

(load custom-file)
