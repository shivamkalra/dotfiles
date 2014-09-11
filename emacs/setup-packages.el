;;; init-packages.el -- Packages are configured with `use-package'
;;; Adapted from: https://github.com/bdd/.emacs.d
(require 'package)

;;; Code:
(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/"))
              '(("org" . "http://orgmode.org/elpa/"))))

;; disable automatic package loading
(setq package-enable-at-startup nil)
(package-initialize)

;; If never connected to repositories before, download package descriptions so
;; `use-package' can trigger installation of missing packages.
(unless package-archive-contents
  (message "Refreshing ELPA package archives...")
  (package-refresh-contents))

;; but before everything, make sure `use-package' is installed.
(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

;; `use-package' is added as a sub-module
(require 'use-package)
(setq use-package-minimum-reported-time 0)

;; theme
(if window-system
    (use-package monokai-theme
      :ensure t)
  (use-package color-theme-sanityinc-solarized
    :ensure t))

(use-package linum
  :init
  (add-hook 'prog-mode-hook 'linum-mode t)
  :config
  (if (not window-system)
      (setq linum-format "%d ")))

(use-package helm-swoop
  :ensure t
  :bind 
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))
  :config
  (progn
    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i")
      'helm-multi-swoop-all-from-helm-swoop)
    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)
    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)
    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color nil)))

(use-package powerline
  :ensure t
  :defer t
  :config
  (powerline-center-theme))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-." . ace-jump-mode)
	  ("C-," . ace-jump-word-mode)
	  ("C-/" . ace-jump-line-mode)))

(use-package ido-mode
  :defer t
  :init
  (ido-mode t))

(use-package windmove
  :defer t
  :init
  (windmove-default-keybindings))

(use-package multiple-cursors
  :ensure t
  :init
  (progn
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

(use-package paren
  :defer t
  :init
  (add-hook 'prog-mode-hook 'show-paren-mode t)
  :config
  (set-face-attribute 'show-paren-match nil
		      :foreground "#E6DB74"
		      :background "#23241e")
  
  (setq show-paren-style 'parenthesis))

;; visual hint after 80th column number
(use-package whitespace
  :defer t
  :diminish whitespace-mode
  :init
  (hook-into-modes 'whitespace-mode
		   '(prog-mode-hook))
  :config
  (progn
    (message "Whitespace init")
    (setq whitespace-line-column 80)
    (setq whitespace-style '(face lines-tail))))

(use-package browse-kill-ring
  :ensure t
  :defer t
  :config
  (browse-kill-ring-default-keybindings))

(use-package diminish
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package flx-ido
  :ensure t
  :defer t
  :init
  (flx-ido-mode 1)
  :config
  (progn
    (setq gc-cons-threshold (* 20 (expt 2 20)) ; megabytes
          ido-use-faces nil)))

(use-package ido-vertical-mode
  :ensure t
  :defer t
  :init
  (ido-vertical-mode t))

(use-package git-commit-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package goto-chg
  :ensure t
  :defer t
  :bind (("C-c <" . goto-last-change)
         ("C-c >" . goto-last-change-reverse)))

(use-package ido-ubiquitous
  :ensure t
  :init
  ;; Fix ido-ubiquitous for newer packages
  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
    `(eval-after-load ,package
       '(defadvice ,cmd (around ido-ubiquitous-new activate)
          (let ((ido-ubiquitous-enable-compatibility nil))
            ad-do-it))))
  :config
  (progn
    (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
    (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)))

(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep))
  :init
  (progn
    ;; magit extensions
    (use-package magit-blame
      :bind ("C-c C-g b" . magit-blame-mode))

    ;; make magit status go full-screen but remember previous window
    ;; settings
    ;; from: http://whattheemacsd.com/setup-magit.el-01.html
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    ;; Close popup when commiting - this stops the commit window
    ;; hanging around
    ;; From: http://git.io/rPBE0Q
    (defadvice git-commit-commit (after delete-window activate)
      (delete-window))

    (defadvice git-commit-abort (after delete-window activate)
      (delete-window))

    ;; these two force a new line to be inserted into a commit window,
    ;; which stops the invalid style showing up.
    ;; From: http://git.io/rPBE0Q
    (defun magit-commit-mode-init ()
      (when (looking-at "\n")
        (open-line 1)))

    (add-hook 'git-commit-mode-hook 'magit-commit-mode-init))
  :config
  (progn
    ;; restore previously hidden windows
    (defadvice magit-quit-window (around magit-restore-screen activate)
      (let ((current-mode major-mode))
        ad-do-it
        ;; we only want to jump to register when the last seen buffer
        ;; was a magit-status buffer.
        (when (eq 'magit-status-mode current-mode)
          (jump-to-register :magit-fullscreen))))

    (defun magit-maybe-commit (&optional show-options)
      "Runs magit-commit unless prefix is passed"
      (interactive "P")
      (if show-options
          (magit-key-mode-popup-committing)
        (magit-commit)))

    (define-key magit-mode-map "c" 'magit-maybe-commit)

    ;; major mode for editing `git rebase -i` files
    (use-package rebase-mode)

    ;; magit settings
    (setq
     ;; use ido to look for branches
     magit-completing-read-function 'magit-ido-completing-read
     ;; don't put "origin-" in front of new branch names by default
     magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
     ;; open magit status in same window as current buffer
     magit-status-buffer-switch-function 'switch-to-buffer
     ;; highlight word/letter changes in hunk diffs
     magit-diff-refine-hunk t
     ;; ask me if I want to include a revision when rewriting
     magit-rewrite-inclusive 'ask
     ;; ask me to save buffers
     magit-save-some-buffers t
     ;; pop the process buffer if we're taking a while to complete
     magit-process-popup-time 10
     ;; ask me if I want a tracking upstream
     magit-set-upstream-on-push 'askifnotset
     )))

(use-package projectile
  :ensure t
  :defer t
  :init
  (projectile-global-mode)
  :diminish projectile-mode)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package smex
  :ensure t
  :defer t
  :bind ("M-x" . smex))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (progn
    (setq yas-verbosity 3)
    (yas-global-mode 1)))

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :init (add-hook 'prog-mode-hook 'auto-complete t)
  :config
  (progn
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (ac-config-default)
    (ac-set-trigger-key "TAB")
    (ac-set-trigger-key "<tab>")
    (ac-linum-workaround)))

(use-package anzu
  :ensure t
  :init (global-anzu-mode t))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode t))

(use-package recentf
  :init
  (recentf-mode t)
  :config
  (setq recentf-max-saved-items 50))


;; languages modes
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (progn
    (use-package elpy
      :ensure t
      :init
      (elpy-enable))))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    (use-package ac-js2
      :ensure t)
    (add-hook 'js2-mode-hook 'ac-js2-mode)))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode))

(provide 'setup-packages)
