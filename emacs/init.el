;;; init --- Adapted from Andrew Schwartzmeyer's Emacs init file

;;; Commentary:
;;; Fully customized Emacs configurations
;; (setq debug-on-error t)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)
;;; custom functions
(defun my-yas/prompt (prompt choices &optional display-fn)
  (let* ((names (loop for choice in choices
		      collect (or (and display-fn
				       (funcall display-fn choice))
				  coice)))
	 (selected (helm-other-buffer
		    `(((name . ,(format "%s" prompt))
		       (candidates . names)
		       (action . (("Insert snippet" . (lambda (arg)
							arg))))))
		    "*helm yas/prompt*")))
    (if selected
	(let ((n (position selected names :test 'equal)))
	  (nth n choices))
      (signal 'quit "user quit!"))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;; shortcuts
;; miscellaneous
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c <tab>" 'company-complete)
(bind-key "C-x C-r" 'ido-recentf-open)
;; isearch
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)
(bind-key "C-c g" 'magit-status)

;; projectile command map
(bind-key* "M-[" 'projectile-command-map)

;; disable all menu(s)
(progn
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))
;; set the *sick* fonts for gui
(add-to-list 'default-frame-alist
	     '(font . "Fantasque Sans Mono-10:weight=black"))

(add-hook 'prog-mode-hook 'linum-mode t)

(if (not window-system) (setq linum-format "%d "))

(if window-system
    (add-hook 'prog-mode-hook 'hl-line-mode t))

;; theme (wombat in terminal, solarized otherwise)
(if (display-graphic-p)
    (use-package solarized
      :config
      (progn
	(setq solarized-use-variable-pitch nil)
	(load-theme 'solarized-dark t)))
  (load-theme 'wombat t))

;; windmove
(windmove-default-keybindings)

;; line/column numbers in mode-line
(line-number-mode)
(column-number-mode)

;; C++ mode
(setq c-default-style "linux"
          c-basic-offset 4)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; start week on Monday
(setq calendar-week-start-day 1)

;; indent with new line
(define-key global-map (kbd "RET") 'newline-and-indent)

;; cursor settings
(blink-cursor-mode)

;; visually wrap lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; default truncate lines
(setq-default truncate-lines t)

;; matching parentheses
(show-paren-mode)

;; window undo/redo
(winner-mode)

;; show function in modeline
(which-function-mode)

;;; settings
;; enable all commands
(setq disabled-command-function nil)

;; initial text mode
;; (setq initial-major-mode 'lisp-interaction-mode)

;; visual line mode for text
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; longer commit summaries
(setq git-commit-summary-max-length 72)

;; disable bell
(setq ring-bell-function 'ignore)

;; subword navigation
(subword-mode)

;; increase garbage collection threshold
(setq gc-cons-threshold 20000000)

;; inhibit startup message
(setq inhibit-startup-message t)

;; remove selected region if typing
(pending-delete-mode t)

;; prefer UTF8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; set terminfo
(setq system-uses-terminfo nil)

;; open empty files quietly
(setq confirm-nonexistent-file-or-buffer nil)

;; fix tramp
(eval-after-load 'tramp
  '(progn (setenv "TMPDIR" "/tmp")
	  (setenv "SHELL" "/bin/sh")))
(setq tramp-auto-save-directory "/tmp")

;;; files

;; backups
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 2
      version-control t
      vc-make-backup-files t
      backup-directory-alist `(("." . ,(concat
					user-emacs-directory "backups"))))

;; final-newline
(setq require-final-newline 't)

;; set auto revert of buffers if file is changed externally
(global-auto-revert-mode)

;; symlink version-control follow
(setq vc-follow-symlinks t)

;; add more modes
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.vcsh\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'magic-mode-alist '(";;; " . emacs-lisp-mode))

;; dired
(setq
 ; enable side-by-side dired buffer targets
 dired-dwim-target t
 ; better recursion in dired
 dired-recursive-copies 'always
 dired-recursive-deletes 'top)

;;; functions

;; load local file
(defun load-local (file)
  "Load FILE from ~/.emacs.d, okay if missing."
  (load (f-expand file user-emacs-directory) t))

;; select whole line
(defun select-whole-line ()
  "Select whole line which has the cursor."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))
(bind-key "C-c w" 'select-whole-line)

;; comment/uncomment line/region
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(bind-key "C-c c" 'comment-or-uncomment-region-or-line)

;;; packages
(use-package recentf
  :init
  (recentf-mode t)
  :config
  (setq recentf-max-saved-items 50))

;; ace-jump-mode
(use-package ace-jump-mode
  :config (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))
  :bind (("C-." . ace-jump-mode)
   	 ("C-," . ace-jump-mode-pop-mark)))

;; anzu
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode))

;; browse-kill-ring
(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings)
  :bind ("C-c k" . browse-kill-ring))

;; company "complete anything"
(use-package company
  :config
  (progn
    (setq company-minimum-prefix-length 2
	  company-idle-delay 0.1)
    (add-to-list 'company-backends 'company-c-headers)
    (global-company-mode)))

;; elfeed
(use-package elfeed
  :config (progn (add-hook 'elfeed-new-entry-hook
			   (elfeed-make-tagger :before "2 weeks ago"
					       :remove 'unread)))
  :bind ("C-x w" . elfeed))

;; activate expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; flx-ido
(use-package flx-ido
  :config
  (progn
    (flx-ido-mode)
    (setq ido-enable-flex-matching t
	  ido-use-faces nil)))

;; flyspell
(use-package flyspell
  :config (setq ispell-program-name "aspell" ; use aspell instead of ispell
		ispell-extra-args '("--sug-mode=ultra")))

;; ibuffer
(use-package ibuffer
  :config (add-hook 'ibuffer-mode-hook (lambda () (setq truncate-lines t)))
  :bind ("C-x C-b" . ibuffer))

;; ido setup
(use-package ido
  :config (ido-mode))
(use-package ido-ubiquitous)

;; ido-vertical
(use-package ido-vertical-mode
  :config (ido-vertical-mode))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

;; org-auto-fill
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; org settings
(setq org-pretty-entities t
      org-entities-user '(("join" "\\Join" nil "&#9285;" "" "" "⋈")
			  ("reals" "\\mathbb{R}" t "&#8477;" "" "" "ℝ")
			  ("ints" "\\mathbb{Z}" t "&#8484;" "" "" "ℤ")
			  ("complex" "\\mathbb{C}" t "&#2102;" "" "" "ℂ")
			  ("models" "\\models" nil "&#8872;" "" "" "⊧"))
      org-export-backends '(html beamer ascii latex md))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (C . t)
   (emacs-lisp . t)
   (haskell . t)
   (latex . t)
   (python . t)
   (ruby . t)
   (sh . t)))

;; elpy
(use-package elpy
  :config
  (progn
    (elpy-enable)
    (elpy-use-ipython)))

;; git
(use-package git-gutter
  :init (global-git-gutter-mode t)
  :config
  (progn
    (require 'git-gutter-fringe)
    (global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
    (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
    ;; Jump to next/previous hunk
    (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
    (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
    ;; Stage current hunk
    (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
    ;; Revert current hunk
    (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)))

;; popwin
(use-package popwin
  :config
  (progn
    (popwin-mode)
    ;; cannot use :bind for keymap
    (global-set-key (kbd "C-z") popwin:keymap)))

(defun switch-pyvenv-for-project()
  "Switching Python virtual environment for the project switch"
  ;; check if project name exists in Python virtual environments
  (let (project-name)
    (setq project-name (projectile-project-name))
    (unless (string-equal project-name pyvenv-virtual-env-name)
      (if (member project-name (pyvenv-virtualenv-list))
	  (if (yes-or-no-p (format "Switch virtualenv to [%s]?" project-name))
	      (progn
		(pyvenv-workon project-name)
		(message (format "Switched to [%s]." project-name))))
	))))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-switch-project-action 'projectile-dired)
    (add-hook 'projectile-switch-project-hook 'switch-pyvenv-for-project)))

  ;; saveplace
  (use-package saveplace
    :config
    (setq-default save-place t
		  save-place-file (f-expand "saved-places" user-emacs-directory )))
  ;; scratch
  (use-package scratch
    :bind ("C-c s" . scratch))

;; slime
(use-package sly-autoloads
  :config (setq
	   inferior-lisp-program (executable-find "sbcl")))

;; activate smartparens
(use-package smartparens
  :config
  (progn (use-package smartparens-config)
	 (smartparens-global-mode)
	 (show-smartparens-global-mode)))

;; setup smex bindings
(use-package smex
  :config
  (progn
    (setq smex-save-file (f-expand "smex-items" user-emacs-directory))
    (smex-initialize))
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))

;; scrolling
(use-package smooth-scroll
  :config
  :diminish smooth-scroll-mode
  (progn
    (smooth-scroll-mode)
    (setq smooth-scroll/vscroll-step-size 8)))

;; undo-tree
(use-package undo-tree
  :config
  :diminish undo-tree-mode
  (progn
    (global-undo-tree-mode)
    (add-to-list 'undo-tree-history-directory-alist
		 `("." . ,(f-expand "undo-tree" user-emacs-directory)))
    (setq undo-tree-auto-save-history t)))

;; uniquify
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; setup virtualenvwrapper
(use-package virtualenvwrapper
  :config (setq venv-location "~/.virtualenvs/"))

;; whitespace
(use-package whitespace
  :config
  (progn
    (add-hook 'before-save-hook 'whitespace-cleanup)
    (setq whitespace-line-column 80 ;; limit line length
	  whitespace-style '(face tabs empty trailing lines-tail))))

;; yasn\ippet
(use-package yasnippet
  :config
  (progn
    (yas-global-mode)
    (unbind-key "<tab>" yas-minor-mode-map)
    (unbind-key "TAB" yas-minor-mode-map)
    (bind-key "C-c y" 'yas-expand yas-minor-mode-map)
    (custom-set-variables '(yas/prompt-functions '(my-yas/prompt))))
  :idle (yas-reload-all))

;;; provide init package
(provide 'init)

;;; init.el ends here
