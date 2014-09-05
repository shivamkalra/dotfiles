;;; init.el --- The Emacs Initialization File
(setq message-log-max 16384)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq gc-cons-threshold 20000000)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Allow font-lock-mode to do background parsing
(setq jit-lock-stealth-time 1
      jit-lock-stealth-load 100
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0.01)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

;; Override the packages with the git version of package
(add-to-list 'load-path (emacs-d "use-package"))
(add-to-list 'load-path (emacs-d "auto-java-complete"))

;;; External Packages
(load (emacs-d "sk-utils"))
(load (emacs-d "setup-packages"))

(require 'ajc-java-complete-config)
(add-hook 'java-mode-hook 'ajc-java-complete-mode)
(setq ajc-tag-file-list (list (expand-file-name "~/.java_base.tag")))

;; no splash screen - thank you
(setq inhibit-startup-message t
      initial-buffer-choice t)

;; load the theme
(if window-system
    (progn
      (load-theme 'monokai t)
      (set-cursor-color "#FF8800")
      (add-hook 'prog-mode-hook
		(lambda () (progn
			     (hl-line-mode t)
			     (set-face-background 'hl-line "#23241e")))))
  (load-theme 'sanityinc-solarized-dark t))

;;(load-theme 'sanityinc-solarized-dark t)

;; disable all menu(s)
(progn
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

;; enable column number by default
(column-number-mode 1)

;; Visible bell..geez
(setq visible-bell t)

;; disable the case matching for file name
(setq read-file-name-completion-ignore-case t)

;; set the *sick* fonts for gui
(add-to-list 'default-frame-alist
	     '(font . "Fantasque Sans Mono-10:weight=black"))
  
;; key bindings
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-M-_") 'undo-tree-visualize)
(global-set-key (kbd "C-M-+") 'undo-tree-visualizer-quit)

(global-set-key (kbd "C-c y s") 'sk-youtube-search)
(global-set-key (kbd "C-c y p") 'sk-youtube-play-pause)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
