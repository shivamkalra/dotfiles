;;; init.el --- The Emacs Initialization File
(setq message-log-max 16384)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

;;; External Packages
(load (emacs-d "sk-utils"))
(load (emacs-d "setup-packages"))

;; no splash screen - thank you
(setq inhibit-startup-message t
      initial-buffer-choice t)

;; disable all menu(s)
(progn
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

;; enable column number by default
(column-number-mode 1)

;; Visible bell..geez
(setq visible-bell t)

;; key bindings
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; settings for window and console
(if window-system
    (progn
      ;; add hook for linenum mode add aswell
      (add-hook 'prog-mode-hook 'linum-mode t)
      ;; set the *sick* fonts for gui
      (add-to-list 'default-frame-alist
		   '(font . "Fantasque Sans Mono-10:weight=black"))))
  
;; load the theme
(load-theme 'monokai t)
