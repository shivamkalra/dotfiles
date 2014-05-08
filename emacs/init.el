(require 'package)
(require 'whitespace)

(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(ido-mode t)
(ido-vertical-mode t)

;;; Lazy loading of smex
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")

(global-set-key (kbd "M-x") 'smex)

;; M-x convert space to hyphen
(defadvice smex (around space-inserts-hyphen activate compile)
        (let ((ido-cannot-complete-command 
               `(lambda ()
                  (interactive)
                  (if (string= " " (this-command-keys))
                      (insert ?-)
                    (funcall ,ido-cannot-complete-command)))))
          ad-do-it))

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
 
;; enable recent files mode.
(recentf-mode t)
 
; 50 files ought to be enough.
(setq recentf-max-saved-items 50)
 
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; theming
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess pit up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'solarized-dark t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; 80 column
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)
