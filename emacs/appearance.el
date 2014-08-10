(defun setup-window-appearance()
  "Call functions only on window mode"
  ;; disabling all the visible menus
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; This is for any specific apprearance requirements in window mode
  (add-to-list 'default-frame-alist
	       '(font . "Fantasque Sans Mono-10:weight=black"))
  ;; global line mode looks ugly in console
  (global-hl-line-mode 't)
  (global-linum-mode 't)
  ;; wind move default keybindings
  (windmove-default-keybindings))

(defun setup-console-appearance()
  ;; wind move
  (global-set-key (kbd "C-c <left>")  'windmove-left)
  (global-set-key (kbd "C-c <right>") 'windmove-right)
  (global-set-key (kbd "C-c <up>")    'windmove-up)
  (global-set-key (kbd "C-c <down>")  'windmove-down))

;; common to both consol and window
(menu-bar-mode -1)
(require 'whitespace)

;; Visual hint when line exceeds 80 columns
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))

;; Hooks this only for programming mode
(add-hook 'prog-mode-hook 'whitespace-mode)

(if window-system
    (setup-window-appearance)
  (setup-console-appearance))

(provide 'appearance)
