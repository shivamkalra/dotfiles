(defun setup-window-appearance()
  "Call functions only on window mode"
  ;; disabling all the visible menus
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; This is for any specific apprearance requirements in window mode
  (add-to-list 'default-frame-alist
	       '(font . "DejaVu Sans Mono-9"))
  ;; global line mode looks ugly in console
  (global-hl-line-mode 't))

(defun setup-console-appearance())


;; common to both consol and window
(menu-bar-mode -1)
(require 'whitespace)

;; Visual hint when line exceeds 80 columns
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))

;; Hooks this only for programming mode
(add-hook 'prog-mode-hook 'whitespace-mode)

(if window-system
    (progn
      (setup-window-appearance)
      (setup-console-appearance)))

(provide 'appearance)
