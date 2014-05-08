(defun setup-window-appearance()
  ;; This is for any specific apprearance requirements in window mode
  (add-to-list 'default-frame-alist
	       '(font . "DejaVu Sans Mono-9"))
  ;; global line mode looks ugly in console
  (global-hl-line-mode 't))

(defun setup-console-appearance())

(if window-system
    (progn
      (setup-window-appearance)
      (setup-console-appearance)))

;; common stuff

(provide 'appearance)
