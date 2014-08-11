(defun eshell/clear ()
  "Hi, you will clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (message "erase eshell buffer")))

(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

(provide 'sk-utils)
