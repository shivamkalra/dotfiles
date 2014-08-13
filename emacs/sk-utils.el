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

(defun byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun replace-last-sexp ()
  "Evaluate the last lisp expression and replace it with its result"
    (interactive)
    (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))

(defadvice hl-line-mode (after 
			 advice-hl-line-mode 
			 activate 
			 compile)
  (if (not window-system)
      (progn
	(set-face-background hl-line-face "gray13")
	(set-face-foreground 'highlight nil))))


(defadvice linum-mode (after 
			 advice-linum-mode 
			 activate 
			 compile)
  (set-face-attribute 'linum nil :height 100))


(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(provide 'sk-utils)
