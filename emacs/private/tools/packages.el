(setq tools-packages
    '(
      drag-stuff
      ag
      (emacs-helm-ispell :location local)
      ))

;; List of packages to exclude.
(setq tools-excluded-packages '())

;; For each package, define a function tools/init-<package-name>
;;
(defun tools/init-drag-stuff ()
  "Initialize drag stuff package"
  (use-package drag-stuff
    :init (drag-stuff-global-mode)
    (spacemacs/set-leader-keys "om" 'tools)
    :config
    (progn
      (global-set-key (kbd "M-<up>") 'drag-stuff-up)
      (global-set-key (kbd "M-<down>") 'drag-stuff-down)
      )))

(defun tools/init-emacs-helm-ispell ()
  "Initialize helm ispell package"
  (use-package helm-ispell
    :init (global-set-key (kbd "M-<tab>") 'helm-ispell)))


(defun tools/init-ag ()
  "Initialize ag package"
  (use-package ag))
