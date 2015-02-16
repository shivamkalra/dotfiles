;;; init --- Adapted from Andrew Schwartzmeyer's Emacs init file

;;; Commentary:
;;; Fully customized Emacs configurations
;; (setq debug-on-error t)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
;; Keeps ~Cask~ file in sync with the packages
(require 'pallet)

(require 'use-package)

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
