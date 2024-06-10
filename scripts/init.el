;; This is a emacs config used for building the org-mode documentation files.

;; Turn this off since use-package takes care of enabling packages
(setq package-enable-at-startup nil)

;; Turn this off since NixOS provides all packages
(setq package-archives nil)

(eval-when-compile
  (require 'use-package)
  (setq use-package-compute-statistics t))

(defconst emacs-src-dir (file-name-directory load-file-name)
  "Absolute path to zplatform emacs directory.")

;;---- DEFUNS ------------------------------------------------------------------

(defun resolve-org-file (file)
  "Gets the absolute path of the zplatform org-file FILE."
  (expand-file-name file emacs-src-dir))

(defun tangle-org-file (file)
  "Loads the org-file FILE that exists inside the zplatform emacs dir."
  (find-file (resolve-org-file file))
  (org-babel-tangle)
  (kill-buffer))

;;------------------------------------------------------------------------------

(require 'org)
(require 'ob)
(require 'ob-tangle)

(use-package htmlize)

(setq org-plantuml-jar-path
  (concat (getenv "PLANTUML_PATH") "/lib/plantuml.jar"))

(setq org-src-fontify-natively t)

(setq org-log-done 'note)
(setq org-export-html-validation-link nil)
(setq org-latex-listings t)
(setq org-src-preserve-indentation t)
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))
