;; This file is used as the entry point for main installation

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Some additional configuration to ensure an optimal run (note: saved for remembering
;; (require 'elpaca-menu-elpa)
;; (setf (alist-get 'packages-url (alist-get 'gnu elpaca-menu-elpas))
;;       "https://git.savannah.gnu.org/gitweb/?p=emacs/elpa.git;a=blob_plain;f=elpa-packages;hb=HEAD"
;;       (alist-get 'packages-url (alist-get 'nongnu elpaca-menu-elpas))
;;       "https://git.savannah.gnu.org/gitweb/?p=emacs/nongnu.git;a=blob_plain;f=elpa-packages;hb=HEAD")
(setq elpaca-queue-limit 5)


;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t
        use-package-always-defer nil))

;; Ensure that the proper compat is used to get compat-macs
(use-package compat :ensure t)

;; Block until current queue processed.
(elpaca-wait)

(unload-feature 'eldoc t)
(setq custom-delayed-init-variables '())
(defvar global-eldoc-mode nil)

(elpaca eldoc
  (require 'eldoc)
  (global-eldoc-mode)) ;; This is usually enabled by default by Emacs

;; Transclusion is needed, so install it :)
(use-package org :ensure t)
(use-package org-transclusion :ensure t)
(elpaca-wait)

;; Define helper function
(defun tangle-current-file (file)
  "Helpers which is designed to 1. transclude, 2. tangle, 3. clean the transclude a given FILE."
  (message "##### %s"file)
  (with-current-buffer (find-file-noselect file)
    (org-transclusion-add-all)
    (org-babel-tangle)
    (org-transclusion-remove-all)))

;; And now tangle everything
(setq org-confirm-babel-evaluate nil)
(dolist (file command-line-args-left)
  (tangle-current-file file))
