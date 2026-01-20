;;; org-gradle-export.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 16 January 2026
;;

;; Author: Sébastien Le Maguer <sebastien.lemaguer@helsinki.fi>

;; Package-Requires: ((emacs "29.1"))
;; Keywords: org-mode, gradle, export
;; Homepage:

;; org-gradle-export is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; org-gradle-export is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with org-gradle-export.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defcustom org-gradle-export-nb-workers 1
  "The number of parallel workers used by gradle during the compilation."
  :type 'integer
  :group 'org-gradle-export)

(defcustom org-gradle-export-log-level 'info
  "The level of verbosity during the compilation.
It could be either 'info or 'debug"
  :group 'org-gradle-export)

(defun find-gradle-project-root (dir)
  "Find the root directory of the gradle project containg the given `dir'."
  (unless (or (string= dir "/") (string= dir (expand-file-name "~")))
      (if (file-exists-p (concat dir "/build.gradle"))
          dir
        (find-gradle-project-root (f-parent dir)))))

;;;###autoload
(defun org-gradle-export-beamer (&optional with-notes without-multimedia without-video)
  "Export the beamer presentation of the current gradle presentation project.
It assumes the script gradle_export in the path.
`with-notes' exports the beamer slides with the notes.
`without-multimedia' exports the beamer slides without the multimedia content (audio & video).
`without-video' exports the beamer slides without the video content."
  (interactive)
  (let* ((compile-command (concat "gradle_export -j " (number-to-string org-gradle-export-nb-workers) " -s "
                                  (if (eq org-gradle-export-log-level 'info) "-i"
                                    "-d")))
         (compile-command (if with-notes (concat compile-command " -n") compile-command))
         (compile-command (if without-multimedia (concat compile-command " -M") compile-command))
         (compile-command (if without-video (concat compile-command " -V") compile-command))
         (compile-command (concat compile-command " exportBeamer"))
         (project-dir (find-gradle-project-root (expand-file-name (pwd)))))
    (if project-dir
        (let ((default-directory project-dir))
          (compile compile-command t))
      (error "The current buffer is not part of any gradle project!"))))

(defun org-gradle-open-beamer ()
  "Open the *ALREADY COMPILED* beamer presentation"
  (interactive)
  (let* ((project-dir (find-gradle-project-root (expand-file-name (pwd))))
         (beamer-dir (when project-dir (concat project-dir "/build/beamer")))
         (pdfs (when beamer-dir (directory-files beamer-dir nil "[.][Pp][Dd][Ff]$"))))
    (if beamer-dir
        (if pdfs
            (switch-to-buffer-other-window (find-file-noselect (concat beamer-dir "/" (car pdfs))))
          (error "The presentation has not been compiled"))
      (error "The current buffer is not part of any gradle project!"))))

;;;###autoload
(defun org-gradle-export-beamer-teacher ()
  "Export the beamer presentation to be presented by the teacher (with multimedia and notes)."
  (interactive)
  (org-gradle-export-beamer t))

;;;###autoload
(defun org-gradle-export-beamer-student ()
  "Export the beamer presentation to be shared with the students (no multimedia and no notes)."
  (interactive)
  (org-gradle-export-beamer nil t nil))

(provide 'org-gradle-export)

;;; org-gradle-export.el ends here
