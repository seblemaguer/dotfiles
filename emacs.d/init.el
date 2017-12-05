;;; init.el --- Dotemacs -*- no-byte-compile: t -*-
;; Copyright (C) 2011, 2012  Sébastien Le Maguer

;; Author: Sébastien Le Maguer <sebastien.le_maguer@irisa.fr>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;; KEYS
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'cl)
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("local-melpa" . "http://localhost/melpa/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("orgmode" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Silence!!!
(setq save-abbrevs 'silently)

;; Install org
(use-package org
  :ensure org-plus-contrib)

(load-library "url-handlers")

;; Some default variables
(defvar config-basedir "~/.emacs.d/")
(defvar generated-basedir "~/.emacs.d/")
(add-to-list 'load-path (format "%s/plugins" config-basedir))

;; don't let Customize mess with my .emacs
(setq custom-file (concat "~/.emacs.d/custom.el"))
(load custom-file 'noerror)

;; Load private variables
(when (file-exists-p (format "%s/private-variables.el" generated-basedir))
  (load-file (format "%s/private-variables.el" generated-basedir)))

;; Initialise everything now
(org-babel-load-file (format "%s/main.org" config-basedir))
