;;; wordreference.el ---

;; Copyright 2013 Sébastien Le Maguer
;;
;; Author: Sébastien Le Maguer
;; Version: $Id: wordreference.el,v 0.0 2013/07/19 10:23:33 lemaguer Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'wordreference)

;;; Code:

(require 'shr)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defconst wordreference-roots "http://www.wordreference.com/"
  "Wordreference root URL")

(defvar wordreference-source-language "fr"
  "Source language for the translation")

(defvar wordreference-target-language "en"
  "Target language for the translation")


(defun translate (word)
  "Translate the given word using WordReference"
  (interactive "sEnter the word to translate: ")
  (shr-browse-url (concat wordreference-roots wordreference-source-language
					  wordreference-target-language "/"
					  word))
  )

(defun translate-at-point()
  "Translate the word at point using WordReference."
  (interactive)
  (translate (thing-at-point 'word))
  )


(provide 'wordreference)

;;; wordreference.el ends here
