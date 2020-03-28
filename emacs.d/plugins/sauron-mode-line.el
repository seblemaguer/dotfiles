;;; alert-sauron.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 19 March 2020
;;

;; Author: Sébastien Le Maguer <lemagues@tcd.ie>

;; Package-Requires: ((emacs "25.2"))
;; Keywords:
;; Homepage:

;; alert-sauron is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; alert-sauron is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with alert-sauron.  If not, see http://www.gnu.org/licenses.

;;; Commentary:


;;; Code:

;;; alert-sauron.el ends here

(require 'alert)

(defvar sauron-mode-line-current-level 0
  "The current level associated with the modeline higlight alert.")

(defvar sauron-mode-line-threshold 1
  "The mode coloring highlight threshold. If current level is
      below, mode-line is sticking to its current state.")

(defface sauron-mode-line-backup-face nil
  "The face which store the backup default mode."
  :group 'alert)

(defun sauron-mode-line-clear ()
  "Helper to clear the content of the sauron log and reset the mode-line status."
  (interactive)
  (progn
    (when (> sauron-mode-line-current-level sauron-mode-line-threshold)
      (progn
        (copy-face 'sauron-mode-line-face 'mode-line)
        (setq sauron-mode-line-current-level 0)))
    (sauron-clear)))

(defun sauron-mode-line-toggle-hide-show ()
  "Helper to show/hide the sauron part and reset the mode-line status."
  (interactive)
  (progn
    (sauron-toggle-hide-show)
    (when (> sauron-mode-line-current-level sauron-mode-line-threshold)
      (progn
        (copy-face 'backup-mode-line-face 'mode-line)
        (setq sauron-mode-line-current-level 0)))))

;; ==============================================================================================================

(defcustom sauron-mode-line-priorities
  '((urgent   . 5)
    (high     . 4)
    (moderate . 3)
    (normal   . 2)
    (low      . 1)
    (trivial  . 0))
  "A mapping of alert severities onto Growl priority values."
  :type '(alist :key-type symbol :value-type integer)
  :group 'alert)

(defun alert-sauron-notify (info)
  "Handler to create a sauron event using INFO which is emitted by alert."
  (sauron-add-event (if (plist-get info :category)
                        (if (symbolp (plist-get info :category))
                            (plist-get info :category)
                          (intern (plist-get info :category)))
                      (if (plist-get info :title)
                          (if (symbolp (plist-get info :title))
                              (plist-get info :title)
                            (intern (plist-get info :title)))
                        'unknown))
                    (cdr (assq (plist-get info :severity) sauron-mode-line-priorities))
                    (plist-get info :message)))

(alert-define-style 'sauron
                    :title "Use sauron as a backend for alert"
                    :notifier #'alert-sauron-notify
                    :continue t
                    :remover #'alert-message-remove)

;; ==============================================================================================================

(defun sauron-mode-line-notifier (info)
  "Alert mode-line flashing style notifier."
  ;; First time => copy the current mode line faceq
  (when (<= sauron-mode-line-current-level sauron-mode-line-threshold)
    (copy-face 'mode-line 'backup-mode-line-face))

  ;; Change the policy
  (when (and (> (cdr (assq (plist-get info :severity) sauron-mode-line-priorities))
                sauron-mode-line-threshold)
             (> (cdr (assq (plist-get info :severity) sauron-mode-line-priorities))
                sauron-mode-line-current-level))
    (set-face-attribute 'mode-line nil
                        :background (cdr (assq (plist-get info :severity)
                                               alert-severity-colors)))
    )

  ;; Change current level if this one is upper
  (when (> (cdr (assq (plist-get info :severity) sauron-mode-line-priorities))
           sauron-mode-line-current-level)
    (setq sauron-mode-line-current-level (cdr (assq (plist-get info :severity)
                                                    sauron-mode-line-priorities))))

  ;; Print the message everytime !
  (alert-sauron-notify info))


(defun sauron-mode-line-notifier-from-sauron (origin prio msg props)
  "Alert mode-line flashing style notifier."

  ;; First time => copy the current mode line faceq
  (when (<= sauron-mode-line-current-level sauron-mode-line-threshold)
    (copy-face 'mode-line 'backup-mode-line-face))

  ;; Change the policy
  (when (and (> prio sauron-mode-line-threshold)
             (> prio sauron-mode-line-current-level))
    (let ((sev (nth prio '(trivial low normal high urgent))))
      (set-face-attribute 'mode-line nil
                          :background (cdr (assq sev alert-severity-colors)))))

  ;; Change current level if this one is upper
  (when (> prio sauron-mode-line-current-level)
    (setq sauron-mode-line-current-level prio))

  (unless props
    (message "%S: %S" origin msg)))

(alert-define-style 'sauron-mode-line-style
                     :title "Sauron/flashing mode line style"
                     :notifier 'sauron-mode-line-notifier
                     :continue t
                     :remover #'alert-message-remove)

(provide 'sauron-mode-line)
