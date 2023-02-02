(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(require 'org)
(require 'ox)
(require 'ox-beamer)
(require 'ox-latex)
(require 'cl)

(require 'org-notebook)

(setq org-list-allow-alphabetical t) ;; FIXME quoi qu'est ce?

;; Add packages
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"
      org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

;; Display images directly in the buffer
(setq org-babel-results-keyword "results")
(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;; Add languages
(require 'ob-ipython)
(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (dot . t)
                               (ditaa . t)
                               (R . t)
                               (ipython . t)
                               (ruby . t)
                               (gnuplot . t)
                               (clojure . t)
                               (sh . t)
                               (ledger . t)
                               (org . t)
                               (plantuml . t)
                               (latex . t)))


; Define specific modes for specific tools
(add-to-list 'org-src-lang-modes '("plantuml" . fundamental))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)


(require 'ox-html)
(require 'htmlize)
(require 'ox-reveal)

(setq org-html-xml-declaration '(("html" . "")
                                 ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
                                 ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
      org-export-html-inline-images t
      org-export-with-sub-superscripts nil
      org-export-html-style-extra "<link rel=\"stylesheet\" href=\"org.css\" type=\"text/css\" />"
      org-export-html-style-include-default nil
      org-export-htmlize-output-type 'css ; Do not generate internal css formatting for HTML exports
      org-html-htmlize-output-type 'css
      )

(defun endless/export-audio-link (path desc format)
  "Export org audio links to hmtl."
  (cl-case format
    (html (format "<audio src=\"%s\" controls>%s</audio>" path (or desc "")))))
(org-add-link-type "audio" #'ignore #'endless/export-audio-link)


(defun endless/export-video-link (path desc format)
  "Export org video links to hmtl."
  (cl-case format
    (html (format "<video controls src=\"%s\"></video>" path (or desc "")))))
(org-add-link-type "video" #'ignore #'endless/export-video-link)

(add-to-list 'org-file-apps '("\\.x?html?\\'" . "/usr/bin/firefox %s"))

(require 'ox-latex)
  (setq org-latex-listings t
	org-export-with-LaTeX-fragments t
	org-latex-pdf-process (list "latexmk -f -pdf %f"))

(require 'ox-beamer)

(defun my-beamer-bold (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))
(add-to-list 'org-export-filter-bold-functions 'my-beamer-bold)

(setq org-export-docbook-xsl-fo-proc-command "fop %s %s"
      org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")

(require 'ox-md)
(require 'ox-gfm)

;;
(load-theme 'meacupla t)
