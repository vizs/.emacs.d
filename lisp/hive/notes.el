;; -*- lexical-binding: t; -*-

;; TODO: Add functions to search through notes and add
;; capture-templates to insert notes.

;; * Annotations

;; This file is mostly to do with PDF annotations. This will be
;; achieved using pdftools and org-noter + org-pdftools.

(use-package pdf-tools
  :config
  (pdf-tools-install))

;; ** Set `header-line-format'
;; To let me be lazy and use the mouse for highlighting, a header-line
;; with commands will be added for pdf buffers; think org-caputre's
;; headerline but with clickable text instead. ALso, I think it's
;; faster this way.

(defvar vz/pdf-tools-header-line-functions
  '(("text" . #'pdf-annot-add-text-annotation)
    ("underline" . #'pdf-annot-add-underline-markup-annotation)
    ("highlight" . #'pdf-annot-add-highlight-markup-annotation)
    ("remove" . #'pdf-annot-delete))
  "An alist of text to be displayed and the function to be run
  when pressing mouse-1 on text.")

(defun vz/pdf-tools-header-line--generate-event-function (cmd)
  `(lambda (_)
    (interactive "e")
    (call-interactively ,cmd)))

(defun vz/pdf-tools-header-line--generate-images ()
  (-map (fn
         (-let (((txt . cmd) <>))
           (message "%s" cmd)
           (propertize (format " %s " txt)
                       'display (vz/mode-line-roundise-text txt nil nil t)
                       'local-map (let ((map (make-sparse-keymap)))
                                    (define-key map
                                      [header-line mouse-1]
                                      (vz/pdf-tools-header-line--generate-event-function cmd))
                                    map))))
        vz/pdf-tools-header-line-functions))

(add-hook 'pdf-annot-minor-mode-hook
          (defun vz/pdf-tools-pdf-annot-set-header-line-format ()
            (face-remap-add-relative 'header-line `(:inherit mode-line
                                                    :box (:line-width 2 :color ,(face-attribute 'mode-line :background))))
            (let* ((images (vz/pdf-tools-header-line--generate-images))
                   (length (-reduce-from (fn (+ (length <2>) <1>))
                                         0 images)))
              (setq-local header-line-format
                          (cons (propertize " " 'display `((space :align-to (- center (0.5 . ,length))))
                                            'face 'header-line)
                                images)))))

;; ** Remove file status from mode-line and page number
;; The file status is always going to be the same so having it is pointless.
;; Page number is handy to have though.

(defun vz/pdf-tools-mode-line--get-page-number ()
  (format "%d/%d" (pdf-view-current-page) (pdf-cache-number-of-pages)))

(add-hook 'pdf-tools-enabled-hook
          (defun vz/pdf-tools-mode-line ()
            (setq-local vz/mode-line-file-include-file-status? nil
                        vz/mode-line-file-extra-info
                        '(vz/pdf-tools-mode-line--get-page-number))))

;; * org-noter
;; org-noter package is inspired by interleave which in turn mimicks
;; some old books that had an empty after every page so that students
;; and other reads could take notes side-by-side. When I read a
;; physical book, I tend to scribble my notes on the page itself and
;; underline text. So I think org-noter, which is a "reimplementation"
;; of interleave without some nuances, will work the best for me.

(use-package org-noter
  :config
  (setq
   org-noter-default-heading-title "Page $p$"
   org-noter-doc-split-fraction '(0.6 . 0.4)
   org-noter-auto-save-last-location t
   org-noter-default-notes-file-names '("annotations.org")
   org-noter-notes-search-path (-map (fn (~ <>)) '("doc/org" "doc/uni"))
   org-noter-doc-property-in-notes t)
  ;; I really don't need org-noter to add stuff to my modeline
  (advice-add 'org-noter--mode-line-text :override (fn "")))

;; TODO: org-noter-pdftools doesn't really like to work. It starts
;; asking stuff when I enable org-noter.
(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

;; * TODO: org-transclusion (buggy rn)
;; org-transclusion lets you preview the part you're citing and lets
;; you edit too. This is useful when crosslinking multiple files and
;; when exporting too.

;; (use-package org-transclusion
;;   :straight (:host github
;;                    :branch "main" ;; Why?
;;                    :repo "nobiot/org-transclusion")
;;   :config
;;   (setq
;;    org-transclusion-activate-persisent-message t))

;; * Latex Macros and custom cdlatex commands
;; cdlatex has commands like fr which expands to \frac{}{}. This is
;; really useful and saves a whole bunch of time. Other functions like
;; \mathrm'ing text seems like it will be fast but I found that it is
;; quite tedious especially when writing derivative of a variable so
;; custom latex macros like \d will do for these. A lot of these are
;; inspired by other posts like:
;; 1. https://castel.dev/post/lecture-notes-1/
;; 2. https://sbseminar.wordpress.com/2010/02/25/chromatic-homotopy-ii-or-how-i-learned-to-stop-worrying-and-love-latexing-in-real-time/

;; ** Custom cdlatex-commands

(with-eval-after-load 'cdlatex
  (setq
   cdlatex-command-alist
   '(("d" "Insert derivative" "\\frac{\\mathrm{d}?}{\\mathrm{d}}" cdlatex-position-cursor nil nil t)
     ("p" "Insert partial derivative" "\\frac{\\partial ?}{\\partial}" cdlatex-position-cursor nil nil t))))

;; ** TODO: Custom latex macros
;; Look into using this https://www.reddit.com/r/orgmode/comments/7u2n0h/tip_for_defining_latex_macros_for_use_in_both/

;; * -*-
;; Local Variables:
;; eval: (outline-minor-mode)
;; outline-regexp: ";; [*]+"
;; End:
