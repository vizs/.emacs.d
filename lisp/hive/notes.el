;; -*- lexical-binding: t;

;; TODO: Add functions to search through notes and add
;; capture-templates to insert notes.
;;
;; TODO: Slurping and kill-line like paredit inside math
;; environments would be nice
;;
;; TODO: Inside math environment, when I remove { and if the previous
;; character is ^ or _, I would like to remove {, } /and/ _ or ^
;;
;; TODO: pdf-annot delete last annotation like Okular's C-z

;; * Annotations

;; This file is mostly to do with PDF annotations. This will be
;; achieved using pdftools and org-noter + org-pdftools.

(use-package pdf-tools
  :config
  (pdf-tools-install))

;; ** Set `header-line-format'
;; Highlighting text in pdf-tools is really tedious. I
;; prefer ephiphany's way of highlighting where you click a button and
;; whenever you drag and release your mouse, it highlights the
;; text. So, here's my attempt at making it.

(defvar vz/pdf-annot-highlight-functions
  '(underline
    highlight)
  "A list of description of highlight functions.")

(defvar vz/pdf-annot-header-line-functions
  '((text . #'pdf-annot-add-text-annotation)
    (highlight . #'pdf-annot-add-highlight-markup-annotation)
    (underline . #'pdf-annot-add-underline-markup-annotation)
    (remove . #'pdf-annot-delete))
  "An alist of annotation functions that are to be run when clicking on it.
Highlight functions are handled specially.")

(defvar-local vz/pdf-annot-highlight-function-on-mouse-release nil
  "Highlight function to be run when releasing the mouse.")

(define-minor-mode vz/pdf-annot-highlight-on-mouse-release-mode
  "Runs `vz/pdf-annot-highlight-function-on-mouse-release' after
  `pdf-view-mouse-set-region' when active."
  nil nil nil
  (if vz/pdf-annot-highlight-on-mouse-release-mode
      (advice-add 'pdf-view-mouse-set-region :after
                  (defun vz/pdf-annot-highlight-on-mouse-release (&rest _)
                    (when (and vz/pdf-annot-highlight-on-mouse-release-mode
                               pdf-annot-minor-mode
                               (not (null vz/pdf-annot-highlight-function-on-mouse-release)))
                      ;; TODO: Find out how to remove eval
                      (eval `(call-interactively
                              ,(asoc-get vz/pdf-annot-header-line-functions
                                vz/pdf-annot-highlight-function-on-mouse-release))))))
    (advice-remove 'pdf-view-mouse-set-region #'vz/pdf-annot-highlight-on-mouse-release)))

(defun vz/pdf-annot-header-line--update-images ()
  (setq-local
   header-line-format
   (-map (fn (when-let ((func (get-text-property 0 'func <>))
                        (_ (-contains? vz/pdf-annot-highlight-functions func)))
               (put-text-property
                0 (length <>)
                `display
                (vz/mode-line-roundise-text (symbol-name func)
                                             (unless (eq func vz/pdf-annot-highlight-function-on-mouse-release)
                                               vz/mode-line-fgi)
                                             nil t) <>))
             <>)
         header-line-format))
  (force-mode-line-update))

(defun vz/pdf-annot-update-highlight-function (fn)
  (if vz/pdf-annot-highlight-on-mouse-release-mode
      (progn
        (message "%s" vz/pdf-annot-highlight-function-on-mouse-release)
        (setq-local vz/pdf-annot-highlight-function-on-mouse-release
                    (unless (eq vz/pdf-annot-highlight-function-on-mouse-release fn)
                      fn))
        (vz/pdf-annot-header-line--update-images))
    (eval `(call-interactively ,(asoc-get vz/pdf-annot-header-line-functions fn)))))

(defun vz/pdf-annot-header-line--generate-images ()
  (-map (fn (propertize
             (format " %s " (car <>))
             'display (vz/mode-line-roundise-text (symbol-name (car <>)) vz/mode-line-fgi nil t)
             'func (car <>)
             'local-map
             (let ((map (make-sparse-keymap)))
               (define-key map [header-line mouse-1]
                 `(lambda (_)
                    (interactive "e")
                    ,(if (member (car <>) vz/pdf-annot-highlight-functions)
                         `(vz/pdf-annot-update-highlight-function ',(car <>))
                       `(call-interactively ,(cdr <>)))))
               map)))
        vz/pdf-annot-header-line-functions))

(add-hook 'pdf-annot-minor-mode-hook
          (defun vz/pdf-annot--set-header-line-format ()
            (face-remap-add-relative 'header-line '(:inherit mode-line))
            (let* ((images (vz/pdf-annot-header-line--generate-images))
                   (length (-reduce-from (fn (+ (length <2>) <1>))
                                         0 images)))
              (setq-local header-line-format
                          (cons (propertize " " 'display `((space :align-to (- center (0.5 . ,length))))
                                            'face 'header-line)
                                images)))))

(add-hook 'pdf-annot-minor-mode-hook #'vz/pdf-annot-highlight-on-mouse-release-mode)

;; ** Remove file status from mode-line and page number
;; The file status is always going to be the same so having it is pointless.
;; Page number is handy to have though.

(defun vz/pdf-tools-mode-line--get-page-number ()
  (let ((string (format "%d/%d" (pdf-view-current-page) (pdf-cache-number-of-pages))))
    (concat " "
            (propertize string
                        'display (vz/mode-line-roundise-text string nil nil t)
                        'keymap (make-mode-line-mouse-map
                                 'mouse-1
                                 (lambda (_) (interactive "e")
                                   (call-interactively #'pdf-view-goto-page)))))))

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
   org-noter-notes-search-path (-map (fn (~ <>)) '("doc/org" "doc/uni/notes"))
   org-noter-doc-property-in-notes t)
  ;; I really don't need org-noter to add stuff to my modeline
  (advice-add 'org-noter--mode-line-text :override (fn "")))

;; TODO: org-noter-pdftools doesn't really like to work. It starts
;; asking stuff when I enable org-noter.
;; It somehow makes emacs segfault?
(use-package org-pdftools
  :after pdf-tools
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

(use-package cdlatex
  :config
  (setq
   ;; Don't use dollar
   cdlatex-use-dollar-to-ensure-math nil
   cdlatex-command-alist
   '(("dv" "Insert derivative" "\\frac{\\mathrm{d}?}{\\mathrm{d}}" cdlatex-position-cursor nil nil t)
     ("pv" "Insert partial derivative" "\\frac{\\partial ?}{\\partial }" cdlatex-position-cursor nil nil t)
     ("txt" "Insert \\intertext{}" "\\intertext{?}" cdlatex-position-cursor nil nil t)
     ("lim" "Insert limit" "\\lim_{?}" cdlatex-position-cursor nil nil t)
     ("cc" "Insert the concentration of substance" "[\\ch{?}] " cdlatex-position-cursor nil nil t)
     ("ch" "Insert the chemical formula" "\\ch{?}" cdlatex-position-cursor nil nil t)
     ("intl" "Insert integral with limits" "\\int_{?}^{}" cdlatex-position-cursor nil nil t)
     ("1/" "Insert the inverse of" "\\frac{1}{?}" cdlatex-position-cursor nil nil t)))
  (cdlatex-compute-tables))

;; ** TODO: Custom latex macros
;; Look into using this https://www.reddit.com/r/orgmode/comments/7u2n0h/tip_for_defining_latex_macros_for_use_in_both/

;; * Racket-plot template
;; I prefer to use racket for drawing plots.

(use-package ob-racket
  :demand t
  :straight (:host github :repo "hasu/emacs-ob-racket")
  :config
  ;; This will do the boilerplate
  (tempo-define-template
   "org-racket-plot"
   '("#+begin_src racket :lang racket/base :require plot :var out-file=\""
     (p "Path to file: ") "\" :results file :exports results" n
     r n
     "(string->symbol out-file)" ; This dirty thing is here because of how write works in racket
     n "#+end_src")
   "<plt")
  (add-to-list 'org-tempo-tags '("<plt" . tempo-template-org-racket-plot))
  (setq tempo-interactive t))

;; * Special abbreviation option
;; The expansions set this way only works in text enviroments

(defvar-local vz/org-abbrev-mode nil
  "Enable abbreviations in org-mode buffer.")

(defvar-local vz/org-abbrev-file nil
  "File in which abbreviations are stored. To be used in
  .dir-locals.el and is relative.")

;; Le not safe but this shouldn't be a problem
(put 'vz/org-abbrev-file 'safe-local-variable
     (fn (or (null <>)
             (stringp <>))))

(defun vz/org-abbrev--expand-function ()
  "When `vz/org-abbrev-mode' is active, then allow abbreviation expansion
only in text environments. Otherwise, the default behaviour is
followed."
  (require 'texmathp)
  (if (and (derived-mode-p 'org-mode)
           vz/org-abbrev-mode)
      (unless (texmathp)
        (abbrev--default-expand))
    (abbrev--default-expand)))

(setq abbrev-expand-function #'vz/org-abbrev--expand-function)

(add-hook 'hack-local-variables-hook
          (defun vz/org-turn-on-abbrev-mode-maybe? ()
            (when (and (derived-mode-p 'org-mode)
                       vz/org-abbrev-mode
                       vz/org-abbrev-file)
              (abbrev-mode t)
              (read-abbrev-file (expand-file-name vz/org-abbrev-file))
              (add-hook 'after-save-hook
                        (defun vz/org-abbrev-mode-hook ()
                          (write-abbrev-file (expand-file-name vz/org-abbrev-file)))
                        nil t))))

(add-to-list 'org-startup-options '("abbrev" vz/org-abbrev-mode t))

;; * Change \(\) to \[\] and shift parenthesis

(defvar vz/latex-equation-pairs
  '(("\\(" . "\\)")
    ("\\[" . "\\]")))

(defvar vz/latex-paren-pairs
  '(("(" . ")")
    ("[" . "]")
    ("\\{" . "\\}")
    ("\\left(" . "\\right)")
    ("\\left[" . "\\right]")
    ("\\left\\{" . "\\right\\}")))

(defun vz/string-begs-and-ends-with? (str s-beg s-end)
  "Does STR begin and end with S-BEG and S-END respectively?"
  (and (s-starts-with? s-beg str)
       (s-ends-with? s-end str)))

;; I only really write racket so...
(defun vz/change-latex-pair (beg end matchers direction)
  (letrec ((text (substring-no-properties (delete-and-extract-region beg end)))
           (total-matches (length matchers))
           (loop (lambda (n matches)
                   (if (null matches)
                       (progn (insert text) nil)
                     (if (vz/string-begs-and-ends-with? text (caar matches) (cdar matches))
                         (let ((sur (nth (% (+ n (if direction 1 -1)) total-matches) matchers))
                               (point (point)))
                           (insert (format "%s%s%s"
                                           (car sur)
                                           (substring text (length (caar matches)) (- (length (cdar matches))))
                                           (cdr sur)))
                           (setq deactivate-mark nil)
                           (set-mark point)
                           t)
                       (funcall loop (1+ n) (cdr matches)))))))
    (funcall loop 0 matchers)))

(defun vz/change-latex-equation-pair (arg)
  "Change \(\) to \[\] and vice-versa."
  (interactive "P")
  (vz/change-latex-pair (region-beginning) (region-end) vz/latex-equation-pairs (not arg)))

(defun vz/change-latex-parens-pair (arg)
  "Change latex parenthesis. See `vz/latex-paren-pairs' for valid parens."
  (interactive "P")
  (vz/change-latex-pair (region-beginning) (region-end) vz/latex-paren-pairs (not arg)))

(add-hook 'org-metaright-hook
          (defun vz/org-latex-change-pair-metaright ()
            (when (region-active-p)
              (if (vz/change-latex-equation-pair '())
                  t
                (vz/change-latex-parens-pair '())))))

(add-hook 'org-metaleft-hook
          (defun vz/org-latex-change-pair-metaleft ()
            (when (region-active-p)
              (if (vz/change-latex-equation-pair '(t))
                  t
                (vz/change-latex-parens-pair '(t))))))

;; * Smart delete and kill line

;; Pairs to look for is already defined in `vz/latex-paren-pairs' and
;; `vz/latex-equation-pairs'. But I'd also like to handle \\ and
;; auto-delete & when you're deleting =, >, <.

(defvar vz/latex-smart-delete-pairs-start (-map (-compose #'regexp-quote #'car)
                                                (append vz/latex-paren-pairs vz/latex-equation-pairs)))

(defvar vz/latex-smart-delete-pairs-end (-map (-compose #'regexp-quote #'cdr)
                                              (append vz/latex-paren-pairs vz/latex-equation-pairs)))

(defvar vz/latex-smart-delete-align-symbols '(?= ?> ?<)
  "Symbols which when deleted also deletes & before the character
  if present.")

(defun vz/latex-smart-delete-char ()
  (interactive)
  (letrec ((loop
            (lambda (pair-starts pair-ends)
              (unless (null pair-starts)
                (let* ((start (car pair-starts))
                       (end (car pair-ends))
                       (len-start (length start))
                       (len-end (length end)))
                  (cond
                   ((and (looking-at-p end)
                         (looking-back start len-start))
                    (backward-char len-start)
                    (delete-char (+ len-start len-end) t)
                    t)
                   ((looking-at-p (concat start end))
                    (delete-char (+ len-start len-end))
                    t)
                   (t
                    (funcall loop (cdr pair-starts) (cdr pair-ends)))))))))
    (unless (funcall loop vz/latex-smart-delete-pairs-start vz/latex-smart-delete-pairs-end) ; Delete empty paren pairs
      (cond
       ((and (char-equal (char-before (point)) ?&) ; If cursor is between & and =/>/<
             (-contains? vz/latex-smart-delete-align-symbols (char-after (point))))
        (backward-char 1)
        (delete-char 2))
       ((and (char-equal (char-after (point)) ?&) ; If cursor is after &{=,>,<}
             (-contains? vz/latex-smart-delete-align-symbols (char-after (1+ (point)))))
        (delete-char 2))
       ((and (-contains? '(?^ ?_) (char-after (point))) ; Slurp out whatever's inside {}, if any
             (save-excursion (forward-char)
                             (char-equal (char-after (point)) ?{)))
        (delete-char 2)
        (when (looking-at ".*}") ; Has to be greedy so we get the last }
          (let ((start (point))) ; (Might be a problem since this only goes as far as EOL)
            (forward-char (- (match-end 0) start 1))
            (delete-char 1)
            (goto-char start))))
       ((funcall loop '("{") '("}"))    ; Remove ^,_ before empty {}
        (when (-contains? '(?^ ?_) (char-before (point)))
          (backward-char 1)
          (delete-char 1)))
       (t (delete-char 1))))))

;; * -*-*-*-
;; Local Variables:
;; eval: (outline-minor-mode)
;; outline-regexp: ";; [*]+"
;; End:
