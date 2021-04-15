;; -*- lexical-binding: t;

;; TODO: Add functions to search through notes and add
;; capture-templates to insert notes.

;; TODO: Slurping like paredit inside math environments would be nice

;; TODO: pdf-annot delete last annotation like Okular's C-z

;; TODO: Adding math units to expand-region try list would be nice but
;; I'm not sure how to write this, it's really confusing.

;; TODO: Consider completely removing cdlatex and using
;; https://github.com/ymarco/auto-activating-snippets instead

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
   (seq-map #'(lambda (x)
                (when-let ((func (get-text-property 0 'func x))
                           (_ (-contains? vz/pdf-annot-highlight-functions func)))
                 (put-text-property 0 (length x) 'display
                  (vz/mode-line-roundise-text (symbol-name func)
                   (unless (eq func vz/pdf-annot-highlight-function-on-mouse-release)
                    vz/mode-line-fgi)
                   nil t) x))
                x)
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
  (seq-map #'(lambda (x)
               (propertize
                (format " %s " (car x))
                'display (vz/mode-line-roundise-text (symbol-name (car x)) vz/mode-line-fgi nil t)
                'func (car x)
                'local-map
                (let ((map (make-sparse-keymap)))
                 (define-key map [header-line mouse-1]
                   `(lambda (_)
                      (interactive "e")
                      ,(if (member (car x) vz/pdf-annot-highlight-functions)
                           `(vz/pdf-annot-update-highlight-function ',(car x))
                         `(call-interactively ,(cdr x)))))
                 map)))
        vz/pdf-annot-header-line-functions))

(add-hook 'pdf-annot-minor-mode-hook
          (defun vz/pdf-annot--set-header-line-format ()
            (face-remap-add-relative 'header-line '(:inherit mode-line))
            (let* ((images (vz/pdf-annot-header-line--generate-images))
                   (length (seq-reduce #'(lambda (x res) (+ (length res) x))
                                       images 0)))
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
;; some old books that had an empty page after every page so that students
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
   org-noter-notes-search-path (seq-map #'(lambda (x) (~ x)) '("doc/org" "doc/uni/notes"))
   org-noter-doc-property-in-notes t)
  ;; I really don't need org-noter to add stuff to my modeline
  (advice-add 'org-noter--mode-line-text :override #'(lambda () ""))
  (vz/bind
   :map org-mode-map
   "C-c n" #'org-noter
   :map pdf-view-mode-map
   "C-c n" #'org-noter))

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
  (vz/bind
   :map org-cdlatex-mode-map
   "`" nil
   ";" #'(lambda ()
           (interactive)
           (if (texmathp)
               (cdlatex-math-symbol)
             (let (org-cdlatex-mode)
              (call-interactively (key-binding (vector last-input-event)))))))
  (setq
   ;; Don't use dollar
   cdlatex-use-dollar-to-ensure-math nil
   cdlatex-math-symbol-prefix ?\;
   cdlatex-command-alist
   '(("dv" "Insert derivative" "\\frac{\\mathrm{d}?}{\\mathrm{d}}" cdlatex-position-cursor nil nil t)
     ("pv" "Insert partial derivative" "\\frac{\\partial ?}{\\partial }" cdlatex-position-cursor nil nil t)
     ("pv(" "Insert partial derivative" "\\left(\\frac{\\partial ?}{\\partial }\\right)" cdlatex-position-cursor nil nil t)
     ("txt" "Insert \\intertext{}" "\\intertext{?}" cdlatex-position-cursor nil nil t)
     ("lim" "Insert limit" "\\lim_{?}" cdlatex-position-cursor nil nil t)
     ("cc" "Insert the concentration of substance" "[\\ch{?}] " cdlatex-position-cursor nil nil t)
     ("ch" "Insert the chemical formula" "\\ch{?}" cdlatex-position-cursor nil nil t)
     ("intl" "Insert integral with limits" "\\int_{?}^{}" cdlatex-position-cursor nil nil t)
     ("1/" "Insert the inverse of" "\\frac{1}{?}" cdlatex-position-cursor nil nil t))
   cdlatex-math-symbol-alist '((?0 ("\\ominus" "\\circ"))))
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
     #'(lambda (x) (or (null x) (stringp x))))

;; TODO: Maybe I should change this to an around advice instead?
;; Context: info page on abbrev-mode
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

;; TODO: Can this be a local variable?
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
    ("\\[" . "\\]")
    ("\\begin{align*}\n" . "\n\\end{align*}")))

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
(defun vz/change-latex-pair-region (beg end matchers direction)
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

;; Because marking is too much effort!
(defun vz/change-latex-pair-around-point (matchers direction)
  (letrec ((total-matches (length matchers))
           (loop
            (lambda (n matches)
              (unless (null matches)
                (let ((length-start (length (caar matches)))
                      (length-end   (length (cdar matches))))
                  (if (and (s-equals? (buffer-substring (point) (+ (point) length-end))   (cdar matches))
                           (s-equals? (buffer-substring (point) (- (point) length-start)) (caar matches)))
                      (progn
                        (backward-delete-char length-start)
                        (delete-char length-end)
                        (let ((sur (nth (% (+ n (if direction 1 -1)) total-matches) matchers))
                              (point (point)))
                          (insert (car sur))
                          (insert (cdr sur))
                          (goto-char (- (point) (length (cdr sur))))
                          t))
                    (funcall loop (1+ n) (cdr matches))))))))
    (funcall loop 0 matchers)))

(defun vz/change-latex-equation-pair (arg)
  "Switch through available pairs in `vz/latex-equation-pairs'."
  (interactive "P")
  (if (use-region-p)
      (vz/change-latex-pair-region (region-beginning) (region-end) vz/latex-equation-pairs (not arg))
    (vz/change-latex-pair-around-point vz/latex-equation-pairs (not arg))))

(defun vz/change-latex-parens-pair (arg)
  "Switch through available pairs in `vz/latex-paren-pairs'."
  (interactive "P")
  (if (use-region-p)
      (vz/change-latex-pair-region (region-beginning) (region-end) vz/latex-paren-pairs (not arg))
    (vz/change-latex-pair-around-point vz/latex-paren-pairs (not arg))))

(add-hook 'org-metaright-hook
          (defun vz/org-latex-change-pair-metaright ()
            (when org-cdlatex-mode
              (if (vz/change-latex-equation-pair '())
                  t
                (vz/change-latex-parens-pair '())))))

(add-hook 'org-metaleft-hook
          (defun vz/org-latex-change-pair-metaleft ()
            (when org-cdlatex-mode
              (if (vz/change-latex-equation-pair '(t))
                  t
                (vz/change-latex-parens-pair '(t))))))

;; * Smart delete and kill line

;; Pairs to look for is already defined in `vz/latex-paren-pairs' and
;; `vz/latex-equation-pairs'. But I'd also like to handle \\ and
;; auto-delete & when you're deleting =, >, <.

(defvar vz/latex-smart-delete-pairs-start
  (seq-sort-by #'cdr #'>
               (seq-map #'(lambda (p) (cons (regexp-quote (car p)) (length (car p))))
                        (append vz/latex-paren-pairs vz/latex-equation-pairs
                                '(("{" . "}")))))
  "The definition is awfully complicated since I wanted to save
time by doing some expensive processes beforehand.")

(defvar vz/latex-smart-delete-pairs-end
  (seq-sort-by #'cdr #'>
               (seq-map #'(lambda (p) (cons (regexp-quote (cdr p)) (length (cdr p))))
                        (append vz/latex-paren-pairs vz/latex-equation-pairs
                                '(("{" . "}"))))))

(defun vz/latex-smart-delete--find-pair-around-point (direction point pair-starts pair-ends)
  "Find the pair around point. POINT is the point to search
around, PAIR-STARTS and PAIR_ENDS are list of pair of starting
and ending string of pairs and their length. See the definition
of `vz/latex-smart-delete-pairs-start'. Returns (point-position
pair-start pair-end) where point-position can be between, start
or end; nil otherwise. If DIRECTION is positive, then it can be
between or start. If DIRECTION is negative, then it can be
between or end."
  (unless (null pair-starts)
    (pcase-let ((`(,start . ,len-start) (car pair-starts))
                (`(,end   . ,len-end)   (car pair-ends)))
      (cond
       ;; If we are looking at END straight ahead, then see if the
       ;; text behind the point is START
       ((and (looking-at-p end) (looking-back start len-start))
        (list 'between (car pair-starts) (car pair-ends)))
       ;; See if we are looking at START and END straight ahead with
       ;; no space in between
       ((and (> direction 0) (looking-at-p (concat start end)))
        (list 'start (car pair-starts) (car pair-ends)))
       ;; See if we are after START and END pairs i.e., START_END|
       ;; where | is the point and _ is just a visual indicator
       ((and (< direction 0) (looking-back (concat start end) (+ len-start len-end)))
        (list 'end (car pair-starts) (car pair-ends)))
       (t
        (vz/latex-smart-delete--find-pair-around-point direction
                                                       point
                                                       (cdr pair-starts)
                                                       (cdr pair-ends)))))))

(defun vz/latex-smart-delete-char (n)
  "This function should, hopefully, find the approriate
``proper'' pair around point and delete it. Pairs' start and
end are defined in `vz/latex-smart-delete-pairs-start' and
`vz/latex-smart-delete-pairs-end' respectively.

In addition to that, it should also delete
1. ^,_ before {}
2. &=, &>, &<
3. \\.

A ``proper'' pair is defined as ``<pair_start><pair_end>''."
  (interactive "p")
  (when (> (abs n) 0)
    (let ((pair
           (vz/latex-smart-delete--find-pair-around-point (point) 1
                                                          vz/latex-smart-delete-pairs-start
                                                          vz/latex-smart-delete-pairs-end)))
      (if (null pair)
          (delete-char (if (looking-at-p (rx (or "&=" "&>" "&<" "\\")))
                           2
                         1))
        (pcase-let ((`(,ppos (,start . ,len-start) (,end . ,len-end)) pair))
          (pcase ppos
            ('between
             (backward-char len-start)
             (delete-char (+ len-start len-end)))
            ('start
             (delete-char (+ len-start len-end))))
          ;; If ^,_ present before {, then delete it
          (when (seq-contains '(?_ ?^) (char-before) #'char-equal)
            (delete-backward-char 1)))))
    (vz/latex-smart-delete-char (1- n))))

(defun vz/latex-smart-delete-backward-char (n)
  "Like `vz/latex-smart-delete-char' but backwards."
  (interactive "p")
  (when (> (abs n) 0)
    (let ((pair
           (vz/latex-smart-delete--find-pair-around-point (point) -1
                                                          vz/latex-smart-delete-pairs-start
                                                          vz/latex-smart-delete-pairs-end)))
      (if (null pair)
          (delete-backward-char (if (looking-back (rx (or "&=" "&>" "&<" "\\")) 2)
                                    2
                                  1))
        (pcase-let ((`(,ppos (,start . ,len-start) (,end . ,len-end)) pair))
          (pcase ppos
            ('between
             (backward-char len-start)
             (delete-char (+ len-start len-end)))
            ('end
             (delete-backward-char (+ len-start len-end))))
          ;; If ^,_ present before {, then delete it
          (when (seq-contains '(?_ ?^) (char-before) #'char-equal)
            (delete-backward-char 1)))))
    (vz/latex-smart-delete-backward-char (1- n))))

;; This command looks for the closest starting pair backwards, then tries
;; to find the corresponding closest ending pair and deletes everything
;; from point till the start of the ending pair.

(defun vz/latex-smart-kill ()
  (interactive)
  (if-let ((points (vz/filter-map
                    #'(lambda (x) (when (looking-back (format "%s.*?" (car x)))
                                   (cons (match-beginning 0) (car x))))
                    vz/latex-smart-delete-pairs-start)))
      (pcase-let* ((`(_ . ,start-pair) (car (seq-sort-by #'car #'< points)))
                   (`(,end-pair . ,length) (nth (vz/find-index #'(lambda (x) (s-equals? (car x) start-pair))
                                                               vz/latex-smart-delete-pairs-start)
                                                vz/latex-smart-delete-pairs-end)))
        (if (looking-at (format ".*?%s" end-pair))
            (delete-char (- (match-end 0) (point) length))
          (kill-line)))
    (kill-line)))

;; ** Bindings
;; This is bound to break innit?

(vz/bind
 :map org-cdlatex-mode-map
 ;; TODO: remap does not work
 "C-d" (defun vz/org-cdlatex-smart-delete-char (N)
         (interactive "p")
         (if (texmathp)
             (vz/latex-smart-delete-char N)
           (org-delete-char N)))
 "C-k" (defun vz/org-cdlatex-smart-kill-line (&optional arg)
         (interactive "p")
         (if (texmathp)
             (vz/latex-smart-kill)
           (org-kill-line arg)))
 "C-w" (defun vz/org-cdlatex-smart-delete-backward-char (&optional arg)
         (interactive "p")
         (if (and (not (use-region-p)) (texmathp))
             (vz/latex-smart-delete-backward-char arg)
           (let (org-cdlatex-mode)
             (call-interactively (key-binding (vector last-input-event)))))))

;; * -*-*-*-
;; Local Variables:
;; eval: (outline-minor-mode)
;; outline-regexp: ";; [*]+"
;; End:
