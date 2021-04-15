;; -*- lexical-binding: t; -*-

;; Enable structure template completion
(require 'org-tempo)

;; Latex equation* template
(tempo-define-template
 "org-latex-equation*"
 '("\\begin{equation*}" n "\\begin{alignat}" n r n
   "\\end{alignat}" n "\\end{equation*}" >)
 "<eq")

(add-to-list 'org-tempo-tags '("<eq" . tempo-template-org-latex-equation*))

;; Scale up the org-latex-preview images
(plist-put org-format-latex-options :scale 1.5)
;; Make it completely black
(plist-put org-format-latex-options :foreground "Black")

(setq org-entities-user (append '(("ominus" "\\ominus" t "o" "o" "o" "⊖")
                                  ("vdots" "\\vdots" t "&x2999" "..." "..." "⋮")
                                  ("ddots" "\\ddots" t "&x22F1" "..." "..." "⋱"))
                              org-entities-user))

(use-package cdlatex
  :demand t
  :config
  ;; Decrease cdlatex popup helper timeout
  (setq-default cdlatex-auto-help-delay 0.50)
  (setq
   ;; +Disable simplification of super- and sub-scripts+
   cdlatex-simplify-sub-super-scripts t
   ;; Romanise sub/superscript if _,^ is pressed twice
   cdlatex-make-sub-superscript-roman-if-pressed-twice t))

;; Add a 'cdlatex' startup option to autostart `org-cdlatex-mode'
(add-to-list 'org-startup-options '("cdlatex" org-cdlatex-mode t))

;; Completey hide headline stars
(use-package org-starless
  :straight (:type git :host github :repo "TonCherAmi/org-starless")
  :defer t
  :hook (org-mode . org-starless-mode))

;; Instead of headline stars, `org-num-mode' is better
(add-hook 'org-mode-hook #'org-num-mode)

;; native uses auctex highlighting?
(setq org-highlight-latex-and-related '(latex))

(defun vz/org-style ()
  (interactive)

  (setq-local buffer-face-mode-face `(:family ,vz/variable-font :height 120)
              line-spacing 0.1)
  (buffer-face-mode)

  (seq-each (lambda (x) (face-remap-add-relative x :weight 'bold :slant 'normal))
            org-level-faces)

  (let ((faces '(org-link org-code org-block org-drawer
                 org-date org-special-keyword org-verbatim org-tag)))
    (seq-each (lambda (x)
                (face-remap-add-relative x :height 102
                                         :family vz/monospace-font))
              faces))
  (face-remap-add-relative 'org-latex-and-related
                           :family "IBM Plex Mono"
                           :slant 'italic
                           :height 102)

  (face-remap-add-relative 'org-quote :family vz/variable-font :slant 'italic)

  (let ((height 100))
    (face-remap-add-relative 'org-block-begin-line :height height :weight 'bold)
    (face-remap-add-relative 'org-block-end-line   :height height :weight 'bold))

  (let* ((height0 (+ 120 40))
         (height1 (+ 120 30))
         (height2 (+ 120 25))
         (height3 (+ 120 20)))
    (face-remap-add-relative 'org-level-1 :height height0)
    (face-remap-add-relative 'org-level-2 :height height1)
    (face-remap-add-relative 'org-level-3 :height height2)
    (face-remap-add-relative 'org-level-4 :height height3)

    (setq-local org-num-face `(:family ,vz/variable-font :height 110 :weight bold
                               :slant normal))))

(add-hook 'org-mode-hook #'vz/org-style)

;; Better tables
(use-package valign
  :straight (:type git :host github :repo "casouri/valign")
  :defer t
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; Hide emphasis markers
(setq org-hide-emphasis-markers t
      org-fontify-quote-and-verse-blocks t)

;; Turn on `org-indent-mode'
(setq org-startup-indented t)

;; Misc settings
(setq
 ;; For whitespace sensiitive languages
 org-src-preserve-indentation t

 org-src-fontify-natively nil

 ;; Path to various stuff
 org-directory (~ "doc/org")
 org-default-notes-file (~ "doc/org/notes.org")
 org-preview-latex-image-directory (~ ".cache/org-ltximg/")

 ;; Timestamp format
 org-time-stamp-custom-formats '("<%A, %d %B, %Y>" . "<%A, %d %B, %Y %H:%M>")

 org-preview-latex-default-process 'dvisvgm

 ;; From org 9.3, it is set to 'show-everything
 org-startup-folded t

 ;; Avoid editing invisible part
 org-catch-invisible-edits 'show-and-error

 org-babel-load-languages '((emacs-lisp . t) (C .t)))

(setq-default org-display-custom-times t)

(org-babel-do-load-languages 'org-babel-load-languages
                             org-babel-load-languages)

;; Capture templates
(vz/use-package doct "captures")

(add-hook 'org-capture-after-finalize-hook
          (defun vz/kill-frame-after-org-capture-script ()
            (when (s-equals? (frame-parameter (selected-frame) 'name)
                             "vz/org-capture-frame")
              (delete-frame (selected-frame)))))

;; Setup `counsel-org-goto'
(defun vz/counsel-org-goto ()
  (interactive)
  (counsel-org-goto)
  (vz/beacon-highlight)
  (org-show-entry))

(vz/bind
 "C-x C-l" #'org-store-link
 :map org-mode-map
 "C-c j" #'vz/counsel-org-goto)

;; Advice around `org-set-tags-command' to use `counsel-org-tag'
(advice-add 'org-set-tags-command
            :override #'(lambda (&optional arg) (counsel-org-tag)))

;; Instead of completing removing _, ^, { and } when setting fancy
;; super-sub-script display, give it a new face and "prettify"
;; it too.

(setq org-pretty-entities-include-sub-superscripts t)

(defface vz/org-script-markers '((t :inherit shadow))
  "Face to be used for sub/superscripts markers i.e., ^, _, {, }.")

(defun vz/org-raise-scripts (limit)
  "Add raise properties to sub/superscripts but don't remove the
markers for sub/super scripts but fontify them."
  (when (and org-pretty-entities-include-sub-superscripts
	           (re-search-forward
	            (if (eq org-use-sub-superscripts t)
		              org-match-substring-regexp
		            org-match-substring-with-braces-regexp)
	            limit t))
    (let* ((pos (point)) table-p comment-p
	         (mpos (match-beginning 3))
	         (emph-p (get-text-property mpos 'org-emphasis))
	         (link-p (get-text-property mpos 'mouse-face))
	         (keyw-p (eq 'org-special-keyword (get-text-property mpos 'face))))
      (goto-char (point-at-bol))
      (setq table-p (looking-at-p org-table-dataline-regexp)
	          comment-p (looking-at-p "^[ \t]*#[ +]"))
      (goto-char pos)
      ;; Handle a_b^c
      (when (member (char-after) '(?_ ?^)) (goto-char (1- pos)))
      (unless (or comment-p emph-p link-p keyw-p)
	      (put-text-property (match-beginning 3) (match-end 0)
			                     'display
			                     (if (equal (char-after (match-beginning 2)) ?^)
			                         (nth (if table-p 3 1) org-script-display)
			                       (nth (if table-p 2 0) org-script-display)))
	      (put-text-property (match-beginning 2) (match-end 2)
                           'face 'vz/org-script-markers)
	      (when (and (eq (char-after (match-beginning 3)) ?{)
		               (eq (char-before (match-end 3)) ?}))
	        (put-text-property (match-beginning 3) (1+ (match-beginning 3))
			                       'face 'vz/org-script-markers)
	        (put-text-property (1- (match-end 3)) (match-end 3)
			                       'face 'vz/org-script-markers)))
      t)))

(advice-add 'org-raise-scripts :override
            #'vz/org-raise-scripts)

;; `org-pretty-entities' is quite tedious to use compared to
;; `prettify-symbols-mode'. That is, you can't unprettify the symbol
;; when the cursor is over it. There's a patch[1] to do it but it hasn't
;; been merged yet. Until then, I'm going to make a subsitute using
;; `prettify-symbols-mode'.
;; 1. https://orgmode.org/list/CAGEgU=j+UJoWwoRKChkVxN5dmwbD4YaNTWdLS6Qgj57osZLRJA@mail.gmail.com

(defvar vz/org-prettify-symbols nil
  "Alist for `prettify-symbols-mode'.")

(defun vz/org-prettify--set-prettify-symbols-alist ()
  (dolist (entity (append org-entities-user org-entities))
    (when (listp entity)              ; `org-entities' has strings too
      (when-let* ((match-for (car entity))
                  (replace-with (car (last entity)))
                  (_ (= 1 (length replace-with))))
        (add-to-list 'vz/org-prettify-symbols
                     (cons (concat "\\" match-for) replace-with))))))

(with-eval-after-load 'org
  (vz/org-prettify--set-prettify-symbols-alist))

(defun vz/org-prettify--predicate (_start end _match)
  ;; There's no need the check the character before the entity match
  ;; since all of them start with \. The characters that are
  ;; acceptable after the match are mathmetical operators and some
  ;; special characters.
  (seq-contains-p '(?\C-j ?} ?{ ?\\ ?_ ?- ?+ ?^ ?\( ?\) ?$ ? ?/ ?| ?.)
                  (char-after end)))

(define-minor-mode vz/org-prettify-mode
  "When non-nil, use `prettify-symbols-mode' to prettify
  `org-entities-user'."
  nil nil nil
  (if vz/org-prettify-mode
      (progn
        (setq-local prettify-symbols-compose-predicate #'vz/org-prettify--predicate
                    prettify-symbols-unprettify-at-point 'right-edge
                    prettify-symbols-alist vz/org-prettify-symbols)
        (prettify-symbols-mode t))
    (setq-local prettify-symbols-alist nil
                prettify-symbols-unprettify-at-point nil
                prettify-symbols-compose-predicate #'prettify-symbols-default-compose-p)
    (prettify-symbols-mode -1)))

(add-hook 'org-mode-hook #'vz/org-prettify-mode)

;; Load in notes.el
(load-file (expand-file-name "lisp/hive/notes.el" user-emacs-directory))

;; Special C-{a,e} movements
(setq org-special-ctrl-a/e t)

(with-eval-after-load 'org
  (when org-special-ctrl-a/e
    (vz/bind
     :map org-mode-map
     [remap vz/beginning-of-line] #'org-beginning-of-line)))

;; This is convenient to have
(use-package math-delimiters
  :straight (:type git :host github :repo "oantolin/math-delimiters")
  :config
  (vz/bind
   :map org-cdlatex-mode-map
   "$" #'math-delimiters-insert))

;; Agenda
(with-eval-after-load 'org-agenda
  (load-file (expand-file-name "lisp/hive/agenda.el" user-emacs-directory)))
