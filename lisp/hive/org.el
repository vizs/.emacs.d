;; -*- lexical-binding: t; -*-

;; Enable structure template completion
(require 'org-tempo)

;; Latex equation* template
(tempo-define-template
 "org-latex-equation*"
 '("\\begin{equation*}" n "\\begin{split}" n r n
   "\\end{split}" n "\\end{equation*}" >)
 "<eq")

(add-to-list 'org-tempo-tags '("<eq" . tempo-template-org-latex-equation*))

;; Scale up the org-latex-preview images
(plist-put org-format-latex-options :scale 1.5)

;; Decrease cdlatex popup helper timeout
(setq-default cdlatex-auto-help-delay 0.50)

;; Add a 'cdlatex' startup option to autostart `org-cdlatex-mode'
(add-to-list 'org-startup-options '("cdlatex" org-cdlatex-mode t))

;; Completey hide headline stars
(use-package org-starless
  :straight (:type git :host github :repo "TonCherAmi/org-starless")
  :defer t
  :hook (org-mode . org-starless-mode))

;; Instead of headline stars, `org-num-mode' is better
(add-hook 'org-mode-hook #'org-num-mode)

(use-package org-padding
  :straight (:type git :host github :repo "TonCherAmi/org-padding")
  :defer t
  :hook (org-mode . org-padding-mode)
  :config
  (setq
   ;; org-padding-heading-padding-alist '((2.0 . 1.5) (2.0 . 1.25) (2.0 . 1.15)
   ;;                                     (2.0 . 1.0) (2.0 . 1.0) (2.0 . 1.0)
   ;;                                     (2.0 . 1.0) (2.0 . 1.0))
   org-padding-block-end-line-padding '(2.0 . 1.0)
   org-padding-block-begin-line-padding '(2.0 . 1.0)))

;; Use a variable pitch font for most things
(add-hook 'org-mode-hook
          (defun vz/org-mode-setup-buffer-face ()
            (setq
             buffer-face-mode-face `(:family ,vz/variable-font :height 120))
            (buffer-face-mode)))

;; Some faces has to be monospace!
(let ((faces '(org-table org-link org-code org-block org-drawer
               org-date org-special-keyword org-verbatim org-tag
               org-latex-and-related)))
  (vz/set-monospace-faces faces)
  ;; Adjust font size to be closer to that of the variable font
  (-each faces (fn (set-face-attribute <> nil :height 102))))

;; Let latex stuff be monospace too!
(setq org-highlight-latex-and-related '(latex entities))

;; Make headline larger and bold and decrease the height of `org-num-face'
(-each org-level-faces (fn (set-face-attribute <> nil :weight 'bold :slant 'normal)))
(let* ((height0 (+ 120 40))
       (height1 (+ 120 30))
       (height2 (+ 120 25))
       (height3 (+ 120 20)))
  (set-face-attribute 'org-level-1 nil :height height0)
  (set-face-attribute 'org-level-2 nil :height height1)
  (set-face-attribute 'org-level-3 nil :height height2)
  (set-face-attribute 'org-level-4 nil :height height3)
  (setq org-num-face '(:height 110 :weight bold)))

;; Better tables
(use-package valign
  :straight (:type git :host github :repo "casouri/valign")
  :defer t
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))
;; Toggle pretty entities
;; Hopefully this gets merged or something https://orgmode.org/list/CAGEgU=j+UJoWwoRKChkVxN5dmwbD4YaNTWdLS6Qgj57osZLRJA@mail.gmail.com/
(setq org-pretty-entities t
      org-pretty-entities-include-sub-superscripts t
      org-hide-emphasis-markers t
      org-fontify-whole-heading-line t
      org-fontify-quote-and-verse-blocks t)

;; Customise random faces
(set-face-attribute 'org-quote nil :family vz/variable-font :slant 'italic)
(let ((height 100))
  (set-face-attribute 'org-block-begin-line nil :height height :weight 'bold)
  (set-face-attribute 'org-block-end-line nil :height height :weight 'bold))

;; Enable electric-pair mode and add $ to the list
(add-hook 'org-mode-hook
          (defun vz/org-mode-setup-electric-pair-mode ()
            (electric-pair-local-mode t)
            (setq-local
             electric-pair-inhibit-predicate
             `(lambda (c) (if (char-equal c ?<) t
                           (,electric-pair-inhibit-predicate c)))
             electric-pair-pairs (cons '(?$ . ?$) electric-pair-pairs))))

;; Turn on `org-indent-mode'
(setq org-startup-indented t)

;; Misc settings
(setq
 ;; For whitespace sensiitive languages
 org-src-preserve-indentation t

 ;; Path to various stuff
 org-directory (~ "doc/org")
 org-default-notes-file (~ "doc/org/notes.org")
 org-preview-latex-process-alist (~ ".cache/org-ltximg")

 ;; Timestamp format
 org-time-stamp-formats '("<%A, %d %B, %Y>" . "<%A, %d %B, %Y %k:%M>")

 org-preview-latex-default-process 'dvisvgm)

(setq-default org-display-custom-times t)

;; Capture templates
(use-package doct
  :config
  (setq org-capture-templates
        (doct `(("Dump links, book names, w/e"
                 :keys "d"
                 :file "dump.org"
                 :prepend t
                 :template ("* %{todo-state} %{description}"
                            ":PROPERTIES:"
                            ":type: %{typ}"
                            ":added: %U"
                            "%?")
                 :children (("Anime/Manga" :keys "a"
                             :todo-state "TODO"
                             :description "%^{Name|}"
                             :typ "%^{Is it|anime|manga}")
                            ("Emacs" :keys "e"
                             :todo-state "TODO"
                             :description "%^{What is it?|}"
                             :typ "emacs")
                            ("Quote" :keys "q"
                             :template ("* %^{Quote|Quote %U|}"
                                        ":PROPERTIES:"
                                        ":type: quote"
                                        ":END:"
                                        "#+begin_quote\n%?\n-- %^{Author|}\n#+end_quote"))
                            ("Music" :keys "m"
                             :todo-state ""
                             :description "%^{Name|}"
                             :typ "music")
                            ("Others" :keys "o"
                             :todo-state "TODO"
                             :description "%^{Description|}"
                             :typ "%^{Type|article|book|misc}")))
                ("University Schedule for quizzes and assignments"
                 :file ,(~ "doc/uni/schedule.org")
                 :olp ("Semester I" "Changes/Quizzes/Assignments")
                 :prepend t
                 :type item
                 :keys "u"
                 :template ("- %(org-time-stamp '(1)) :: %?"))))))

;; Setup `counsel-org-goto'
(defun vz/counsel-org-goto ()
  (interactive)
  (counsel-org-goto)
  (vz/beacon-highlight))

(vz/bind
 :map org-mode-map
 "C-c j" #'vz/counsel-org-goto)
