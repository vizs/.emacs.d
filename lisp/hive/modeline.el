;; -*- lexical-binding: t; -*-

(require 'battery)
(require 'time)
(use-package moody
  :demand t)

(defun vz/mode-line-file-state ()
  (if (buffer-file-name)
      (cond
       (buffer-read-only    " [!]")
       ((buffer-modified-p) " [+]")
       (t                   ""))
    ""))

(defun vz/mode-line-file-short-dir ()
  (if-let ((path (when (derived-mode-p 'comint-mode) (concat default-directory "/a")))
           (dir (let* ((dir (f-split (f-short (f-dirname path))))
                       (dir* (cdr dir)))
                  (if (s-equals? (car dir*) "~") dir* dir))))
      (concat (f-short (apply #'f-join
                              (append (seq-map #'(lambda (f) (substring f 0 1)) (butlast dir))
                                      (last dir))))
              "|")
    ""))

(defun vz/mode-line-git-branch ()
  (if-let ((branch (when (or (buffer-file-name)
                             (derived-mode-p 'comint-mode))
                     (car (vc-git-branches)))))
      (format "(%s)" branch)
    ""))

;; From https://0x0.st/oYX8
(defun vz/mode-line-fill (face except)
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin)
                                           ,except)))
              'face face))

;; TODO: For variable width fonts, the following should be better
;; width:  (apply #'+ (--map (aref it 4) (font-get-glyphs (font-at 0 nil "l") 0 4 " ass")))
;; height: (apply #'max (--map (+ (aref it 7) (aref it 8)) (font-get-glyphs (font-at 0 nil "l") 0 4 " ass")))
(defun vz/mode-line-roundise-text (text &optional foreground background raw-image?)
  "Return an image object with TEXT surrounded by arcs on either side."
  (require 'svg)
  (cond
   ((s-blank? text) "")
   ((not (display-graphic-p)) text)
   (t
    (let* ((f (font-at 0 nil "l"))
           (fw (window-font-width nil 'mode-line))
           (h (window-font-height nil 'mode-line))
           (w (+ h (* fw (string-width text))))
           (svg (svg-create w h)))
      (svg-rectangle svg 0 0 w h
                     :rx (/ h 2)
                     :ry (/ h 2)
                     :fill (or background
                               (if (moody-window-active-p) vz/mode-line-bg vz/mode-line-bgi)))
      (svg-text svg (format " %s " text)
                :font-family (font-get f :family)
                :font-size   (font-get f :size)
                :font-weight (font-get f :weight)
                :fill (or foreground
                          (if (moody-window-active-p) vz/mode-line-fg vz/mode-line-fgi))
                :x (/ h 2)
                :y (font-get f :size))
      (if-let ((img (svg-image svg :ascent 'center))
               raw-image?)
          img
        (propertize " " 'display img))))))

;; Update battery and time at intervals
(setq-default battery-update-interval 240)

(defvar vz/mode-line-battery ""
  "Variable that stores the svg image of battery information.")
(defvar vz/mode-line-time ""
  "Variable that stores the svg image of current time.")

(let ((fn #'(lambda (_ _ x)
              (when (eq x 'set)
               (force-mode-line-update t)))))
  (add-variable-watcher 'vz/mode-line-battery fn)
  (add-variable-watcher 'vz/mode-line-time fn))

(defun vz/mode-line-update-battery ()
  "Update battery status in mode-line."
  (interactive)
  (require 'notifications)
  (setq vz/mode-line-battery
        (vz/mode-line-roundise-text
         (let* ((battery-status (funcall battery-status-function))
                (percentage (floor (string-to-number (asoc-get battery-status ?p))))
                (charging? (s-equals? (asoc-get battery-status ?B) "Charging")))
           (cond
            ((and (not charging?) (<= percentage 20))
             ;(start-process "Lock due to low battery" nil "slock")
             )
            ((s-equals? (asoc-get battery-status ?B) "Full")
             (notifications-notify :title "Battery" :body "BATTERY IS FULL" :urgency 'critical)))
           (format "%d%%%s" percentage (if charging? "+" "")))
         vz/mode-line-fg vz/mode-line-bg)))

(defun vz/mode-line-update-time ()
  "Update time in mode-line."
  (interactive)
  (setq vz/mode-line-time
        (vz/mode-line-roundise-text (format-time-string "%H:%M")
                                    vz/mode-line-fg vz/mode-line-bg)))


;; When no graphical frames are on screen, the modeline images does not
;; get updated, so whenever a frame is created, recreate the image
;; to make sure the info is up-to-date.
;; (add-hook 'server-after-make-frame-hook
;;           (defun vz/mode-line-update-after-make-frame ()
;;             (vz/mode-line-update-battery)
;;             (vz/mode-line-update-time)))
;; (make-thread #'(lambda () (while (eq (length (visible-frame-list)) 1)
;;                    (sleep-for 1))
;;                  (run-at-time t display-time-interval #'vz/mode-line-update-time)
;;                  (run-at-time nil battery-update-interval #'vz/mode-line-update-battery)))

(defvar-local vz/mode-line-file-include-file-status? t
  "If non-nil, include the value of `vz/mode-line-file-state'
  after buffer-name.")

(defvar-local vz/mode-line-file-include-file-short-path? t
  "If non-nil, include the short path to file before
  buffer-name.")

(defvar-local vz/mode-line-file-extra-info nil
  "If non-nil, include (:eval vz/mode-line-file-extra-info) after
  file name image.")

(setq-default
 vz/mode-line-extra-info '()
 ;; (quote (:eval (and
 ;;                (window-at-side-p nil 'right)
 ;;                (window-at-side-p nil 'bottom)
 ;;                (concat vz/mode-line-time
 ;;                 " "
 ;;                 vz/mode-line-battery))))
 vz/mode-line-format `("  "
                       (:eval (vz/mode-line-roundise-text
                               (concat (when vz/mode-line-file-include-file-short-path?
                                        (vz/mode-line-file-short-dir))
                                (buffer-name)
                                (when vz/mode-line-file-include-file-status? (vz/mode-line-file-state)))))
                       (:eval (when vz/mode-line-file-extra-info
                               (eval vz/mode-line-file-extra-info)))
                       " "
                       (:eval (vz/mode-line-roundise-text (vz/mode-line-git-branch)))
                       (:eval (vz/mode-line-fill 'mode-line 18))
                       ,vz/mode-line-extra-info)
 mode-line-format vz/mode-line-format)
