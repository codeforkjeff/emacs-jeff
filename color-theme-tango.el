
;; jeff's customizations to tango
;; See http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines#Color

;;; Color theme based on Tango Palette. Created by danranx@gmail.com

(require 'color-theme)

(defvar tango-butter-light "#fce94f")
(defvar tango-butter-med "#edd400")
(defvar tango-butter-dark "#c4a000")
(defvar tango-orange-light "#fcaf3e")
(defvar tango-orange-med "#f57900")
(defvar tango-orange-dark "#ce5c00")
(defvar tango-choc-light "#e9b96e")
(defvar tango-choc-med "#c17d11")
(defvar tango-choc-dark "#8f5902")
(defvar tango-cham-light "#8ae234")
(defvar tango-cham-med "#73d216")
(defvar tango-cham-dark "#4e9a06")
(defvar tango-skyblue-light "#729fcf")
(defvar tango-skyblue-med "#3465a4")
(defvar tango-skyblue-dark "#204a87")
(defvar tango-plum-light "#ad7fa8") 
(defvar tango-plum-med "#75507b") 
(defvar tango-plum-dark "#5c3566")
(defvar tango-red-light "#ef2929")
(defvar tango-red-med "#cc0000")
(defvar tango-red-dark "#a40000")
(defvar tango-alum-light1 "#eeeeec")
(defvar tango-alum-light2 "#d3d7cf")
(defvar tango-alum-med1 "#babdb6")
(defvar tango-alum-med2 "#888a85")
(defvar tango-alum-dark1 "#555753")
(defvar tango-alum-dark2 "#2e3436")

(defun color-theme-tango ()
  "A color theme based on Tango Palette."
  (interactive)
  (color-theme-install
   `(color-theme-tango
     ((background-color . ,tango-alum-dark2)
      (background-mode . dark)
      (border-color . ,tango-alum-med2)
      (cursor-color . ,tango-butter-light)
      (foreground-color . ,tango-alum-light1)
      (mouse-color . ,tango-cham-light))
     ((help-highlight-face . underline)
      (ibuffer-dired-buffer-face . font-lock-function-name-face)
      (ibuffer-help-buffer-face . font-lock-comment-face)
      (ibuffer-hidden-buffer-face . font-lock-warning-face)
      (ibuffer-occur-match-face . font-lock-warning-face)
      (ibuffer-read-only-buffer-face . font-lock-type-face)
      (ibuffer-special-buffer-face . font-lock-keyword-face)
      (ibuffer-title-face . font-lock-type-face))
     (border ((t (:background ,tango-alum-med2))))
     (fringe ((t (:background ,tango-alum-dark2))))
     (mode-line ((t (:foreground ,tango-alum-light1 :background ,tango-alum-dark1))))
     (region ((t (:background ,tango-alum-dark1))))

     (font-lock-builtin-face ((t (:foreground ,tango-skyblue-light))))
     (font-lock-comment-face ((t (:foreground ,tango-alum-med2))))
     (font-lock-constant-face ((t (:foreground ,tango-red-light))))
     (font-lock-doc-face ((t (:foreground ,tango-alum-med2))))
     (font-lock-keyword-face ((t (:foreground ,tango-skyblue-light :bold t))))
     (font-lock-string-face ((t (:foreground ,tango-plum-light :italic t))))
     (font-lock-type-face ((t (:foreground ,tango-cham-light :bold t))))
     (font-lock-variable-name-face ((t (:foreground ,tango-skyblue-light))))
     (font-lock-warning-face ((t (:bold t :foreground ,tango-orange-med))))
     (font-lock-function-name-face ((t (:foreground ,tango-red-light :bold t :italic t))))

     (comint-highlight-input ((t (:italic t :bold t))))
     (comint-highlight-prompt ((t (:foreground ,tango-cham-light))))
     (isearch ((t (:background ,tango-orange-med :foreground ,tango-alum-dark2))))
     (isearch-lazy-highlight-face ((t (:foreground ,tango-alum-dark2 :background ,tango-choc-light))))
     (show-paren-match-face ((t (:foreground ,tango-alum-dark2 :background ,tango-cham-med))))
     (show-paren-mismatch-face ((t (:background ,tango-plum-light :foreground ,tango-alum-dark2))))
     (minibuffer-prompt ((t (:foreground ,tango-skyblue-light :bold t))))
     (info-xref ((t (:foreground ,tango-skyblue-light))))
     (info-xref-visited ((t (:foreground ,tango-plum-light))))

     (hl-line ((t (:background ,tango-skyblue-dark))))

     (nxml-element-local-name ((t (:foreground ,tango-skyblue-light))))
     (rng-error ((t (:foreground ,tango-red-med))))

     (diff-indicator-removed ((t (:foreground ,tango-red-light))))
     (diff-removed ((t (:foreground ,tango-red-light))))
     (diff-indicator-added ((t (:foreground ,tango-skyblue-light))))
     (diff-added ((t (:foreground ,tango-skyblue-light))))
     )))

(setq ansi-color-names-vector
      (vector tango-alum-dark1
              tango-red-light
              tango-cham-dark
              tango-butter-dark
              tango-skyblue-light
              tango-plum-light
              tango-alum-med2
              tango-alum-light2
              ))

; this next line is crucial; ansi-color-map doesnt actually pick up
; the values from above automatically
(setq ansi-color-map (ansi-color-make-color-map))

(add-to-list 'color-themes '(color-theme-tango
                             "Tango"
                             "Jeff Chiu <jeff@codefork.com>"))

(provide 'color-theme-tango)
