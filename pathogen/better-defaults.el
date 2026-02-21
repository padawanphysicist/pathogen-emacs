;;; better-defaults.el --- UI optimizations and tweaks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Victor Santos
;;
;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Since I strive for a keyboard centric experience, I disable most of the
;; point-and-click UI.  The settings here should provide some saner defaults, but
;; not too opiniated.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Smooth scrolling
;;
;;
(setq
 ;; Ensure that the mouse wheel scroll the window the mouse is over.
 mouse-wheel-follow-mouse t
 ;; Scroll one line at a time when using mouse
 ;; This is less "jumpy" than the default behaviour.
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 ;; Horizontal scrolling
 mouse-wheel-scroll-amount-horizontal 2
 ;; Don't accelerate scrolling
 ;; The original behaviour is to scroll as fast as the user moves the wheel.
 mouse-wheel-progressive-speed nil
 ;; Keyboard scroll one line at a time
 scroll-step 1
 ;; Emacs spends too much effort recentering the screen if you scroll the
 ;; cursor more than N lines past window edges (where N is the settings of
 ;; `scroll-conservatively'). This is especially slow in larger files during
 ;; large-scale scrolling commands. If kept over 100, the window is never
 ;; automatically recentered.
 scroll-conservatively 101
 scroll-margin 0
 scroll-preserve-screen-position t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor tweaks
;;
;;
;; Don't stretch the cursor to fit wide characters
(setq x-stretch-cursor nil)
;; Lazy people like me never want to type "yes" when "y"
;; will suffice
(fset #'yes-or-no-p #'y-or-n-p)
;; always avoid GUI
(setq use-dialog-box nil)
;; Don't display floating tooltips
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; Show current key-sequence in minibuffer. This
;; provides some additional feedback for the user.
(setq echo-keystrokes 0.02)
;; Expand the minibuffer to fit multi-line text
;; displayed in the echo-area
;;(setq resize-mini-windows 'grow-only)
(setq resize-mini-windows nil)

;; Enable pixel-scroll-precision-mode for smooth scrolling (Emacs 29+)
;; This provides smooth pixel-level scrolling with mouse/trackpad instead of
;; jumping line-by-line. Significantly improves the scrolling experience on
;; modern displays and input devices.
(when (and (>= emacs-major-version 29)
	   (fboundp 'pixel-scroll-precision-mode))
  (pixel-scroll-precision-mode 1))

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file 'noerror)

(provide 'better-defaults)
;;; better-defaults.el ends here
