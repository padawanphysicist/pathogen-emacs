;;; pathogen-ui.el --- UI optimizations and tweaks -*- lexical-binding: t; -*-
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
;; point-and-click UI. The hook defined here is meant to provide some saner
;; defaults, but not too opiniated.
;;
;;; Code:

(defvar pathogen-init-ui-hook nil
  "Hook to provide basic user interface.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Disable tool, menu, and scrollbars
;;
;;
;; To prevent the glimpse of un-styled Emacs we disable these UI elements early
;; by directly setting the variable `default-frame-alist', which keeps the
;; default values used when creating a frame (window in the modern parlance):
(add-hook
 'pathogen-init-ui-hook
 (lambda ()
   (progn
     (push '(menu-bar-lines . 0)   default-frame-alist)
     (push '(tool-bar-lines . 0)   default-frame-alist)
     (push '(vertical-scroll-bars) default-frame-alist))))
;; However, doing this only creates a problem: since their respective varibles
;; are not set, if the user wants to enable the tool-bar for example, it would
;; be necessary to use the cycle twice the command `tool-bar-mode' to enable.
;;
;; Therefore we need to unset their variables too:
(add-hook
 'pathogen-init-ui-hook
 (lambda ()
   (setq
    menu-bar-mode nil
    tool-bar-mode nil
    scroll-bar-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Disable startup screen
;;
;;
;; Let's be honest: if you are using Emacs by now I don't think you are really
;; interested in the info on the startup screen.
(add-hook
 'pathogen-init-ui-hook
 (lambda ()
   (setq inhibit-startup-screen t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Smooth scrolling
;;
;;
(add-hook
 'pathogen-init-ui-hook
 (lambda ()
   (setq
    ;; Ensure that the mouse wheel scroll the window the mouse is over.
    mouse-wheel-follow-mouse t
    ;; Scroll one line at a time when using mouse This is less "jumpy"
    ;; than the default behaviour.
    mouse-wheel-scroll-amount '(1 ((shift) . 1))
    ;; Horizontal scrolling
    mouse-wheel-scroll-amount-horizontal 2
    ;; Don't accelerate scrolling:
    ;; The original behaviour is to scroll as fast as the user moves
    ;; the wheel.
    mouse-wheel-progressive-speed nil
    ;; Keyboard scroll one line at a time
    scroll-step 1
    ;; Emacs spends too much effort recentering the screen if you
    ;; scroll the cursor more than N lines past window edges (where N
    ;; is the settings of `scroll-conservatively'). This is especially
    ;; slow in larger files during large-scale scrolling commands. If
    ;; kept over 100, the window is never automatically recentered.
    scroll-conservatively 101
    scroll-margin 0
    scroll-preserve-screen-position t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minor tweaks
;;
;;
(add-hook
 'pathogen-init-ui-hook
 (lambda ()
   (progn
     ;; The blinking cursor is distracting
     (blink-cursor-mode -1)
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
     (setq resize-mini-windows 'grow-only))))

(provide 'pathogen-ui.el)
;;; pathogen-ui.el ends here
