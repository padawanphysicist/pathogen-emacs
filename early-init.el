;;; early-init.el --- Early initialization file -*- no-byte-compile: t; lexical-binding: t; fill-column: 79; -*-
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
;; This file is loaded very early in the startup process, before the package
;; system and GUI are initialized.  Use it to customize variables that affect
;; the initial package loading and frame setup.
;;
;; For most customizations, especially those related to GUI features, use the
;; regular init.el file instead.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defer package initialization
;;
;;
;; Prior to Emacs 27, the init file was responsible for initializing the
;; package manager by explicitly calling `package-initialize'. Starting with
;; Emacs 27, the default behavior changed: `package-initialize' is now
;; automatically called before loading the init file. This means package
;; initialization occurs after `early-init-file' is loaded but before
;; `user-init-file' is processed. To prevent Emacs from initializing packages
;; automatically, we set `package-enable-at-startup' to nil:
(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Toggle menu bar, tool bar, scroll bars
;;
;; To prevent the glimpse of un-styled Emacs we disable these UI elements early
;; by directly setting the variable `default-frame-alist', which keeps the
;; default values used when creating a frame (window in the modern parlance):
(unless (eq system-type 'android)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (push '(horizontal-scroll-bars) default-frame-alist))

;; However, doing this only creates a problem: since their respective varibles
;; are not set, if the user wants to enable the tool-bar for example, it would
;; be necessary to use the cycle twice the command `tool-bar-mode' to enable.
;;
;; Therefore we need to unset their variables too:
(unless (eq system-type 'android)
  (setq menu-bar-mode nil
        tool-bar-mode nil
        scroll-bar-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modify garbage collector
;;
;; The garbage collector (GC) of Emacs in Emacs is very simple. You allocate
;; some bytes while using it and once you pass a certain threshold, the garbage
;; collector is triggered.
;;
;; It turns out that the default behavior is to garbage collect very often (you
;; can check whether or not this is case for you by setting the variable
;; `garbage-collection-messages' to t). Usually there is so little garbage to
;; collect each time that you will not notice any lag. The problem is when you
;; use memory-intensive features like `helm' on a large collection.
;;
;; GC also can eats up quite a bit of time, easily doubling startup time. We
;; reduce this initialization time by defering the garbage collector, turning up
;; the memory threshold as early as possible.
;;
;; To control the trigger of the garbage collector we can use the variables
;; `gc-cons-threshold' and `gc-cons-percentage'.
;;
;; Therefore to improve the GC we adopt the following strategy:
;;
;;   a. Increase `gc-cons-threshold' to large number so GC is not triggered
;;      early during startup
;;   b. Restore it to a sane value after initialization finishes.
;;
;; Step `a' is easy:
(defun pathogen--defer-gc ()
  "Defer garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))
(pathogen--defer-gc)

;; For step `b', I proceed as follows: since the default value of
;; `gc-cons-threshold' is 800000 (800KB), we temporarily override the default
;; during startup.
;;
;; To ensure these settings are properly reset after initialization, we hook
;; a restoration function into `emacs-startup-hook'. This hook runs later
;; than `after-init-hook', ensuring all init files have finished loading (see:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary).
(defun pathogen--restore-gc ()
  "Restore garbage collection."
  (setq gc-cons-threshold 67108864 ; 64MB. If you experience freezing, decrease
                                   ; this value. If you experience stuttering,
                                   ; increase it.
        gc-cons-percentage 0.5))   ; 50% growth triggers GC
(add-hook 'emacs-startup-hook #'pathogen--restore-gc)

;; Garbage collector within minibuffer
;;
;; We use the same strategy when using minibuffer:
(add-hook 'minibuffer-setup-hook #'pathogen--defer-gc)
(add-hook 'minibuffer-exit-hook  #'pathogen--restore-gc)

;; Garbage collect only when Emacs is out of focus
;;
;; This keeps GC out of your way:
(add-hook 'emacs-startup-hook
          (lambda ()
            (add-function :after after-focus-change-function
                          (lambda ()
                            (unless (frame-focus-state)
                              (garbage-collect))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warning level configuration
;;
;;
;; Set to :error instead of :emergency to avoid suppressing important warnings
;; about configuration issues, deprecated functions, or package loading errors.
;; This still prevents minor warnings from interrupting startup while keeping
;; you informed about actual problems.
;;
;; Warning levels (least to most severe):
;;   :debug < :info < :warning < :error < :emergency
;;
;; - :emergency suppresses almost everything (previous setting)
;; - :error shows errors but hides routine warnings (current setting)
;; - :warning shows all warnings (Emacs default, can be noisy)
(setq warning-minimum-level :error)

;;; early-init.el ends here
