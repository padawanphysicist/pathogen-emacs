;;; early-init.el --- Emacs 27+ pre-initialization -*- lexical-binding: t; -*-
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
;;  Emacs (27+) introduces early-init.el, which is run before init.el, before
;;  package and UI initialization happens.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modify garbage collector
;;
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
;;     a. Increase `gc-cons-threshold' to large number so GC is not triggered
;;        early during startup
;;     b. Restore it to a sane value after initialization finishes.
;;
;; Step 'a' is easy:
(defun pathogen--defer-gc ()
  "Defer garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))
(pathogen--defer-gc)

;; For step 'b', I proceed as follows: since the default value of
;; `gc-cons-threshold' is 800000 (800KB), I define a new value to be default,
(defvar pathogen/gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")

;; and add a function to restore GC as a hook:
;;
;; * `emacs-startup-hook' functions are evaluated later than `after-init-hook'
;; function, as you can check in
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary):
(defun pathogen--restore-gc ()
  "Restore garbage collection."
  (setq gc-cons-threshold pathogen/gc-cons-threshold))
(add-hook 'emacs-startup-hook #'pathogen--restore-gc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Garbage collector within minibuffer
;;
;;
;; We use the same strategy when using minibuffer:
(add-hook 'minibuffer-setup-hook #'pathogen--defer-gc)
(add-hook 'minibuffer-exit-hook  #'pathogen--restore-gc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Garbage collect only when Emacs is out of focus
;;
;;
;; This keeps GC out of your way:
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Improve loading of files
;;
;;
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer 'noninteractive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defer package initialization
;;
;;
;; Package initialization occurs before `user-init-file' is loaded, but after
;; `early-init-file'. This prevent Emacs from doing it early:
(setq package-enable-at-startup nil)


(provide 'early-init)
;;; early-init.el ends here
