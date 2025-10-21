;;; early-funcs.el --- Pathogen Core File -*- lexical-binding: t; -*-
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
;;  This file is sourced by emacs early-init.el file.
;;
;;; Code:

(defun pathogen/toggle-gui-elements-off ()
  "Toggle menu bar, tool bar, scroll bars"
  ;; To prevent the glimpse of un-styled Emacs we disable these UI elements early
  ;; by directly setting the variable `default-frame-alist', which keeps the
  ;; default values used when creating a frame (window in the modern parlance):
  (push '(menu-bar-lines . 0)   default-frame-alist)
  (push '(tool-bar-lines . 0)   default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (push '(horizontal-scroll-bars) default-frame-alist)
  ;; However, doing this only creates a problem: since their respective varibles
  ;; are not set, if the user wants to enable the tool-bar for example, it would
  ;; be necessary to use the cycle twice the command `tool-bar-mode' to enable.
  ;;
  ;; Therefore we need to unset their variables too:
  (setq
   menu-bar-mode nil
   tool-bar-mode nil
   scroll-bar-mode nil))

(defun pathogen/tune-garbage-collector ()
  "Modify garbage collector
  
  The garbage collector (GC) of Emacs in Emacs is very simple. You allocate
  some bytes while using it and once you pass a certain threshold, the garbage
  collector is triggered.
  
  It turns out that the default behavior is to garbage collect very often (you
  can check whether or not this is case for you by setting the variable
  `garbage-collection-messages' to t). Usually there is so little garbage to
  collect each time that you will not notice any lag. The problem is when you
  use memory-intensive features like `helm' on a large collection.
  
  GC also can eats up quite a bit of time, easily doubling startup time. We
  reduce this initialization time by defering the garbage collector, turning up
  the memory threshold as early as possible.
  "
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
  ;; `gc-cons-threshold' is 800000 (800KB), we use the values defined in
  ;; pathogen-vars.el (pathogen/gc-cons-threshold and pathogen/gc-cons-percentage).

  ;; and add a function to restore GC as a hook:
  ;;
  ;; * `emacs-startup-hook' functions are evaluated later than `after-init-hook'
  ;; function, as you can check in
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary):
  (defun pathogen--restore-gc ()
    "Restore garbage collection."
    (setq gc-cons-threshold pathogen-gc-cons-threshold)
    (setq gc-cons-percentage pathogen-gc-cons-percentage))
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
				(garbage-collect)))))))

(defun pathogen/minor-tweaks ()
  "Perform a series of minor tweaks."

  ;; File name handler optimization
  ;;
  ;;
  ;; Disable file-name-handler-alist during initialization for significant
  ;; performance improvement (30-50% faster startup). Handlers check for remote
  ;; files, archives, compression, etc., which are not needed during init.
  ;; The original list is restored after startup completes.
  (setq file-name-handler-alist nil)

  ;; Restore after initialization
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist pathogen--file-name-handler-alist)))

  ;; Improve subprocess communication
  ;;
  ;;
  ;; Increase the amount of data which Emacs reads from subprocesses in a single
  ;; chunk. This is especially important for LSP servers and other tools that
  ;; communicate via stdout/stdin.
  ;;
  ;; The default value is 4096 bytes (4KB), which is far too low for modern
  ;; systems. Setting this to 1MB significantly improves performance of language
  ;; servers, tree-sitter parsers, and other external tools.
  (setq read-process-output-max (* 1024 1024)) ; 1MB

  ;; Improve loading of files
  ;;
  ;;
  ;; In noninteractive sessions, prioritize non-byte-compiled source files to
  ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
  ;; to skip the mtime checks on every *.elc file.
  (setq load-prefer-newer 'noninteractive)

  ;; Warning level configuration
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
  (setq warning-minimum-level :error))

(defun pathogen/ui-tweaks ()
  "Early UI optimizations"

  ;; Disable the startup screen to prevent it from flashing briefly during
  ;; initialization. Setting this in early-init.el ensures it never appears,
  ;; rather than appearing and then being hidden when init.el loads.
  (setq inhibit-startup-screen t)

  ;; Bell configuration: Flash the mode line instead of the entire screen
  ;; or making an audible beep. The default visual bell flashes the whole
  ;; screen which can be jarring and distracting. This subtle mode-line
  ;; flash provides feedback without being intrusive.
  (setq visible-bell nil)  ; Disable default visual bell
  (setq ring-bell-function
	(lambda ()
          (let ((orig-fg (face-foreground 'mode-line)))
            (set-face-foreground 'mode-line "#F2804F")
            (run-with-idle-timer 0.1 nil
				 (lambda (fg) (set-face-foreground 'mode-line fg))
				 orig-fg))))
  )

(provide 'early-funcs)
