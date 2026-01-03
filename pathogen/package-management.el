;;; package-management.el --- Setup package manager -*- lexical-binding: t; -*-
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
;; Setup package manager
;;
;;; Code:

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

  (defun +elpaca-hide-successful-log ()
      "Hide Elpaca log buffer if queues processed successfully."
					;(message "this: %S last: %S" this-command last-command)
      (if-let ((incomplete (cl-find 'incomplete elpaca--queues :key #'elpaca-q<-status))
               ((elpaca-q<-elpacas incomplete)))
          nil
	(when-let ((log (bound-and-true-p elpaca-log-buffer))
                   (window (get-buffer-window log t)) ;; log buffer visible
                   ((or t (member last-command +elpaca-hide-log-commands)
			(member this-command +elpaca-hide-log-commands))))
          (with-selected-window window (quit-window 'kill window)))))

    (defun neo/elpaca--bury-log-deferred (&optional delay)
      (let ((delay (or delay 1)))
	(run-at-time delay nil #'+elpaca-hide-successful-log)))
    (add-hook 'elpaca-post-queue-hook #'neo/elpaca--bury-log-deferred)

    (elpaca elpaca-use-package
      (elpaca-use-package-mode)
      (setq elpaca-use-package-by-default t))

   ;; Allow Elpaca to process queues up to this point
    (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a
    ;; use-package keyword
    (if debug-on-error
	(setq use-package-verbose t
              use-package-expand-minimally nil
              use-package-compute-statistics t)
      (setq use-package-verbose nil
            use-package-expand-minimally t))

(provide 'package-management)
;;; package-management.el ends here
