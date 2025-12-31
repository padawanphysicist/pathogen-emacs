;;; Package manager configuration. -*- lexical-binding: t; -*-

(pathogen/log 'info "Package Manager Engine: %s" +bootstrap/package-manager-engine)

(cond
 ((eq +bootstrap/package-manager-engine 'elpaca)
  ;;;;;;;;;;;;;;;;;;;;;;
  ;; Bootstrap Elpaca ;;
  ;;;;;;;;;;;;;;;;;;;;;;
  (progn
    ;; 1. Essential Elpaca Variables
    (defvar elpaca-installer-version 0.11)
    (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
    (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
    (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
    (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                                  :ref nil :depth 1 :inherit ignore
                                  :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                                  :build (:not elpaca--activate-package)))

    ;; 2. The Bootstrap Logic
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
        (let ((load-source-file-function nil)) (load (expand-file-name "elpaca-autoloads" repo)))))

    ;; 3. The Pathogen Fix: Manual Queue Processing
    ;; Instead of just a hook, we ensure the queue starts now.
    (add-hook 'after-init-hook #'elpaca-process-queues)
    
    (if (bound-and-true-p elpaca-initialized)
        (elpaca `(,@elpaca-order))
      (eval `(elpaca (,@elpaca-order))))

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
          (with-selected-window window (quit-window 'kill window)
				(display-about-screen)))))

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
            use-package-expand-minimally t))))

 ((eq +bootstrap/package-manager-engine 'straight)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Bootstrap straight.el ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (progn
    (defvar bootstrap-version)
    (let ((bootstrap-file
	   (expand-file-name
            "straight/repos/straight.el/bootstrap.el"
            (or (bound-and-true-p straight-base-dir)
		user-emacs-directory)))
	  (bootstrap-version 7))
      (unless (file-exists-p bootstrap-file)
	(with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))))

 (t (pathogen/log 'error (format "⚠️ Unknown package engine: %s" +bootstrap/package-manager-engine))))

;;; config.el ends here
