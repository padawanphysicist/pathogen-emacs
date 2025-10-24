;;; package-manager.el --- Setup package manager -*- lexical-binding: t; -*-
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
;; Module: Package Manager (02)
;; Purpose: Package management infrastructure
;; Dependencies: None
;; Provides: (elpaca ...), (use-package ...)
;;
;; This module bootstraps and configures the Elpaca package manager and
;; integrates it with use-package. CRITICAL: This module must load before
;; any configuration that uses external packages.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Elpaca
;;
;;
;; https://github.com/progfolio/elpaca
;;
;; Bootstrap notes:
;; The bootstrap process clones Elpaca from GitHub on first run. This requires:
;;   - Active internet connection
;;   - Access to github.com (not blocked by firewall/proxy)
;;   - May take 30-60 seconds depending on network speed
;;
;; If Emacs hangs during first startup:
;;   1. Check your internet connection
;;   2. Ensure github.com is accessible
;;   3. Try manual bootstrap:
;;      git clone --depth=1 https://github.com/progfolio/elpaca.git \
;;        ~/.emacs.d/elpaca/repos/elpaca
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a
               ;; use-package keyword

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use-package loading strategy
;;
;;
;; By default, use-package loads packages immediately unless you specify
;; :defer, :hook, :mode, :bind, or similar keywords that trigger lazy loading.
;;
;; For large configurations with slow startup times, you can enable global
;; deferred loading with: (setq use-package-always-defer t)
;;
;; When enabled:
;;   - ALL packages defer loading by default
;;   - Must explicitly use :demand t for immediate loading
;;   - Significantly improves startup time
;;   - Requires careful auditing of which packages need immediate loading
;;
;; Packages that typically need :demand t:
;;   - Themes (visual appearance on startup)
;;   - Core infrastructure (evil, which-key, key-chord)
;;   - Global modes (company, vertico, marginalia)
;;   - Packages with startup hooks (dashboard, all-the-icons)
;;
;; Example with always-defer enabled:
;;
;;   (setq use-package-always-defer t)
;;
;;   (use-package doom-themes
;;     :demand t  ; Theme must load immediately for visual appearance
;;     :config
;;     (load-theme 'doom-one t))
;;
;;   (use-package magit
;;     ;; No :demand needed - magit can load when first invoked
;;     :bind ("C-x g" . magit-status))
;;
;;   (use-package markdown-mode
;;     ;; No :demand needed - loads when opening .md files
;;     :mode "\\.md\\'")
;;
;; NOTE: This configuration does NOT enable use-package-always-defer by default.
;;       Elpaca already provides efficient lazy loading, and most packages in
;;       this configuration already have explicit loading strategies (:hook,
;;       :mode, :bind, etc.). Enable use-package-always-defer only if you
;;       experience slow startup times and want to enforce lazy loading globally.
;;
;; To enable globally, add to your ~/.pathogen.el:
;;   (setq use-package-always-defer t)
;;
;; See also: https://github.com/jwiegley/use-package#loading-packages

(if debug-on-error
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elpaca logging configuration
;;
;;
;; Elpaca logs package operations to the *elpaca-log* buffer by default.
;; This is helpful for debugging package installation or update issues.
;;
;; Access the log with: M-x elpaca-log or (switch-to-buffer "*elpaca-log*")
;; (setq elpaca-log-buffer "*elpaca-log*")  ; Explicit default

;; Optional: Increase verbosity for debugging package issues
;; (setq elpaca-verbosity 2)  ; 0=quiet, 1=normal (default), 2=verbose, 3=debug

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elpaca recipe overrides
;;
;;
;; You can override package recipes to use different sources, branches, or
;; build configurations. This is useful for:
;;   - Using forked repositories
;;   - Testing development branches
;;   - Pinning to specific commits
;;   - Local package development
;;
;; Examples:
;;
;; Use a fork:
;;   (use-package pkg :elpaca (pkg :host github :repo "you/pkg"))
;;
;; Use specific branch:
;;   (use-package pkg :elpaca (pkg :branch "develop"))
;;
;; Pin to commit:
;;   (use-package pkg :elpaca (pkg :ref "abc123"))
;;
;; Local development:
;;   (use-package pkg :elpaca (pkg :local-repo "~/projects/pkg"))
;;
;; See also: https://github.com/progfolio/elpaca#recipe-format

;; This function displays how long Emacs took to start.
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))

(message "Loaded package-manager module")

(provide 'package-manager)
;;; package-manager.el ends here
