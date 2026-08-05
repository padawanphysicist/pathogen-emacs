;;; pathogen-package-manager.el --- Setup package manager system -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs "27.2"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://codeberg.org/padawanphysicist/pathogen-emacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Configure package manager system.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure Elpaca		       ;;
;; 				       ;;
;; https://github.com/progfolio/elpaca ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
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
;; End of default installation suggested by Elpaca website

(when (version< emacs-version "29.1")
  (elpaca elpaca-use-package
    (elpaca-use-package-mode))
  (elpaca use-package)
  (elpaca-wait))

;;; Auto-hiding the elpaca-log buffer
(defun +elpaca--maybe-hide-log ()
  "Hide the Elpaca log buffer if every queued package finished successfully."
  (when (cl-every (lambda (e) (eq (elpaca<-status (cdr e)) 'finished))
                  (apply #'append (mapcar #'elpaca-q<-elpacas elpaca--queues)))
    (when-let ((window (get-buffer-window "*elpaca-log*" t)))
      (progn
	(with-selected-window window (quit-window 'kill window))
	(display-about-screen)))))

(defun +elpaca-hide-successful-log ()
  "Schedule a check to hide the Elpaca log buffer after queues settle."
  (run-at-time 0.5 nil #'+elpaca--maybe-hide-log))

;; (add-hook 'elpaca-post-queue-hook #'+elpaca-hide-successful-log)
(add-hook 'elpaca-after-init-hook
            (lambda () (+elpaca-hide-successful-log)
              (add-hook 'elpaca-post-queue-hook #'+elpaca-hide-successful-log)))

;(when (version< emacs-version "29.1") ;; Before emacs 29.1 use-package is not built-in
;  (elpaca use-package)
;  ;; Install use-package support
;(elpaca elpaca-use-package
;  ;; Enable use-package :ensure support for Elpaca.
;  (elpaca-use-package-mode))
;  (elpaca-wait))

;; Install use-package support
;; (elpaca elpaca-use-package
;;   ;; Enable use-package :ensure support for Elpaca.
;;   (elpaca-use-package-mode))


;
;
;(when (version= pathogen-min-emacs-version emacs-version)
;  (setq package-check-signature nil))
;
;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure Elpaca ;;
;;;;;;;;;;;;;;;;;;;;;;;
;(when (version<= pathogen-min-emacs-version emacs-version)
;  (defvar elpaca-installer-version 0.12)
;  (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
;  (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
;  (defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
;  (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
;                                :ref nil :depth 1 :inherit ignore
;                                :files (:defaults "elpaca-test.el" (:exclude "extensions"))
;                                :build (:not elpaca-activate)))
;  (let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
;         (build (expand-file-name "elpaca/" elpaca-builds-directory))
;         (order (cdr elpaca-order))
;         (default-directory repo))
;    (add-to-list 'load-path (if (file-exists-p build) build repo))
;    (unless (file-exists-p repo)
;      (make-directory repo t)
;      (when (<= emacs-major-version 28) (require 'subr-x))
;      (condition-case-unless-debug err
;          (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
;                    ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
;                                                    ,@(when-let* ((depth (plist-get order :depth)))
;                                                        (list (format "--depth=%d" depth) "--no-single-branch"))
;                                                    ,(plist-get order :repo) ,repo))))
;                    ((zerop (call-process "git" nil buffer t "checkout"
;                                          (or (plist-get order :ref) "--"))))
;                    (emacs (concat invocation-directory invocation-name))
;                    ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
;                                          "--eval" "(byte-recompile-directory \".\" 0 'force)")))
;                    ((require 'elpaca))
;                    ((elpaca-generate-autoloads "elpaca" repo)))
;              (progn (message "%s" (buffer-string)) (kill-buffer buffer))
;            (error "%s" (with-current-buffer buffer (buffer-string))))
;        ((error) (warn "%s" err) (delete-directory repo 'recursive))))
;    (unless (require 'elpaca-autoloads nil t)
;      (require 'elpaca)
;      (elpaca-generate-autoloads "elpaca" repo)
;      (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
;  (add-hook 'after-init-hook #'elpaca-process-queues)
;  (elpaca `(,@elpaca-order))
;
;  (defun +elpaca-safe-close-log-buffer ()
;    "Fecha a janela do buffer de log do Elpaca após um curto intervalo."
;    (when-let ((buf (get-buffer "*elpaca-log*"))
;               (win (get-buffer-window buf)))
;      ;; Fecha a janela sem interferir no processamento do Elpaca
;      (quit-window nil win)))
;
;  (defun +elpaca-queue-finished-callback (&rest _)
;    "Agenda o fechamento do log após o processamento da fila."
;    (run-with-timer 2 nil #'+elpaca-safe-close-log-buffer))
;
;  ;; O hook `elpaca-post-queue-hook' é o ponto de entrada correto
;  (add-hook 'elpaca-post-queue-hook #'+elpaca-queue-finished-callback)
;
;  ;; Install use-package support
;  (elpaca elpaca-use-package
;    ;; Enable use-package :ensure support for Elpaca.
;    (elpaca-use-package-mode))
;  (elpaca use-package)
;  (elpaca-wait))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure package.el ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(unless (version<= pathogen-min-emacs-version emacs-version)
;  (when (version<= "26.3" emacs-version)
;    (require 'package)
;    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;    (package-initialize))
;
;  ;; Refresh package contents periodically (e.g., every 2 days)
;  (let* ((archive-dir (expand-file-name "elpa/archives/melpa/archive-contents" user-emacs-directory))
;         (days-between-updates pathogen-days-between-package-manager-cache-updates)
;         (seconds-between-updates (* days-between-updates 24 60 60)))
;
;    (if (or (not package-archive-contents) ; if no cache
;            (not (file-exists-p archive-dir)) ; if there is no file
;            (> (float-time (time-since (file-attribute-modification-time (file-attributes archive-dir))))
;               seconds-between-updates)) ; if file is older than interval
;        (progn
;          (message "MELPA cache is deprecated. Updating index...")
;          (package-refresh-contents))
;      (message "MELPA cache is up-to-date (< %d days)." days-between-updates)))
;
;  ;; Activate packages according the installed emacs version
;  (if (version<= "27.1" emacs-version)
;      (package-activate-all))
;
;  ;; Declarative Package Management (use-package)
;
;  ;; Configure `use-package' to enable clean, declarative package isolation.
;  ;; Since `use-package' is built-in starting with Emacs 29.1, we conditionally
;  ;; install it from downstream repositories only when running on older Emacs
;  ;; versions. Additionally, `use-package-always-ensure' is enabled globally to
;  ;; automatically fetch and install missing packages during startup without
;  ;; requiring explicit `:ensure t' keywords in every declaration.
;
;  ;; Install use-package if it's not already there
;  (when (version< emacs-version "29.1")
;    (unless (package-installed-p 'use-package)
;      (condition-case nil
;          (package-install 'use-package)
;        (error
;         (message "Failed upon installing use-package. Updating MELPA index...")
;         (package-refresh-contents)
;         (package-install 'use-package)))))
;
;  ;; Activates use-package
;  (require 'use-package)
;  (setq use-package-always-ensure t)
;
;  ;; If any package declared with `use-package' fails to install, this hook forces
;  ;; an update of MELPA and attempts to install again automatically.
;  (add-hook 'use-package-ensure-failed-hook
;            (lambda (package error)
;              (message "Failed installing %s due to: %s. Trying to update MELPA..." package error)
;              (package-refresh-contents)
;              (package-install package))))

(provide 'pathogen-package-manager)
;;; pathogen-package-manager.el ends here

