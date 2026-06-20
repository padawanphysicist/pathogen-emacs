;;; pathogen-better-defaults.el --- Better defaults for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Victor Santos
;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs "27.2"))
;; Keywords: convenience
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://codeberg.org/padawanphysicist/pathogen-emacs

;;; Commentary:
;; A collection of sensible defaults to improve Emacs ergonomics.
;; This module avoids external dependencies, focusing on core improvements.

;;; Code:

(defgroup better-defaults nil
  "Customizations for improved Emacs defaults."
  :group 'convenience)

;;; Standardizing defaults
(use-package emacs
  :ensure nil
  :config
  ;; UI: Cleanup visual clutter
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode t)

  ;; Editing: Modern habits
  (setq-default indent-tabs-mode nil)
  (setq-default show-trailing-whitespace t)
  (delete-selection-mode 1)
  (show-paren-mode 1)
  (fset 'yes-or-no-p #'y-or-n-p)

  ;; Clipboard & System Integration
  (setq select-enable-clipboard t
        mouse-yank-at-point t)

  ;; Files: Redirect backups and enable auto-revert
  (let ((backup-dir (expand-file-name "backups/" pathogen-cache-directory)))
    (setq backup-directory-alist `(("." . ,backup-dir))
          auto-save-file-name-transforms `((".*" ,backup-dir t))))
  (global-auto-revert-mode t))

;;; Built-in package configuration
(use-package savehist  :ensure nil :init (savehist-mode 1))
(use-package recentf   :ensure nil :init (recentf-mode 1))
(use-package saveplace :ensure nil :init (save-place-mode 1))
(use-package icomplete
  :ensure nil
  :config
  (fido-mode 1)
  (icomplete-vertical-mode 1))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

(provide 'pathogen-better-defaults)
;;; pathogen-better-defaults.el ends here
