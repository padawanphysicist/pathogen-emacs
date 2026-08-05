;;; pathogen-ui.el --- Improvements on user-experience -*- lexical-binding: t; -*-

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

;;; Code:

;;;; Window & Workspace layout

;; Popper: Keeps temporary/popup buffers tightly managed
(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

;; Winum: Window number selection (M-1, M-2, etc.)
(use-package winum
  :ensure t
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-²") 'winum-select-window-by-number)
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map))
  :config
  ;; Consistency fix: Prevent winum from numbering popper's popup windows
  (setq winum-ignored-buffers-regexp '("\\*Messages\\*" "\\*Help\\*" "\\*Compile-Log\\*"))
  (winum-mode +1))

;; Transpose-frame: Rotate, flip, or transpose window layouts
(use-package transpose-frame
  :ensure t
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise))

;;;; Completion system
(if (version<= "29.1" emacs-version)
    (require 'pathogen-icr-vompeccc)
  (require 'pathogen-icr-icomplete))

(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer)
   ("M-g M-g" . avy-goto-line)))

;;;; Version control
(when (version<= "28.1" emacs-version)
  (use-package transient
    :ensure t
    :custom
    (transient-levels-file (expand-file-name "transient/levels.el" pathogen-cache-directory))
    (transient-values-file (expand-file-name "transient/values.el" pathogen-cache-directory))
    (transient-history-file (expand-file-name "transient/history.el" pathogen-cache-directory)))

  ;; Magit: A spectacular Git interface for Emacs
  (use-package magit
    :ensure t
    :bind ("C-x g" . magit-status))

  (use-package hl-todo
    :after magit
    :ensure t
    :custom
    (hl-todo-keyword-faces
     '(("TODO"   . "#FF0000")
       ("FIXME"  . "#FF0000")
       ("DEBUG"  . "#A020F0")
       ("GOTCHA" . "#FF4500")
       ("STUB"   . "#1E90FF")))
    :config
    (with-eval-after-load 'magit
      (add-hook 'magit-log-wash-summary-hook
                #'hl-todo-search-and-highlight t)
      (add-hook 'magit-revision-wash-message-hook
                #'hl-todo-search-and-highlight t)))

  (use-package magit-todos
    :after magit
    :ensure t
    :config (magit-todos-mode 1)))

;;;; Terminal

;; Eat and Eat powered Eshell, fast featureful terminal inside Emacs:
;; https://emacsconf.org/2023/talks/eat/
;; https://codeberg.org/akib/emacs-eat

(when (version<= "28.1" emacs-version)
  (use-package eat
    :ensure t
    :after project
    :custom (eat-term-name "xterm-256color")
    :hook (eshell-load . eat-eshell-mode)))

;;; Misc

;; o [[https://github.com/vedang/pdf-tools][pdf-tools]] é uma
;; biblioteca de visualização de PDFs muito superior ao =doc-view=
;; (padrão), permitindo anotações e busca real.
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; Integração com LaTeX (AUCTeX)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  :custom
  (pdf-annot-activate-created-annotations t "Ativa anotações automaticamente"))

;; https://github.com/atykhonov/google-translate
(use-package google-translate
  :ensure t
  :custom
  (google-translate-translation-directions-alist
   '(("pt-br" . "en") ("en" . "pt-br")))
  :config
  (require 'google-translate-smooth-ui))

;; Multiple cursors 
;;
;; https://melpa.org/#/multiple-cursors
;;
(use-package multiple-cursors
  :ensure t
  :config
  (defun mc/toggle-cursor-at-point ()
    "Add or remove a cursor at point."
    (interactive)
    (if multiple-cursors-mode
        (message "Cannot toggle cursor at point while `multiple-cursors-mode' is active.")
      (let ((existing (mc/fake-cursor-at-point)))
        (if existing
            (mc/remove-fake-cursor existing)
          (mc/create-fake-cursor-at-point)))))
  (add-to-list 'mc/cmds-to-run-once 'mc/toggle-cursor-at-point)
  (add-to-list 'mc/cmds-to-run-once 'multiple-cursors-mode)
  :bind (;; Mouse and custom bindings
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-S-SPC" . mc/toggle-cursor-at-point)
         ("<C-S-return>" . multiple-cursors-mode)
         ;; Standard multiple-cursors bindings
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-c C->" . mc/skip-to-next-like-this)
         ("C-c C-<" . mc/skip-to-previous-like-this)))

;; Visual regex search on steroids
;;
;;
;; https://github.com/benma/visual-regexp-steroids.el
;; Deferred: only loads when keybinding is used
;;
(use-package visual-regexp-steroids
  :ensure t  
  :defer t
  :bind
  (("C-c q" . vr/query-replace)))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :preface
  ;; Usar &rest permite aceitar os argumentos adicionais que o dired envia
  (defun my/nerd-icons-icon-for-file (file &rest _)
    (nerd-icons-icon-for-file file :height 0.9 :v-adjust 0.45))

  (defun my/nerd-icons-icon-for-dir (dir &rest _)
    (nerd-icons-icon-for-dir dir :height 0.9 :v-adjust 0.45))

  :custom
  (nerd-icons-dired-file-icon-function #'my/nerd-icons-icon-for-file)
  (nerd-icons-dired-dir-icon-function #'my/nerd-icons-icon-for-dir)
  :hook
  (dired-mode . nerd-icons-dired-mode))


(when (version<= "28.1" emacs-version)
  (use-package dired-preview
    :ensure t
    :hook
    (dired-mode-hook . dired-preview-mode)))

;;; External API

(use-package emacs
  :ensure nil
   :hook (emacs-startup . delete-other-windows))

(provide 'pathogen-ui)
;;; pathogen-ui.el ends here
