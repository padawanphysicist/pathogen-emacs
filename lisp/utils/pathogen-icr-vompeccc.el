;;; pathogen-icr-vompeccc.el --- Incremental Completing Read (ICR) Substrate using VOMPECCC Stack -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs "27.2"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://codeberg.org/padawanphysicist/pathogen-emacs

;; This file is NOT part of GNU Emacs.

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

;; This package implements the core concepts outlined in the article:
;; URL: https://www.chiply.dev/post-icr-primer
;;
;; DESIGN PHILOSOPHY (The ICR Substrate):
;; As argued in the chiply.dev primer, "Incremental Completing Read" (ICR) should
;; not be treated as a mere UI convenience or an isolated search box widget.
;; Instead, it is approached as a deep structural property and a programmable
;; substrate exposed by the Emacs ecosystem.
;;
;; The purpose of this file is to demonstrate pure completion as an architectural
;; primitive. It decouples business logic from the graphical interface, allowing
;; complex data streams (such as API responses, file systems, or metadata collections)
;; to flow seamlessly over the modular Emacs modern infrastructure.
;;
;; MODULAR ARCHITECTURE (The VOMPECCC Stack):
;; To align with the ideal ICR philosophy, this file integrates orthogonally with
;; the modern modular Emacs stack:
;;   V - Vertico     : Provides the minimal vertical interactive UI for the minibuffer.
;;   O - Orderless   : Provides the flexible, non-literal, incremental matching engine.
;;   M - Marginalia  : Rich metadata annotations added dynamically at runtime.
;;   P - Prot (MCT)  : (Optional) Alternative completion buffer interaction.
;;   E - Embark      : The "keyboard click system" to act upon targeted candidates.
;;   C - Consult     : Search, filtering, and synchronous/asynchronous preview primitives.
;;   C - Corfu       : Fast and lightweight in-buffer completion.
;;   C - Cape        : Completion At Point Extensions (flexible backend completion providers).
;;
;; IMPLEMENTED CONCEPS:
;; 1. UI Dependency Inversion: Functions generate pure lists or dynamic completion
;;    tables completely agnostic of the minibuffer visual layout.
;; 2. Dynamic Annotation via Substrate: Leveraging text properties or Marginalia
;;    hooks to attach rich metadata without polluting the raw completion strings.
;; 3. Candidate-Action Workflows (Embark Integration): Setting up custom completion
;;    categories so any string selected via ICR can be passed to targeted,
;;    context-aware commands (Action-over-Candidate).
;;
;; EXAMPLE USAGE:
;;   (require 'pathogen-icr)
;;

;;; Code:

;; ==========================================================================
;; 0. CORE PREREQUISITES
;; ==========================================================================

(use-package compat
  :ensure t
  )

;; ==========================================================================
;; 1. VERTICO (The Visual Substrate)
;; ==========================================================================
;; Vertico provides a minimal, high-performance vertical UI for the minibuffer
;; while respecting and leveraging Emacs' native completion systems.

(use-package vertico
  :ensure t
  :after compat
  :custom
  (vertico-cycle t "Allow cycling from the last candidate back to the first")
  (vertico-resize nil "Freeze minibuffer size to prevent jarring UI jitter")
  (read-file-name-completion-ignore-case t "Ignore case when searching files")
  (read-buffer-completion-ignore-case t "Ignore case when searching buffers")
  :init
  (vertico-mode 1))

;; ==========================================================================
;; 2. ORDERLESS (The Matching Engine)
;; ==========================================================================
;; Orderless splits the minibuffer input into space-separated components,
;; matching them in *any order*. This is the mathematical core of ICR.

(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; CRITICAL IMPROVEMENT: Fall back to partial-completion for files (e.g., TRAMP/paths)
  (completion-category-overrides '((file (styles partial-completion orderless basic))))
  ;; Regexp and initialism matching style (e.g., "m-x" matches "execute-extended-command")
  (orderless-matching-styles '(orderless-initialism orderless-regexp)))

;; ==========================================================================
;; 3. MARGINALIA (The Contextual Metadata Layer)
;; ==========================================================================
;; Marginalia runs in the background, adding text properties and rich context 
;; (permissions, docstrings, variable values) to minibuffer candidates dynamically.

(use-package marginalia
  :ensure t
  :after vertico
  :hook
  (after-init . marginalia-mode))

;; ==========================================================================
;; 4. PRESCIENT (The Frecency Optimization Layer)
;; ==========================================================================
;; Prescient introduces sorting based on frequency and recency ("frecency").
;; It remembers your favorite commands and bubbles them to the top.

(use-package prescient
  :ensure t
  :custom
  (prescient-history-length 150)
  (prescient-save-parsed-modes t)
  :config
  ;; Persist history across Emacs sessions
  (prescient-persist-mode 1))

;; ==========================================================================
;; 5. EMBARK (The Action/Targeting System)
;; ==========================================================================
;; Embark acts as a context menu ("Right-Click for the Keyboard"). It turns 
;; completion candidates into active targets for structural operations.

(use-package embark
  :ensure t
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ; Universal action trigger
   ("M-." . embark-dwim)        ; Context-aware "Do What I Mean"
   ("C-h B" . embark-bindings)) ; Contextual replacement for describe-bindings
  :config
  ;; AVY INTEGRATION: Jump to a character on screen and immediately run an Embark action on it
  (with-eval-after-load 'avy
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)))

;; Bridges the gap between Embark actions and Consult search snapshots
(use-package embark-consult
  :ensure t
  :after (vertico embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ==========================================================================
;; 6. CONSULT (The Command & Live-Preview Engine)
;; ==========================================================================
;; Consult provides optimized search and navigation commands wrapped around 
;; Vertico, generating decoupled live previews asynchronously.

(use-package consult
  :ensure t
  :after vertico
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (
         ;; Mode-specific quick access maps (C-c)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ([f9] . consult-theme)

         ;; Global Control Navigation Map (C-x)
         ("C-x C-r" . consult-recent-file)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)

         ;; Registers and Editing (M-# / M-y)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)

         ;; Code Navigation Map (M-g)
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; Asynchronous System Search Map (M-s)
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)

         ;; Optimized Isearch Hijack
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)

         ;; Live Minibuffer History Scrolling
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :init
  ;; Improvement: Use the "<" character for rapid category narrowing (e.g. within buffers)
  (setq consult-narrow-key "<")
  :config
  ;; PERFORMANCE IMPROVEMENT: Add a safe debouncing delay to heavy live previews.
  ;; This stops Emacs from lagging out when rapid-cycling through files or themes.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

;; ==========================================================================
;; 7. CORFU (The In-Buffer UI Element)
;; ==========================================================================
;; Corfu acts as the Vertico equivalent but directly inside the editing buffer,
;; providing fast, non-intrusive auto-completion popups.

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t "Trigger popup list automatically while typing")
  (corfu-auto-delay 0.2 "Wait 200ms before drawing the popup window")
  (corfu-auto-prefix 2 "Require at least 2 characters to start completing")
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  (global-corfu-minibuffer nil "Keep corfu completely away from the minibuffer core")
  :init
  (global-corfu-mode 1))

;; Enable Frecency sorting inside Corfu popups using Prescient
(use-package corfu-prescient
  :ensure t
  :after (corfu prescient)
  :config
  (corfu-prescient-mode 1))

;; Adds elegant UI iconography to your completion choices
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Standard Emacs baseline overrides required to make Corfu perform flawlessly
(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete "TAB handles both indentation and code expansion")
  (text-mode-ispell-word-completion nil "Disable old laggy spelling completion routines")
  (read-extended-command-predicate #'command-completion-default-include-p "Hide irrelevant mode commands from M-x"))

;; ==========================================================================
;; 8. CAPE (Completion At Point Extensions / Backends)
;; ==========================================================================
;; Cape provides modular backends supplying raw data to Corfu's front UI layer.

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Append universal, context-free data providers globally.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  :config
  ;; ARCHITECTURAL IMPROVEMENT: Cleanly disable global data crawlers (like dabbrev)
  ;; inside the minibuffer to prevent variable pollution during inputs.
  (defun my/disable-cape-in-minibuffer ()
    "Isolate the minibuffer from intrusive local buffer completion crawlers."
    (setq-local completion-at-point-functions
                (seq-remove (lambda (f)
                              (memq f '(cape-dabbrev cape-file cape-elisp-block)))
                            completion-at-point-functions)))

  (add-hook 'minibuffer-setup-hook #'my/disable-cape-in-minibuffer))

(provide 'pathogen-icr-vompeccc)
;;; pathogen-icr-vompeccc.el ends here
