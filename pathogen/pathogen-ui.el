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
(when (version<= "29.1" emacs-version)
    (require 'pathogen-icr-vompeccc)
  (require 'pathogen-icr-builtin))

;; (require 'pathogen-avy)

(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer))
  
  ;; :init
  ;; (defgroup pathogen-icr-avy nil
  ;;   "ICR action extensions for Avy."
  ;;   :group 'avy)

  :config

  ;; avy-goto-line no org-mode: fix para hints em todas as linhas visíveis
  ;;
  ;; Problema: o org-modern marca certas linhas com a propriedade 'invisible'
  ;; usando o valor 'org-modern (em vez de nil ou org-link, que são os únicos
  ;; valores que avy--line aceita por padrão). Com isso, avy pulava silenciosamente
  ;; essas linhas ao executar avy-goto-line, deixando partes do buffer sem hint.
  ;;
  ;; Solução: reimplementar avy--line localmente adicionando 'org-modern à lista
  ;; de valores permitidos para a propriedade 'invisible. O restante da lógica
  ;; é idêntico ao avy--line original (avy 0.4.0+).
  ;; (with-eval-after-load 'avy
  ;;   (defun my/avy-goto-line-all-visible ()
  ;;     (interactive)
  ;;     (avy-with avy-goto-line
  ;;       (let (candidates)
  ;;         (avy-dowindows nil
  ;;           (let ((ws (window-start)))
  ;;             (save-excursion
  ;;               (save-restriction
  ;;                 (narrow-to-region ws (window-end (selected-window) t))
  ;;                 (goto-char (point-min))
  ;;                 (while (< (point) (point-max))
  ;;                   (when (member (get-char-property
  ;;                                  (max (1- (point)) ws) 'invisible)
  ;;                                 '(nil org-link org-modern)) ;; <-- adicionado
  ;;                     (push (cons
  ;;                            (if (eq avy-style 'post)
  ;;                                (line-end-position)
  ;;                              (line-beginning-position))
  ;;                            (selected-window))
  ;;                           candidates))
  ;;                   (forward-line 1))))))
  ;;         (avy-process (nreverse candidates))))))

  ;;;; pre-tip: coloca o hint ANTES do conteúdo, simulando margem
  ;;(setq avy-style 'pre)
  ;;(setq avy-styles-alist '((avy-goto-line . pre)))
  ;;
  ;;;; Cores que diferenciam hint do código
  ;;(custom-set-faces
  ;; '(avy-lead-face   ((t (:foreground "#ff6c6b" :background "#23272e" :bold t))))
  ;; '(avy-lead-face-0 ((t (:foreground "#98be65" :background "#23272e" :bold t)))))
  
  ;; (setq avy-keys '(?q ?e ?r ?y ?u ?o ?p
  ;;                     ?a ?s ?d ?f ?g ?h ?j
  ;;                     ?k ?l ?' ?c ?v ?b
  ;;                     ?n ?, ?/))

  ;; --- Internal Helpers ---

  (defun pathogen-icr-avy--kill-line-stay (pt)
    "Kill the entire line at PT without moving the current cursor."
    (save-excursion
      (goto-char pt)
      (let ((kill-whole-line t))
        (forward-line 0)
        (kill-line))))

  ;; --- Pathogen ICR Avy Custom Actions ---

  (defun pathogen-icr-avy-action-teleport (pt)
    "Move (teleport) the s-expression at PT to the current cursor position."
    (avy-action-kill-stay pt)
    (save-excursion (yank))
    t)

  (defun pathogen-icr-avy-action-teleport-line (pt)
    "Move (teleport) the entire line at PT to the current cursor position."
    (pathogen-icr-avy--kill-line-stay pt)
    (save-excursion (yank))
    t)

  (defun pathogen-icr-avy-action-copy-line (pt)
    "Copy the entire line at PT without moving the current cursor."
    (save-excursion
      (goto-char pt)
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (kill-new (buffer-substring-no-properties beg end))))
    t)

  (defun pathogen-icr-avy-action-embark (pt)
    "Trigger the Embark contextual actions menu at PT without losing current focus."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (when (fboundp 'embark-act)
            (embark-act)))
      (when (and (boundp 'avy-ring) (not (ring-empty-p avy-ring)))
        (select-window (cdr (ring-ref avy-ring 0)))))
    t)

  ;; --- Mapping Verbs to the Avy Dispatch Alist ---
  (setq avy-dispatch-alist
        `((?x . avy-action-kill-stay)
          (?X . ,#'pathogen-icr-avy--kill-line-stay)   ; Fixed line-kill helper
          (?w . avy-action-copy)
          (?W . pathogen-icr-avy-action-copy-line)     ; Added safe remote line copy
          (?t . pathogen-icr-avy-action-teleport)
          (?T . pathogen-icr-avy-action-teleport-line) ; Fixed line-teleport helper
          (?. . pathogen-icr-avy-action-embark))) ; Run Embark on remote target

  :bind
  ("M-g M-g" . avy-goto-line)
  ;;("M-g M-g" . my/avy-goto-line-all-visible)
  )

;;;; Version control
(when (version<= "28.1" emacs-version)

  ;; Transient: Required dependency for Magit to prevent version mismatches
  ;; (setq package-pinned-packages
  ;;       '((magit . "melpa-stable")
  ;;         (magit-section . "melpa-stable")
  ;;         (transient . "melpa-stable")
  ;;         (with-editor . "melpa-stable")))

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
    :config (magit-todos-mode 1))
  )
;;;; Terminal

;; Eat and Eat powered Eshell, fast featureful terminal inside Emacs:
;; https://emacsconf.org/2023/talks/eat/
;; https://codeberg.org/akib/emacs-eat

(when (version<= "28.1" emacs-version)
(use-package eat
  :ensure t
  :after project
  :custom (eat-term-name "xterm-256color")
  :hook (eshell-load . eat-eshell-mode))
)



;; https://howardism.org/Technical/Emacs/templates-tutorial.html
;; (use-package yasnippet
;;   :ensure t
;;   :init
;;   (yas-global-mode 1)
;;   :config
;;   (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))

;;     ;; Função auxiliar do tutorial para forçar o Yasnippet a expandir o arquivo injetado
;;   (defun autoinsert-yas-expand ()
;;     "Substitui o texto no template usando o yasnippet."
;;     (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;;   ;; Associa arquivos .el ao template criado e aplica a expansão do Yasnippet
;;   (define-auto-insert "\\.el$" ["default-elisp.el" autoinsert-yas-expand]))

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

;; (use-package olivetti
;;   :ensure t
;;   :bind
;;   ("C-M-z" . olivetti-mode)
;;   :custom
;;   (olivetti-body-width 80)
;;   ;;:hook
;;   ;; (org-agenda-mode . olivetti-mode)
;;   ;; (text-mode olivetti-mode)
;;   )

;; Multiple cursors 
;;
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
  (dired-mode . nerd-icons-dired-mode)
  :config
  (add-to-list 'nerd-icons-extension-icon-alist
    `("ctx" nerd-icons-sucicon "nf-seti-tex" :face nerd-icons-lred)))


(when (version<= "28.1" emacs-version)
(use-package dired-preview
  :ensure t
  :hook
  (dired-mode-hook . dired-preview-mode))
)

;;; External API

(use-package emacs
  :ensure nil
  :config
  (defun pathogen/shell-pop (&optional arg)
    (interactive "P")
    (cond
     ;; Se C-u foi pressionado, abre o terminal na janela inteira
     (arg
      (delete-other-windows-vertically)
      (eat))
     ;; Comportamento padrão (sem C-u)
     (t
      (split-window-below)
      (other-window 1)
      (eat)
      (shrink-window 10))))

  (defun pathogen/open-config ()
    (interactive)
    (find-file pathogen-custom-file))
  
  :bind
  ("C-c t" . pathogen/shell-pop))


(provide 'pathogen-ui)
;;; pathogen-ui.el ends here
