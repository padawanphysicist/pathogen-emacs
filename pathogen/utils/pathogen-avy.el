;;; pathogen-avy.el --- Avy and action extensions -*- lexical-binding: t; -*-

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
;; This package implements the "Filter -> Select -> Act" paradigm (Incremental
;; Completing Read) using Avy as a 2D spatial selector. It allows you to trigger
;; complex remote actions (copying, moving, killing, or dispatching Embark)
;; on any visible text target without ever moving your point/cursor.
;;
;; https://karthinks.com/software/avy-can-do-anything/

;;; Code:

(use-package avy
  :ensure t
  :bind
  (("M-j" . avy-goto-char-timer))
  
  :init
  (defgroup pathogen-icr-avy nil
    "ICR action extensions for Avy."
    :group 'avy)

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

(provide 'pathogen-avy)
;;; pathogen-avy.el ends here
