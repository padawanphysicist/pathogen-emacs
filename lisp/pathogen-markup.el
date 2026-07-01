;;; pathogen-markup.el --- Configuration for markup languages -*- lexical-binding: t; -*-

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

(use-package org
  ;; Forces Elpaca to install Org before continuing
  :ensure (:wait t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 🔗 [[https://jblevins.org/projects/markdown-mode/][markdown-mode]]
;;
;; O modo principal para editar arquivos Markdown. Configurado para
;; usar GitHub Flavored Markdown (GFM) em arquivos README e suporte a
;; renderização externa.
;;
(use-package markdown-mode
  :ensure t
  ;; Usa o modo do GitHub para arquivos README
  :mode ("README\\.md\\'" . gfm-mode)
  :init 
  ;; Define o comando externo para exportação/preview (ex: multimarkdown ou pandoc)
  (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;;; YAML
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package outline-yaml
  :ensure (:type git
           :host github
           :repo "jamescherti/outline-yaml.el")
  :hook
  ((yaml-mode . outline-yaml-minor-mode)
   (yaml-ts-mode . outline-yaml-minor-mode)))


;; TeX/LaTeX/ConTeXt
(use-package auctex :ensure t
  :config
  ;; Set .ctx files to ConTeXt mode
  (add-to-list 'auto-mode-alist '("\\.ctx\\'" . ConTeXt-mode))
  (add-to-list 'auto-mode-alist '("\\.mkiv\\'" . ConTeXt-mode))
  ;; Set default ConTeXt engine to ConTeXt (instead of texexec)
  (setq-default TeX-command-default "ConTeXt")

  ;; Set ConTeXt engine version to Mark IV or LMTX (IV by default, change to VI if desired)
  (setq-default ConTeXt-Mark-version "IV"))

(use-package cdlatex
  :ensure t
  :after (latex-mode org-mode) ; Load after these modes
  :config
  (define-key cdlatex-mode-map (kbd "TAB") 'cdlatex-tab)
  ;; Add any other specific configurations here
  ;; For example, to enable CDLaTeX in Org mode:
  (add-hook 'org-mode-hook 'cdlatex-mode)
  ;; Or to enable it in LaTeX mode:
  (add-hook 'latex-mode-hook 'cdlatex-mode))


(provide 'pathogen-markup)
;;; pathogen-markup.el ends here
