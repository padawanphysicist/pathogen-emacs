;;; pathogen-icr-builtin.el --- Completion system using emacs built-ins -*- lexical-binding: t; -*-

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

;;; Code:

;; =============================================================================
;; Configuração do Icomplete Vertical para Emacs 27.1
;; =============================================================================

;; 2. Estilos e Comportamento de Busca
;; Tenta corresponder por substring, flex (fuzzy) e parcial
(setq completion-styles '(substring flex partial-completion))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles basic partial-completion))))

;; Ajustes gerais de UX do Icomplete
(setq icomplete-compute-delay 0)            ; Exibe as sugestões instantaneamente
(setq icomplete-prospects-height 10)        ; Número de candidatos exibidos na vertical
(setq icomplete-show-key-bindings t)        ; Mostra atalhos de teclado no M-x
(setq icomplete-hide-common-prefix nil)     ; Mantém o prefixo visível para clareza

;; 3. Ativação e Configuração do Icomplete Vertical
(use-package icomplete-vertical
  :ensure t
  :init
  (icomplete-mode 1)
  (icomplete-vertical-mode 1)
  :bind
  (:map icomplete-minibuffer-map
        ("C-n"   . icomplete-forward-completions)      ; Próximo candidato
        ("C-p"   . icomplete-backward-completions)     ; Candidato anterior
        ("<down>". icomplete-forward-completions)
        ("<up>"  . icomplete-backward-completions)
        ("RET"   . icomplete-force-complete-and-exit) ; Seleciona o item atual
        ("C-j"   . minibuffer-complete-and-exit)))    ; Aceita o texto exato digitado

(provide 'pathogen-icr-icomplete)
;;; pathogen-icr-icomplete.el ends here
