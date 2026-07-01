;;; pathogen-terminal.el --- Terminal settings -*- lexical-binding: t; -*-

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

;; Eat and Eat powered Eshell, fast featureful terminal inside Emacs:
;; https://emacsconf.org/2023/talks/eat/

(use-package eat
  :ensure t
  :after project
  :custom (eat-term-name "xterm-256color")
  :hook (eshell-load . eat-eshell-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Project + Eat Terminal Integration
;;
;; Problem: defining eat-project inside `use-package eat :config` meant the
;; function and project-switch-commands entry only existed after eat was loaded
;; — which happens lazily, i.e.  only when first required. So the menu entry was
;; never registered at startup.
;;
;; Additionally, wrapping everything in `with-eval-after-load 'project` inside
;; eat's :config block was redundant: because eat declared `:after project`, by
;; the time :config ran, project was already loaded — but eat itself still
;; wasn't guaranteed to load early enough for the side effects to take.
;;
;; Solution: move the integration into a standalone `with-eval-after-load
;; 'project` block, completely outside any use-package declaration. Since
;; project is loaded eagerly via `:demand t`, this block fires reliably during
;; init.
;;
;; The `require 'eat` call inside eat-project handles lazy loading of eat itself
;; — it loads only when the terminal is actually invoked, not at startup. This
;; keeps startup fast while ensuring the menu entry and keybinding are always
;; registered.
;;
(use-package emacs
  :ensure nil
  :config
  (with-eval-after-load 'project
    (defun eat-project ()
      "Open the Eat terminal in the root of the current project."
      (interactive)
      (require 'eat)
      (let ((default-directory (project-root (project-current t))))
        (eat)))

    (define-key project-prefix-map "t" '("Eat terminal" . eat-project))

    (add-to-list 'project-switch-commands
                 '(eat-project "Eat terminal" ?t) t)))

(use-package emacs
  :config
(defun pathogen/shell-pop ()
  (interactive)
  ;; (split-window-below &optional SIZE WINDOW-TO-SPLIT)
  (split-window-below) ;; 1. Cria nova janela abaixo
  (other-window 1) ;; 2. vai para a outra janela
  (eat)
  (shrink-window 10))
:bind
("C-c t" . pathogen/shell-pop)
)
;; (use-package shell-pop
;;   :ensure t
;;   :bind (("C-c t" . shell-pop))
;;   :custom
;;   (shell-pop-universal-key "C-c t")
;;   (shell-pop-window-position "bottom")
;;   (shell-pop-full-span nil)
;;   (shell-pop-term-shell shell-file-name)
;;   (shell-pop-window-size 30)
;;   (shell-pop-autocd-to-working-dir nil))

;; (with-eval-after-load 'shell-pop
;;   (setopt shell-pop-shell-type
;;           `("eat" "*eat*"
;;             (lambda ()
;;               (when (fboundp 'eat)
;;                 ;; Verifica se o project.el (nativo do Emacs) encontra um projeto
;;                 (let ((default-directory (if-let ((proj (project-current)))
;;                                              (project-root proj)
;;                                            default-directory)))
;;                   (eat shell-pop-term-shell)))))))



(provide 'pathogen-terminal)
;;; pathogen-terminal.el ends here
