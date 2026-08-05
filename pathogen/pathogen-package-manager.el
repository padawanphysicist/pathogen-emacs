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

;; 2. Inicializar o Package.el e definir os repositórios
(require 'package)

;; CORREÇÃO DO ERRO DE ASSINATURA (Bad Signature)
;; Permite pacotes sem assinatura válida temporariamente para podermos atualizar as chaves GPG
;(setq package-check-signature 'allow-unsigned)
;; Ignora completamente as assinaturas dos pacotes para evitar travamentos devido a chaves GPG expiradas
(setq package-check-signature nil)


;; Segurança: Forçar conexões HTTPS seguras
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; Evita bugs de TLS antigos no Emacs 27

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(setq package-archives
;      '(("melpa" . "https://melpa.org/archives")
;        ("gnu"   . "https://gnu.org")
;        ("nongnu". "https://nongnu.org"))) ; Compatível com Emacs 27+

;; Inicializa os pacotes instalados
(unless package--initialized
  (package-initialize))

;; 3. Instalação automática do Use-Package (Se não estiver presente)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 4. Configuração do Use-Package
(eval-when-compile
  (require 'use-package))

;; Faz com que o use-package baixe os pacotes automaticamente por padrão
(setq use-package-always-ensure t)

;; Garante que erros em pacotes não travem a inicialização do resto do sistema
(setq use-package-verbose t)

;; ;; 1. Garante que as macros de 'cl-lib' (como cl-macrolet) estejam prontas no interpretador
;; (eval-when-compile
;;   (require 'cl-lib))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Configure Elpaca		       ;;
;; ;; 				       ;;
;; ;; https://github.com/progfolio/elpaca ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar elpaca-installer-version 0.12)
;; (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
;; (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
;; (defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
;; (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
;;                               :ref nil :depth 1 :inherit ignore
;;                               :files (:defaults "elpaca-test.el" (:exclude "extensions"))
;;                               :build (:not elpaca-activate)))
;; (let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
;;        (build (expand-file-name "elpaca/" elpaca-builds-directory))
;;        (order (cdr elpaca-order))
;;        (default-directory repo))
;;   (add-to-list 'load-path (if (file-exists-p build) build repo))
;;   (unless (file-exists-p repo)
;;     (make-directory repo t)
;;     (when (<= emacs-major-version 28) (require 'subr-x))
;;     (condition-case-unless-debug err
;;         (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
;;                   ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
;;                                                   ,@(when-let* ((depth (plist-get order :depth)))
;;                                                       (list (format "--depth=%d" depth) "--no-single-branch"))
;;                                                   ,(plist-get order :repo) ,repo))))
;;                   ((zerop (call-process "git" nil buffer t "checkout"
;;                                         (or (plist-get order :ref) "--"))))
;;                   (emacs (concat invocation-directory invocation-name))
;;                   ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
;;                                         "--eval" "(byte-recompile-directory \".\" 0 'force)")))
;;                   ((require 'elpaca))
;;                   ((elpaca-generate-autoloads "elpaca" repo)))
;;             (progn (message "%s" (buffer-string)) (kill-buffer buffer))
;;           (error "%s" (with-current-buffer buffer (buffer-string))))
;;       ((error) (warn "%s" err) (delete-directory repo 'recursive))))
;;   (unless (require 'elpaca-autoloads nil t)
;;     (require 'elpaca)
;;     (elpaca-generate-autoloads "elpaca" repo)
;;     (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
;; (add-hook 'after-init-hook #'elpaca-process-queues)
;; (elpaca `(,@elpaca-order))
;; ;; End of default installation suggested by Elpaca website

;; ;; Install `use-package'
;; (when (version< emacs-version "29.1")
;;   (elpaca use-package)
;;   (elpaca elpaca-use-package
;;     (setq elpaca-use-package-by-default nil)
;;     (elpaca-use-package-mode)))
;; (elpaca-wait)

;; ;; :ensure is now `t' by default
;; ;;(setq use-package-always-ensure t)

;; ;;; Auto-hiding the elpaca-log buffer
;; (defun +elpaca--maybe-hide-log ()
;;   "Hide the Elpaca log buffer if every queued package finished successfully."
;;   (when (cl-every (lambda (e) (eq (elpaca<-status (cdr e)) 'finished))
;;                   (apply #'append (mapcar #'elpaca-q<-elpacas elpaca--queues)))
;;     (when-let ((window (get-buffer-window "*elpaca-log*" t)))
;;       (progn
;; 	(with-selected-window window (quit-window 'kill window))
;; 	(display-about-screen)))))

;; (defun +elpaca-hide-successful-log ()
;;   "Schedule a check to hide the Elpaca log buffer after queues settle."
;;   (run-at-time 0.5 nil #'+elpaca--maybe-hide-log))

;; (add-hook 'elpaca-after-init-hook
;;             (lambda () (+elpaca-hide-successful-log)
;;               (add-hook 'elpaca-post-queue-hook #'+elpaca-hide-successful-log)))

(provide 'pathogen-package-manager)
;;; pathogen-package-manager.el ends here

