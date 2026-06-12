;;; init.el --- Main initialization file for Emacs -*- lexical-binding: t; fill-column: 80; -*-
;;
;; Copyright (C) 2021 Victor Santos
;;
;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Code that you want to execute when you start Emacs.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Package Manager Initialization
;;
;; Initialize the built-in package system and configure the MELPA repository
;; for Emacs 24.1 and above. To optimize startup, we first activate installed
;; packages using the version-appropriate method (`package-activate-all' for
;; Emacs 27+ or `package-initialize' for older versions). We then load the
;; local cache from disk; if no local archive metadata is found (e.g., on a
;; fresh installation), a network refresh is automatically triggered.
;;
;; References:
;; - https://melpa.org/#/getting-started
;;
(when (version<= "26.3" emacs-version)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; Refresh package contents periodically (e.g., every 2 days)
(let* ((archive-dir (expand-file-name "elpa/archives/melpa/archive-contents" user-emacs-directory))
       (days-between-updates 2) ; <--- Mude aqui para quantos dias quiser (ex: 1, 2, 7)
       (seconds-between-updates (* days-between-updates 24 60 60)))

  (if (or (not package-archive-contents) ; Se não houver cache nenhum
          (not (file-exists-p archive-dir)) ; Se o arquivo físico não existir
          (> (float-time (time-since (file-attribute-modification-time (file-attributes archive-dir))))
             seconds-between-updates)) ; Se o arquivo for mais velho que o intervalo
      (progn
        (message "O cache do MELPA está antigo. Atualizando índices...")
        (package-refresh-contents))
    (message "O cache do MELPA está atualizado (menos de %d dias)." days-between-updates)))

;; Activate packages according the installed emacs version
(if (version<= "27.1" emacs-version)
    (package-activate-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Declarative Package Management (use-package)
;;
;; Configure `use-package' to enable clean, declarative package isolation.
;; Since `use-package' is built-in starting with Emacs 29.1, we conditionally
;; install it from downstream repositories only when running on older Emacs
;; versions. Additionally, `use-package-always-ensure' is enabled globally
;; to automatically fetch and install missing packages during startup without
;; requiring explicit `:ensure t' keywords in every declaration.
;;

;; Install use-package if it's not already there
(when (version< emacs-version "29.0")
  (unless (package-installed-p 'use-package)
    (condition-case nil
        (package-install 'use-package)
      (error
       (message "Failed upon installing use-package. Updating MELPA index...")
       (package-refresh-contents)
       (package-install 'use-package)))))

;; Ativa o use-package
(require 'use-package)

;; Configuração Global do use-package
(setq use-package-always-ensure t)

;; If any package declared with `use-package' fails to install, this hook
;; forces an update of MELPA and attempts to install again automatically.
(add-hook 'use-package-ensure-failed-hook
          (lambda (package error)
            (message "Failed installing %s due to: %s. Trying to update MELPA..." package error)
            (package-refresh-contents)
            (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;
;; Custom Variables ;;
;;;;;;;;;;;;;;;;;;;;;;
(defgroup pathogen nil
  "Customization group for the Pathogen Emacs configuration."
  :group 'local
  :prefix "pathogen-")

(defcustom pathogen-local-config-file
  "~/.pathogen.el"
  "Location for additional configuration. Can be used for testing new features."
  :group 'pathogen)

(defcustom pathogen-icr-framework
  'vompeccc
  "Incremental Completing Read framework (`vompecc', `native')."
  :group 'pathogen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Core Configuration
;;
;; This section configures only built-in Emacs features and core local modules.
;; Every `use-package' block here points to something that either ships with
;; Emacs or belongs to its fundamental core layer (using `:ensure nil' or
;; local `:load-path'). This is pure, standard Emacs customization.
;;
;; The philosophy behind this setup is modularity and portability: anyone can
;; read this file, find a section they like, and copy-paste it directly into
;; their own configuration. No external package dependencies, no complex setup.
;; It just works out of the box.
;;
(use-package pathogen-core
  :load-path "pathogen/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Pathogen Visual Module Initialization
;;
;; This block loads the local custom module `pathogen-ui', which consolidates
;; all visual aesthetics, theming, font configurations, and frame layouts.
;;
(use-package pathogen-ui
  :load-path "pathogen/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Pathogen ICR (Incremental Completing Read Substrate)
;;
;; Concepts from: https://www.chiply.dev/post-icr-primer
;;
;; This configuration integrates 'pathogen-icr', establishing a programmable
;; completion substrate. Rather than treating the minibuffer as a passive
;; text-input widget, it enables data-driven, interactive workflows by decoupling
;; data retrieval from UI rendering.
;;
;; It bridges the local system's capabilities with the modular VOMPECCC stack
;; (Vertico/Orderless/Marginalia/Embark/Consult), enabling structural,
;; candidate-first actions and dynamic contextual narrowing.
;;
(cond
 ((eq pathogen-icr-framework 'vompeccc)
  (progn (use-package pathogen-icr-vompeccc
           :load-path "pathogen/")
         (use-package pathogen-icr-avy-extensions
           :load-path "pathogen/")))
 (t
  (progn (use-package pathogen-icr-native
           :load-path "pathogen/")
         ;; Like avy-goto-line, but using builtin features
         (use-package pathogen-goto-line-numbers
           :load-path "pathogen/utils"
           :init
           (display-line-numbers-mode -1)
           :config
           ;; Activates the package and its global remap behavior
           (pathogen-goto-line-numbers-mode 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Text Editing & Transformation
;;
;; Enhances in-buffer text manipulation capabilities.
;;
(use-package pathogen-editing
  :load-path "pathogen/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Window & Workspace Management
;;
;; Controls screen real estate, layout rotation, and popups.
;;
(use-package pathogen-window-management
  :load-path "pathogen/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Version Control (Git)
;;
;; Provides an interface for Git repositories.
;;
(use-package pathogen-versioning
  :load-path "pathogen/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Pathogen Terminal Setup
;;
;; Load and initialize the custom 'pathogen-terminal' package from the local
;; directory.
;;
(use-package pathogen-terminal
  :load-path "pathogen/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
;;; Modeline configuration
;;
(use-package pathogen-modeline
  :load-path "pathogen/")

;;;;;;;;;;;;;;;;;;;
;; Mini packages ;;
;;;;;;;;;;;;;;;;;;;

;; This replaces hl-todo -----------------------------------------------------80
(use-package pathogen-highlight-keywords
  :load-path "pathogen/utils")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load local custom configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The file is not loaded if you set PATHOGEN_DISABLE envvar
(when (and (file-exists-p pathogen-local-config-file) (not (getenv "PATHOGEN_DISABLE")))
  (load-file pathogen-local-config-file))

;;; init.el ends here
