;;; 03-setup-packages.el --- Core packages -*- lexical-binding: t; -*-
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
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; which-key
;;
;;
;; https://github.com/justbur/emacs-which-key
;;
;; One of the core ideas of Emacs is discoverability. It is a self-documented
;; editor. To see this, check =C-h ?=.
;;
;; However, after enabling a whole plethora of available packages you can get
;; lost by the messiness of the enabled shortcuts.
;;
;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup. This
;; provides a way to discover shortcuts globally.
(use-package
  which-key
  :init
  ;; Don't wait too much for help buffer popup
  (setq which-key-idle-delay 0.1)
  :config
  (which-key-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm
;;
;;
;; https://github.com/emacs-helm/helm
;;
;; Helm is an Emacs framework for incremental completions and narrowing
;; selections. It provides an easy-to-use API for developers wishing to build
;; their own Helm applications in Emacs, powerful search tools and dozens of
;; already built-in commands providing completion to almost everything. It is a
;; must-have for anyone using Emacs as a main work environment. Helm has been
;; widely adopted by many Emacs power-users. It is available in Melpa and can be
;; easily installed from the Emacs package manager.
(use-package
  helm
  ;;:defer t
  :bind
  ;; Overwrite some comands with their helm counterparts
  ("M-x" . helm-M-x)
  :config
  (require 'helm-config)
  ;; Configure helm to always popup at the bottom
  (setq helm-split-window-in-side-p t)

  (add-to-list 'display-buffer-alist
               '("\\`\\*helm.*\\*\\'"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.3)))
  (helm-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evil
;;
;;
;; https://github.com/emacs-evil/evil
;;
;; Evil is an extensible vi layer for Emacs. It emulates the main features of
;; Vim, and provides facilities for writing custom extensions.
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-disable-insert-state-bindings t)
  :config
  ;; Change cursor color according to state
  ;;;;(setq evil-normal-state-cursor '("orange" box)) 
  ;;;;(setq evil-insert-state-cursor '("green" box))
  ;;;;(setq evil-replace-state-cursor '("red" hbar))
  ;;;;(setq evil-emacs-state-cursor '("blue" box)) 
  ;;;;(setq evil-visual-state-cursor '("purple" box))
  ;;;;(setq evil-operator-state-cursor '("red" hollow))
  ;; Do not change cursor position when changing modes
  (setq evil-move-cursor-back t)
  (evil-mode 1)

  ;;(add-function :after after-focus-change-function
  ;;  (lambda ()
  ;;    (unless (frame-focus-state)
  ;;      (evil-normal-state)
  ;;      (message ""))))
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general
;;
;;
;; https://github.com/noctuid/general.el
;;
;; general.el provides a more convenient method for binding keys in emacs (for
;; both evil and non-evil users). Like use-package, which provides a convenient,
;; unified interface for managing packages, general.el is intended to provide a
;; convenient, unified interface for key definitions. While this package does
;; implement some completely new functionality (such as the ability to make
;; vim-style keybindings under non-prefix keys with an optional timeout), its
;; primary purpose is to build on existing functionality to make key definition
;; more clear and concise. general-define-key is user-extensible and supports
;; defining multiple keys in multiple keymaps at once, implicitly wrapping key
;; strings with (kbd ...), using named prefix key sequences (like the leader key
;; in vim), and much more.
(use-package general
  :config
  (setq pathogen-leader-key "SPC")
  (setq pathogen-leader-alt-key "C-SPC")

  (general-create-definer pathogen-leader-def
    :keymaps 'override
    :states '(insert emacs normal hybrid motion visual operator)
    :prefix pathogen-leader-key
    :non-normal-prefix pathogen-leader-alt-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winum
;;
;;
;; https://github.com/deb0ch/emacs-winum
;;
(use-package winum
  :config
  (winum-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dashboard
;;
;;
;; https://github.com/emacs-dashboard/emacs-dashboard
;;
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popwin
;;
;;
;; https://github.com/emacsorphanage/popwin
;;
;; Nice popup management
;;
(use-package popwin
  :config
  (popwin-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; avy
;;
;;
;; https://github.com/abo-abo/avy
;;
;; Easy navigation within buffers
(use-package avy)

(provide '03-setup-packages)
;;; 03-setup-packages.el ends here
