;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pathogen-ui.el --- Visual Aesthetics & Interface Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This module defines the user interface, typography, and visual ecosystem for
;; the 'pathogen' Emacs configuration. It strips away default GUI clutter to
;; establish a clean, modern, and distraction-free editing environment.
;;
;; Key areas managed within this module:
;; - Themes & Colors: Loading color palettes, configuring faces, and styling the
;;   global modeline.
;; - Typography: Defining default, fixed-pitch, and variable-pitch fonts alongside
;;   line-spacing tweaks.
;; - Frame Geometry: Disabling redundant graphical components (toolbars, scrollbars)
;;   and setting startup window bounds.
;; - Visual Indicators: Customizing fringes, window dividers, and subtle feedback
;;   mechanisms.
;;
;; Part of the 'pathogen' modular configuration environment.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom pathogen-font-family "Mononoki Nerd Font Mono"
  "The default fixed-pitch font family used across the environment."
  :type 'string
  :group 'pathogen)

(defcustom pathogen-variable-font-family "Mononoki Nerd Font Mono"
  "The default variable-pitch font family used for prose and prose-like buffers."
  :type 'string
  :group 'pathogen)

(defcustom pathogen-font-size 150
  "The default font size represented as an integer (height * 10).
For example, 170 equals a 17pt font size."
  :type 'integer
  :group 'pathogen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Themes & Color Profiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This ensures we use the version >5
(use-package modus-themes
  :ensure t
  :demand t)

;; Custom flavor palette loaded from a personalized Codeberg fork.
;; See: https://codeberg.org/padawanphysicist/modus-catppuccin
(use-package modus-catppuccin
  :vc (:url "https://codeberg.org/padawanphysicist/modus-catppuccin"
       :rev :newest)
  :after modus-themes
  :demand t
  :config
  ;; Available flavors: 'frappe', 'latte', 'macchiato', or 'mocha'
  (load-theme 'catppuccin-frappe :no-confirm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Typography & Font Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Fontaine (Font Preset Switcher) ---
;; Handles multiple font setups, allowing seamless shifting between presets 
;; (e.g., standard coding versus a large presentation mode).
(use-package fontaine
  :ensure t
  :demand t
  :bind ("C-c f" . fontaine-set-preset)
  :custom
  (fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))
  (fontaine-presets
   `((regular
      :default-family ,pathogen-font-family
      :default-height ,pathogen-font-size
      :variable-pitch-family ,pathogen-variable-font-family)))
  :init
  (defun pathogen--fontaine-update-presets ()
    "Update Fontaine presets using current values from global variables."
    (setq fontaine-presets
          `((regular
             :default-family ,pathogen-font-family
             :default-height ,pathogen-font-size)
            (presentation
             :default-height 180
             :default-weight bold)
            (small
             :default-height 90)))
    (fontaine-set-preset 'regular))

  (defun pathogen--fontaine-variable-watcher (_symbol _newval _operation _where)
    "Watcher hook to dynamically trigger updates when font variables change."
    (run-at-time 0 nil #'pathogen--fontaine-update-presets))
  :config
  (pathogen--fontaine-update-presets)
  (fontaine-mode 1)
  ;; Attach variable watchers to redraw the screen immediately on custom variable updates
  (add-variable-watcher 'pathogen-font-family #'pathogen--fontaine-variable-watcher)
  (add-variable-watcher 'pathogen-font-size #'pathogen--fontaine-variable-watcher))

;; --- Persist Text Scale ---
;; Remembers custom per-buffer text scaling factor changes across editing sessions.
;; (use-package persist-text-scale
;;   :ensure t
;;   :custom
;;   (persist-text-scale-file (expand-file-name "persist-text-scale" pathogen-cache-directory))
;;   ;; Write to disk every 7 minutes to minimize redundant disk I/O operations
;;   (persist-text-scale-autosave-interval (* 7 60))
;;   :config
;;   (persist-text-scale-mode 1)
  
;;   ;; Impede que o persist-text-scale altere ou salve o tamanho da fonte do minibuffer
;;   (add-hook 'minibuffer-setup-hook
;;             (lambda ()
;;               (setq-local persist-text-scale-mode nil)
;;               (text-scale-set 0)))
;;   ;; Impede que o persist-text-scale altere ou salve o tamanho da fonte do org-agenda
;;   (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (setq-local persist-text-scale-mode nil)
;;             (text-scale-set 0)))
;;     ;; Impede que o persist-text-scale altere ou salve o tamanho da fonte do org-capture
;;   (add-hook 'org-capture-mode-hook
;;           (lambda ()
;;             (setq-local persist-text-scale-mode nil)
;;             (text-scale-set 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Visual Cues & Syntax Highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Line Highlight (Hl-Line) ---
;; Highlights the current active line to provide context in large viewports.
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; --- Keyword Decoration (Hl-Todo) ---
;; Highlights special markers like TODO, FIXME, and HACK inside code buffers.
(use-package hl-todo
  :ensure t
  :config
  (defun my-modus-themes-hl-todo-faces (&rest _)
    "Map custom keyword colors to match the loaded theme palette hooks dynamically.
See: https://www.gnu.org/software/emacs/manual/html_node/modus-themes/DIY-Custom-hl_002dtodo-colors.html"
    (setq hl-todo-keyword-faces '(("TODO" .  "#ff0000")
                                  ("HACK" .  "#ffff00")
                                  ("XXX" .   "#00ffff")
                                  ("NOTE" .  "#ff00ff"))))
  
  ;; Sync highlighting accents on theme switch events
  (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-hl-todo-faces)
  (global-hl-todo-mode 1))

;; --- Screen Dimming (Dimmer) ---
;; Dims out background or inactive windows to visually emphasize focus.
(use-package dimmer
  :ensure t
  :config
  ;; Modus theme recommended properties for proper contrast preservation
  (setq dimmer-fraction 0.3)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorspace :rgb)
  (dimmer-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. Iconography
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Nerd Icons Base ---
;; Essential font icon map dependencies utilized by the modeline and child packages.
(use-package nerd-icons
  :ensure t)

;; --- Dired Integration ---
;; Implements specific scale and vertical alignments for Nerd Icons in Dired.
(use-package nerd-icons-dired
  :ensure t
  :after (nerd-icons)
  :preface
  (defun pathogen--nerd-icons-icon-for-file (file &rest _args)
    "Scale down and vertically align icons mapped to standard files."
    (nerd-icons-icon-for-file file :height 0.9 :v-adjust 0.45))

  (defun pathogen--nerd-icons-icon-for-dir (dir &rest _args)
    "Scale down and vertically align icons mapped to directory paths."
    (nerd-icons-icon-for-dir dir :height 0.9 :v-adjust 0.45))
  :custom
  (nerd-icons-dired-file-icon-function #'pathogen--nerd-icons-icon-for-file)
  (nerd-icons-dired-dir-icon-function #'pathogen--nerd-icons-icon-for-dir)
  :hook
  (dired-mode . nerd-icons-dired-mode))


(provide 'pathogen-ui)
;;; pathogen-ui.el ends here
