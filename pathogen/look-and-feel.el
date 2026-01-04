;;; Custom variables for this module

(defvar pathogen-font-family "GeistMono Nerd Font"
  "The default font family.")

(defvar pathogen-font-size 130
  "The default font height.")

;;; Package configuration

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  :init (doom-modeline-mode 1))

(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border "grey30")

;; 1. Pigmentation (Themes)
(use-package ef-themes
  :ensure t
  ;; :after modus-themes
  :init
  ;; This makes the Modus commands listed below consider only the Ef
  ;; themes.  For an alternative that includes Modus and all
  ;; derivative themes (like Ef), enable the
  ;; `modus-themes-include-derivatives-mode' instead.  The manual of
  ;; the Ef themes has a section that explains all the possibilities:
  ;;
  ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
  ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
  (with-eval-after-load 'modus-themes
    (ef-themes-take-over-modus-themes-mode 1))
  ;; :custom
  ;; Define the two themes you want to toggle between
  ;; (ef-themes-to-toggle '(ef-dark ef-eleutheria-dark))
  :config
  ;;(pathogen/log 'debug "Load theme %s"  +ui/appearance-theme)
  (modus-themes-load-theme 'ef-dark))

;; 2. Cellular Structure (Fonts)
(use-package fontaine
  :ensure t
  :init
  (defun pathogen--fontaine-update-presets ()
    "Update fontaine presets with current variable values and reload."
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

  ;; Variable watcher function with deferred execution
  (defun pathogen--fontaine-variable-watcher (_symbol _newval _operation _where)
    "Automatically update fontaine when font variables change."
    (run-at-time 0 nil #'pathogen--fontaine-update-presets))
  :config
  (pathogen--fontaine-update-presets)
  (fontaine-mode 1)
  ;; Add watchers to automatically refresh when variables change
  (add-variable-watcher 'pathogen-font-family #'pathogen--fontaine-variable-watcher)
  (add-variable-watcher 'pathogen-font-size #'pathogen--fontaine-variable-watcher))

(use-package nerd-icons :ensure t)

;; Add tabs
;; https://github.com/ema2159/centaur-tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :after nerd-icons
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-height 35)
  (centaur-tabs-set-icons t)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  :bind
  (("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)
   ("C-c t t" . centaur-tabs-mode))
  :config
  (centaur-tabs-mode t))

(provide 'look-and-feel)
;;; look-and-feel.el ends here
