;; 1. The Modeline (The Primary Organ)
(when (eq +ui/appearance-modeline-style 'doom) 
  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)))

(when +ui/appearance-thin-borders
  (set-face-background 'vertical-border (face-background 'default))
  (set-face-foreground 'vertical-border "grey30"))

;; Pruning Vestigial Structures
(menu-bar-mode (if +ui/appearance-show-menubar 1 -1))
(tool-bar-mode (if +ui/appearance-show-toolbar 1 -1))
(scroll-bar-mode (if +ui/appearance-show-scrollbars 1 -1))

;; 1. Pigmentation (Themes)
(use-package ef-themes
  :ensure t
  :after modus-themes
  :init
  ;; This makes the Modus commands listed below consider only the Ef
  ;; themes.  For an alternative that includes Modus and all
  ;; derivative themes (like Ef), enable the
  ;; `modus-themes-include-derivatives-mode' instead.  The manual of
  ;; the Ef themes has a section that explains all the possibilities:
  ;;
  ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
  ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
  (ef-themes-take-over-modus-themes-mode 1)
  ;; :custom
  ;; Define the two themes you want to toggle between
  ;; (ef-themes-to-toggle '(ef-dark ef-eleutheria-dark))
  :config
  (pathogen/log 'debug "Load theme %s"  +ui/appearance-theme)
  (modus-themes-load-theme +ui/appearance-theme))

;; 2. Cellular Structure (Fonts)
(use-package fontaine
  :ensure t
  :config
  (fontaine-set-preset 'regular)
  (fontaine-mode 1))

(use-package nerd-icons :ensure t)

;; Add tabs
;; https://github.com/ema2159/centaur-tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :after nerd-icons
  :bind
  (("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)
   ("C-c t t" . centaur-tabs-mode))
  :config
  (when +ui/appearance-enable-tabs
    (centaur-tabs-mode t)))
