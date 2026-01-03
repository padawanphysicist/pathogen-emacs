;; 1. The Modeline (The Primary Organ)
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
  :custom
  (fontaine-presets
   `((regular
      :default-family "GeistMono Nerd Font"
      :default-height 110
     (presentation :default-height 180 :default-weight bold)
     (small :default-height 90))))
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
