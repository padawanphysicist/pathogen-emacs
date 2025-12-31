;;; UI Traits

(defvar +ui/appearance-modeline-style 'doom
  "The visual style of the modeline. Options: 'doom, 'minimal, 'standard.")

(defvar +ui/appearance-thin-borders nil
  "Toggle for microscopic window dividers.")

(defvar +ui/appearance-show-menubar nil
  "Toggle for the top menu bar (Vestigial).")

(defvar +ui/appearance-show-toolbar nil
  "Toggle for the icon tool bar (Vestigial).")

(defvar +ui/appearance-show-scrollbars nil
  "Toggle for the vertical scroll bars.")

(defvar +ui/appearance-enable-tabs nil
  "Enable tabs at startup.")

(defvar +ui/appearance-font-family "GeistMono Nerd Font"
  "The primary typeface for the host.")

(defvar +ui/appearance-font-size 110
  "The base font size (1/10th of a pt).")

(pathogen--defvaralias!
 +ui/appearance-theme +distributions/base-default-theme
 'ef-day
  "The Ef-theme to apply (e.g., 'ef-bio, 'ef-night, 'ef-eleutheria-dark).")

(pathogen--defvars-with-aliases!
 +ui/appearance
 
 (use-file-dialog nil)
 (use-dialog-box-p nil)
 
 (doom-modeline-height 25)
 (doom-modeline-bar-width 3)

 (fontaine-presets
  `((default
     :default-family ,+ui/appearance-font-family
     :default-height ,+ui/appearance-font-size)
    (presentation :default-height 180 :default-weight bold)
    (small :default-height 90)))
 
 (centaur-tabs-style "bar")
 (centaur-tabs-height 35)
 (centaur-tabs-set-icons t)
 (centaur-tabs-icon-type 'nerd-icons)
 (centaur-tabs-set-bar 'over)
 (centaur-tabs-set-modified-marker t))
