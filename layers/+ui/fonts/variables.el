(defvar pathogen-ui-fonts/use-fontaine t
  "Use fontaine package for font management.
If nil, falls back to simple font configuration.")

(defvar pathogen-ui-fonts/default-preset 'regular
  "Default fontaine preset to use on startup.
Should be a key from `pathogen-ui-fonts/presets'.")

(defvar pathogen-ui-fonts/presets
  '((tiny
     :default-family "JetBrains Mono"
     :default-height 100
     :variable-pitch-family "ETBembo"
     :variable-pitch-height 1.0)
    (small
     :default-family "JetBrains Mono"
     :default-height 110
     :variable-pitch-family "ETBembo"
     :variable-pitch-height 1.05)
    (regular
     :default-family "JetBrains Mono"
     :default-height 130
     :variable-pitch-family "ETBembo"
     :variable-pitch-height 1.1
     :bold-weight semibold)
    (medium
     :default-family "JetBrains Mono"
     :default-height 150
     :variable-pitch-family "ETBembo"
     :variable-pitch-height 1.15
     :bold-weight semibold)
    (large
     :default-family "JetBrains Mono"
     :default-height 170
     :variable-pitch-family "ETBembo"
     :variable-pitch-height 1.2
     :bold-weight bold)
    (presentation
     :default-family "JetBrains Mono"
     :default-height 220
     :variable-pitch-family "ETBembo"
     :variable-pitch-height 1.3
     :bold-weight bold
     :line-spacing 0.2))
  "Font presets for fontaine.
Each preset is a plist with font configuration options.")

(defvar pathogen-ui-fonts/enable-ligatures t
  "Enable programming ligatures if font supports them.")

(defvar pathogen-ui-fonts/line-spacing 0.1
  "Default line spacing as proportion of line height.")

(defvar pathogen-ui-fonts/save-preset t
  "Save the current fontaine preset to file.")

(defvar pathogen-ui-fonts/preset-save-file
  (expand-file-name "fontaine-preset" user-emacs-directory)
  "File to save current fontaine preset.")

;; Fallback font configuration (when fontaine is disabled)
(defvar pathogen-ui-fonts/default-family "Monospace"
  "Fallback: Default monospace font family.")

(defvar pathogen-ui-fonts/default-size 120
  "Fallback: Default font size in 1/10 pt.")

(defvar pathogen-ui-fonts/variable-pitch-family "Sans Serif"
  "Fallback: Variable pitch font family.")
