;; Fontaine for font preset management
(when pathogen-ui-fonts/use-fontaine
  (use-package fontaine
    :custom
    (
     (fontaine-latest-state-file pathogen-ui-fonts/preset-save-file)
     ;; Set fontaine presets from our custom variable
     (fontaine-presets pathogen-ui-fonts/presets)
     )
    :config

    ;; Apply preset on startup
    (fontaine-set-preset pathogen-ui-fonts/default-preset)

    ;; Restore last preset if available and saving is enabled
    (when (and pathogen-ui-fonts/save-preset
               (file-exists-p pathogen-ui-fonts/preset-save-file))
      (fontaine-restore-latest-preset))

    ;; Enable fontaine mode
    (fontaine-mode 1)

    ;; Store preset on exit
    (when pathogen-ui-fonts/save-preset
      (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))
    :bind
    ("C-c f" . #'fontaine-set-preset)))
