(use-package ef-themes
  :demand t
  :config
  (if (member pathogen-ui-themes/default-theme (custom-available-themes))
      (ef-themes-load-theme pathogen-ui-themes/default-theme)))

(if pathogen-ui-themes/enable-theme-switching
    (use-package midnight
      :ensure nil  ; It's a built-in package, no need to download.
      :init
      ;; This runs *before* the package is loaded.
      ;; Set the trigger time to 5:30 PM (17*3600s + 30*60s = 63000s).
      (midnight-delay-set 'midnight-delay "5:30pm")

      ;;(setq midnight-seconds-to-wait 63000)
      :config
      ;; This runs *after* the package is loaded.

      ;; 2. Add the function to the midnight hook.
      (add-hook 'midnight-hook 'pathogen-ui-themes/load-dark-theme)

      ;; 3. Enable the mode.
      (midnight-mode 1)))
