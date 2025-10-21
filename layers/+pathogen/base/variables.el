;; Define hook run after font resize
(defvar pathogen-after-text-scale-hook nil
  "Hook run after text is rescaled.
This hook is triggered after `text-scale-increase', `text-scale-decrease',
or `text-scale-set' commands are executed.

Example usage:
  (add-hook 'after-text-scale-hook
            (lambda ()
              (message \"Text scaled to: %s\" text-scale-mode-amount)))")

;; Define hook run after theme loading
(defvar pathogen-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

