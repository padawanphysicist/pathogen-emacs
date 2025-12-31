;;; germs/+ui/appearance/enzymes.el --- UI Catalysts

(defun +organoid/toggle-transparency ()
  "Toggle the host's opacity (alpha-background)."
  (interactive)
  (let ((current (frame-parameter nil 'alpha-background)))
    (set-frame-parameter nil 'alpha-background (if (eq current 100) 85 100))))

(defun +organoid/toggle-minimal-ui ()
  "Toggle all vestigial UI elements at once."
  (interactive)
  (menu-bar-mode (if menu-bar-mode -1 1))
  (tool-bar-mode (if tool-bar-mode -1 1))
  (scroll-bar-mode (if scroll-bar-mode -1 1))
  (message "UI Morphology: %s" (if menu-bar-mode "Full" "Minimal")))

(defun +organoid/cycle-font-presets ()
  "Cycle through defined Fontaine presets."
  (interactive)
  (when (fboundp 'fontaine-set-preset)
    (call-interactively #'fontaine-set-preset)))

(defun +organoid/toggle-theme ()
  "Rapidly toggle between the two preferred Ef-themes."
  (interactive)
  (if (fboundp 'ef-themes-toggle)
      (ef-themes-toggle)
    (message "⚠️ ef-themes package not yet expressed.")))

(message "🧪 [Enzyme] +ui/appearance UI helpers synthesized.")
