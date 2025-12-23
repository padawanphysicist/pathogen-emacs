(defun pathogen-virtualization-docker/dockerfile-mode-setup ()
  "Custom setup for dockerfile-mode."
  ;; Use plain progress to avoid ANSI colors in non-interactive buffers
  (setq dockerfile-mode-command "docker build --progress=plain"))
