

(use-package dockerfile-mode
  :ensure t
  :custom
  ((dockerfile-build-progress pathogen-virtualization-docker/dockerfile-build-progress))
  ;;:config
  ;;(add-hook 'dockerfile-mode-hook #'pathogen-virtualization-docker/dockerfile-mode-setup)
  )
