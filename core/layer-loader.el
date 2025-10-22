;; ~/.emacs.d/core/layer-loader.el

(require 'cl-lib) ; For common lisp functions

(defvar pathogen--layers-loaded nil
  "List of successfully loaded layers in order.")

(defvar pathogen--layer-dependencies nil
  "Alist of layer dependencies. Format: (LAYER . DEPENDENCIES)")

(defvar pathogen--layers-enabled nil  
  "List of enabled layers to load.")

(defun pathogen-load-layer (layer-path &optional noerror)
  "Load a configuration LAYER-PATH from the layers directory.
If NOERROR is non-nil, don't error if layer file doesn't exist.
Returns t if layer was loaded successfully, nil otherwise."
  (interactive "sLayer path: ")
  (let ((layer-file (expand-file-name (format "%s.el" layer-path) 
                                      pathogen-layers-dir)))
    (cond
     ((file-exists-p layer-file)
      (load layer-file :noerror :nomessage)
      (unless (member layer-path pathogen--layers-loaded)
        (push layer-path pathogen--layers-loaded))
      (message "✓ Loaded layer: %s" layer-path)
      t)
     
     (noerror
      (message "⚠ Layer not found: %s" layer-path)
      nil)
     
     (t
      (error "Layer file not found: %s" layer-file)))))

(defun pathogen-load-layers (layers)
  "Load multiple LAYERS in order."
  (dolist (layer layers)
    (pathogen-load-layer layer)))

(defun pathogen-layer-loaded-p (layer-path)
  "Return t if LAYER-PATH has been loaded."
  (member layer-path pathogen--layers-loaded))

(defun pathogen-list-loaded-layers ()
  "Return list of loaded layers in load order."
  (reverse pathogen--layers-loaded))

(defun pathogen-add-dependency (layer dependencies)
  "Declare that LAYER depends on DEPENDENCIES.
DEPENDENCIES can be a single symbol or list of symbols."
  (push (cons layer (if (listp dependencies) dependencies (list dependencies)))
        pathogen--layer-dependencies))

;; Provide the layer loader
(provide 'layer-loader)
