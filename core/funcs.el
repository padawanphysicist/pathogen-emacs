;(require 'layers)
;;(defun pathogen--add-layers-dir (dir)
;;  "Add DIR to the list of layer directories."
;;  (add-to-list 'pathogen--layers-dirs (expand-file-name dir) t))
;;
;;(defun pathogen--find-layer-path (layer-name)
;;  "Find and return the path to LAYER-NAME's directory.
;;Searches through all directories in `my-layers-dirs'."
;;  (let ((found-path nil))
;;    (dolist (base-dir pathogen--layers-dirs)
;;      (let ((layer-path (expand-file-name (format "%s/" layer-name) base-dir)))
;;        (when (and (not found-path) (file-exists-p layer-path))
;;          (setq found-path layer-path))))
;;    found-path))
;;
;;(defun pathogen--load-layer-files (layer-path)
;;  "Load all configuration files from LAYER-PATH in order."
;;  (let ((files '("packages.el" "funcs.el" "config.el" 
;;                 "layers.el" "keybindings.el")))
;;    (dolist (file files)
;;      (let ((filepath (expand-file-name file layer-path)))
;;        (when (file-exists-p filepath)
;;          (load filepath))))))

;;(defun pathogen--layer-init-file (layer-path)
;;  "Return the path to the init file in LAYER-PATH."
;;  (expand-file-name "layers.el" layer-path))
;;
;;(defun pathogen--layer-packages-file (layer-path)
;;  "Return the path to the packages file in LAYER-PATH."
;;  (expand-file-name "packages.el" layer-path))
;;
;;(defun pathogen/load-layer (layer-name)
;;  "Load a layer by LAYER-NAME."
;;  (let ((layer-path (pathogen--find-layer-path layer-name)))
;;    (if layer-path
;;        (let ((packages-file (pathogen--layer-packages-file layer-path))
;;              (init-file (pathogen--layer-init-file layer-path)))
;;          ;; Load packages first
;;          (when (file-exists-p packages-file)
;;            (load packages-file))
;;          ;; Then load layer configuration
;;          (when (file-exists-p init-file)
;;            (load init-file))
;;          (add-to-list 'pathogen--enabled-layers layer-name)
;;          (message "Loaded layer: %s from %s" layer-name layer-path))
;;      (warn "Layer not found: %s" layer-name))))

;;(defun pathogen/load-layer (layer-name)
;;  "Load a layer by LAYER-NAME."
;;  (let ((layer-path (pathogen--find-layer-path layer-name)))
;;    (if layer-path
;;        (progn
;;          (pathogen--load-layer-files layer-path)
;;          (add-to-list 'pathogen--enabled-layers layer-name)
;;          (message "✓ Loaded layer: %s" layer-name))
;;      (warn "✗ Layer not found: %s" layer-name))))
;;
;;(defun pathogen/load-layers (layers)
;;  "Load all LAYERS in order."
;;  (dolist (layer layers)
;;    (pathogen/load-layer layer)))

(defun pathogen--layer-name-to-string (name)
  "Convert layer NAME to string."
  (if (symbolp name)
      (symbol-name name)
    name))

(defun pathogen--add-layers-dir (dir)
  "Add DIR to the list of layer directories."
  (add-to-list 'pathogen--layers-dirs (expand-file-name dir) t))

(defun pathogen--find-layer-path (layer-name)
  "Find and return the path to LAYER-NAME's directory.
Searches through all directories in `my-layers-dirs'."
  (let ((found-path nil))
    (dolist (base-dir pathogen--layers-dirs)
      (let ((layer-path (expand-file-name (format "%s/" layer-name) base-dir)))
        (when (and (not found-path) (file-exists-p layer-path))
          (setq found-path layer-path))))
    found-path))

(defun pathogen--load-layer-files (layer-path)
  "Load all configuration files from LAYER-PATH in order."
  (let ((files '("packages.el" "funcs.el" "config.el" 
                 "layers.el" "keybindings.el")))
    (dolist (file files)
      (let ((filepath (expand-file-name file layer-path)))
        (when (file-exists-p filepath)
          (load filepath))))))

;;(defun pathogen--layer-init-file (layer-path)
;;  "Return the path to the init file in LAYER-PATH."
;;  (expand-file-name "layers.el" layer-path))
;;
;;(defun pathogen--layer-packages-file (layer-path)
;;  "Return the path to the packages file in LAYER-PATH."
;;  (expand-file-name "packages.el" layer-path))
;;
;;(defun pathogen/load-layer (layer-name)
;;  "Load a layer by LAYER-NAME."
;;  (let ((layer-path (pathogen--find-layer-path layer-name)))
;;    (if layer-path
;;        (let ((packages-file (pathogen--layer-packages-file layer-path))
;;              (init-file (pathogen--layer-init-file layer-path)))
;;          ;; Load packages first
;;          (when (file-exists-p packages-file)
;;            (load packages-file))
;;          ;; Then load layer configuration
;;          (when (file-exists-p init-file)
;;            (load init-file))
;;          (add-to-list 'pathogen--enabled-layers layer-name)
;;          (message "Loaded layer: %s from %s" layer-name layer-path))
;;      (warn "Layer not found: %s" layer-name))))

;;(defun pathogen/load-layer (layer-name)
;;  "Load a layer by LAYER-NAME."
;;  (let ((layer-path (pathogen--find-layer-path layer-name)))
;;    (if layer-path
;;        (progn
;;          (pathogen--load-layer-files layer-path)
;;          (add-to-list 'pathogen--enabled-layers layer-name)
;;          (message "✓ Loaded layer: %s" layer-name))
;;      (warn "✗ Layer not found: %s" layer-name))))
;;
;;(defun pathogen/load-layers (layers)
;;  "Load all LAYERS in order."
;;  (dolist (layer layers)
;;    (pathogen/load-layer layer)))

;; Convenience macro for cleaner layer configuration
;(defmacro pathogen/layer! (&rest layers)
;  "Define layers to load with a clean syntax.
;
;Examples:
;  (pathogen/layer!
;   base
;   editing
;   (python :variables (:backend lsp :formatter black))
;   (git :variables (:client magit :gutter t))
;   themes)"
;  `(setq pathogen-configuration-layers
;         (list ,@(mapcar (lambda (layer)
;                          (if (listp layer)
;                              ;; Convert (python :key val) to '(:name python :key val)
;                              `(list :name ',(car layer) ,@(cdr layer))
;                            ;; Simple symbol
;                            `',layer))
;                        layers))))
;(provide 'layers)



(provide 'funcs)
