(require 'cl-lib)

;; ============================================
;; Layer structure (internal)
;; ============================================

(cl-defstruct pathogen-layer
  "Structure representing a layer configuration.
This is an internal structure. Access fields using provided functions."
  name           ; Layer name (symbol or string)
  path           ; Path to layer directory
  enabled        ; Whether layer is enabled
  variables      ; Plist of layer variables
  packages       ; List of packages to load
  files-loaded)  ; List of files that were loaded

;; ============================================
;; Public API - User-facing functions
;; ============================================

(defun pathogen-add-layers-dir (dir)
  "Add DIR to the list of layer directories.
Layers in directories added first take precedence."
  (add-to-list 'pathogen-layers-dirs (expand-file-name dir) t))

(defun pathogen-layer-enabled-p (layer-name)
  "Return t if LAYER-NAME is enabled and loaded."
  (member (pathogen--layer-name-to-string layer-name) pathogen--enabled-layers))

(defun pathogen-layer-get (layer-name)
  "Get layer configuration struct for LAYER-NAME.
Returns nil if layer is not loaded."
  (gethash (pathogen--layer-name-to-string layer-name) pathogen--layers-table))

(cl-defun pathogen-load-layer (layer-spec &key variables enabled)
  "Load a layer from LAYER-SPEC.

LAYER-SPEC can be:
  - A symbol: 'python
  - A string: \"python\" or \"lang/python\"
  - A plist: '(:name python :variables (:backend lsp))

Keywords:
  :variables - Plist of variables to set for the layer
  :enabled   - Whether to enable the layer (default t)

Examples:
  (pathogen-load-layer 'python)
  (pathogen-load-layer '+lang/python :variables '(:backend lsp))
  (pathogen-load-layer '(:name python :variables (:backend lsp)))"
  
  (let* ((layer-name (cond
                      ;; Plist form
                      ((and (listp layer-spec) (plist-get layer-spec :name))
                       (plist-get layer-spec :name))
                      ;; Simple symbol or string
                      (t layer-spec)))
         (layer-vars (or variables
                        (and (listp layer-spec) 
                             (plist-get layer-spec :variables))))
         (layer-enabled (if (listp layer-spec)
                           (plist-get layer-spec :enabled t)
                         (if (null enabled) t enabled)))
         (layer-path (pathogen--find-layer-path layer-name))
         (name-str (pathogen--layer-name-to-string layer-name)))
    
    (if (and layer-enabled layer-path)
        (let ((layer-struct (make-pathogen-layer
                            :name name-str
                            :path layer-path
                            :enabled t
                            :variables '()
                            :files-loaded '())))
          
          ;; Store layer
          (pathogen--layer-put name-str layer-struct)
          
          ;; Set variables before loading files
          (when layer-vars
            (pathogen--layer-set-variables layer-struct layer-vars))
          
          ;; Run pre-init hook
          (run-hook-with-args 'pathogen-layer-pre-init-hook layer-struct)
          
          ;; Load layer files
          (pathogen--layer-load-files layer-struct)
          
          ;; Mark as loaded
          (add-to-list 'pathogen--enabled-layers name-str)
          
          ;; Run post-init hook
          (run-hook-with-args 'pathogen-layer-post-init-hook layer-struct)
          
          (message "✓ Loaded layer: %s" name-str)
          layer-struct)
      
      (when layer-enabled
        (warn "✗ Layer not found: %s" layer-name)
        nil))))

(defun pathogen-load-layers (layers)
  "Load all LAYERS in order.
Each element can be a simple name or a plist with configuration.

Example:
  (pathogen-load-layers '(base editing (+lang/python :variables (:backend lsp))))"
  (dolist (layer layers)
    (if (listp layer)
        ;; Plist form: (:name python :variables (:backend lsp))
        (pathogen-load-layer layer)
      ;; Simple form: just the name
      (pathogen-load-layer layer))))

(defmacro pathogen-layers! (&rest layers)
  "Define layers to load with a clean syntax.

Examples:
  (pathogen-layers!
   base
   editing
   (+lang/python :variables (:backend lsp :formatter black))
   (+tools/git :variables (:client magit :gutter t))
   themes)"
  `(setq pathogen-configuration-layers
         (list ,@(mapcar (lambda (layer)
                          (if (listp layer)
                              ;; Convert (python :key val) to '(:name python :key val)
                              `(list :name ',(car layer) ,@(cdr layer))
                            ;; Simple symbol
                            `',layer))
                        layers))))

;; ============================================
;; Private/Internal functions (pathogen--)
;; ============================================

(defun pathogen--layer-name-to-string (name)
  "Internal: Convert layer NAME to string, handling + prefix.
Examples: 
  '+ui/fonts   -> 'ui/fonts'
  'ui/fonts    -> 'ui/fonts'
  '+python     -> 'python'"
  (let ((name-str (if (symbolp name)
                      (symbol-name name)
                    name)))
    ;; Remove leading + if present
    (if (string-prefix-p "+" name-str)
        (substring name-str 1)
      name-str)))

(defun pathogen--find-layer-path (layer-name)
  "Internal: Find and return the path to LAYER-NAME's directory."
  (let ((name-str (pathogen--layer-name-to-string layer-name))
        (found-path nil))
    (dolist (base-dir pathogen-layers-dirs)
      (let ((layer-path (expand-file-name (format "%s/" name-str) base-dir)))
        (when (and (not found-path) (file-exists-p layer-path))
          (setq found-path layer-path))))
    found-path))

(defun pathogen--layer-put (layer-name layer-struct)
  "Internal: Store LAYER-STRUCT for LAYER-NAME."
  (puthash (pathogen--layer-name-to-string layer-name) layer-struct pathogen--layers-table))

(defun pathogen--layer-load-file (layer-struct file)
  "Internal: Load FILE for LAYER-STRUCT if it exists."
  (let ((filepath (expand-file-name file (pathogen-layer-path layer-struct))))
    (when (file-exists-p filepath)
      (load filepath)
      ;; Add to files-loaded list
      (setf (pathogen-layer-files-loaded layer-struct)
            (cons file (pathogen-layer-files-loaded layer-struct)))
      t)))

(defun pathogen--layer-load-files (layer-struct)
  "Internal: Load all configuration files for LAYER-STRUCT in order."
  (let ((files '("variables.el" "packages.el" "funcs.el" 
                 "config.el" "layers.el" "keybindings.el")))
    (dolist (file files)
      (pathogen--layer-load-file layer-struct file))))

(defun pathogen--layer-set-variables (layer-struct variables)
  "Internal: Set VARIABLES for LAYER-STRUCT from plist."
  (let ((vars variables)
        (layer-name (pathogen-layer-name layer-struct)))
    (while vars
      (let* ((key (pop vars))
             (value (pop vars))
             ;; Remove + prefix from layer name for variable names
             (clean-name (if (string-prefix-p "+" layer-name)
                            (substring layer-name 1)
                          layer-name))
             ;; Replace / with - for variable names
             (var-base (replace-regexp-in-string "/" "-" clean-name))
             (var-name (intern (format "pathogen-%s/%s" 
                                      var-base
                                      (substring (symbol-name key) 1)))))
        ;; Set the actual variable
        (set var-name value)
        ;; Store in layer struct
        (setf (pathogen-layer-variables layer-struct)
              (plist-put (pathogen-layer-variables layer-struct) key value))))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API 
;;
;;
;; These functions and variables are designed to be used in .pathogen.el and
;; user configuration.

;;(defun pathogen-add-layers-dir (dir)
;;  "Add DIR to the list of layer directories."
;;  (add-to-list 'pathogen--layers-dirs (expand-file-name dir) t))
;;
;;(cl-defun pathogen-load-layer (layer-spec &key variables enabled)
;;  "Load a layer from LAYER-SPEC.
;;
;;LAYER-SPEC can be:
;;  - A symbol: 'python
;;  - A string: \"python\" or \"languages/python\"
;;  - A plist: '(:name python :variables (:backend lsp))
;;
;;Keywords:
;;  :variables - Plist of variables to set for the layer
;;  :enabled   - Whether to enable the layer (default t)
;;
;;Examples:
;;  (my-load-layer 'python)
;;  (my-load-layer 'python :variables '(:backend lsp :formatter black))
;;  (my-load-layer '(:name python :variables (:backend lsp) :enabled t))"
;;  
;;  (let* ((layer-name (cond
;;                      ;; Plist form
;;                      ((and (listp layer-spec) (plist-get layer-spec :name))
;;                       (plist-get layer-spec :name))
;;                      ;; Simple symbol or string
;;                      (t layer-spec)))
;;         (layer-vars (or variables
;;                        (and (listp layer-spec) 
;;                             (plist-get layer-spec :variables))))
;;         (layer-enabled (if (listp layer-spec)
;;                           (plist-get layer-spec :enabled t)
;;                         (if (null enabled) t enabled)))
;;         (layer-path (pathogen--find-layer-path layer-name))
;;         (name-str (pathogen--layer-name-to-string layer-name)))
;;    
;;    (if (and layer-enabled layer-path)
;;        (let ((layer-struct (make-pathogen-layer
;;                            :name name-str
;;                            :path layer-path
;;                            :enabled t
;;                            :variables '()
;;                            :files-loaded '())))
;;          
;;          ;; Store layer
;;          (pathogen--layer-put name-str layer-struct)
;;          
;;          ;; Set variables before loading files
;;          (when layer-vars
;;            (pathogen--layer-set-variables layer-struct layer-vars))
;;          
;;          ;; Run pre-init hook
;;          (run-hook-with-args 'pathogen--layer-pre-init-hook layer-struct)
;;          
;;          ;; Load layer files
;;          (pathogen--layer-load-files layer-struct)
;;          
;;          ;; Mark as loaded
;;          (add-to-list 'pathogen--enabled-layers name-str)
;;          
;;          ;; Run post-init hook
;;          (run-hook-with-args 'pathogen--layer-post-init-hook layer-struct)
;;          
;;          (message "✓ Loaded layer: %s" name-str)
;;          layer-struct)
;;      
;;      (when layer-enabled
;;        (warn "✗ Layer not found: %s" layer-name)
;;        nil))))
;;
;;(defun pathogen-load-layers (layers)
;;  "Load all LAYERS.
;;Each element can be a simple name or a plist with configuration."
;;  (dolist (layer layers)
;;    (if (listp layer)
;;        ;; Plist form: (:name python :variables (:backend lsp))
;;        (pathogen--load-layer layer)
;;      ;; Simple form: just the name
;;      (pathogen--load-layer layer))))

;; pathogen-layer-enabled-p (layer-name)
;; Check if a layer is loaded and enabled;; .

;pathogen-layer-get (layer-name)
;Get the layer struct for a loaded layer (returns nil if not loaded).


;pathogen-layers! (&rest layers)
;Clean syntax for defining layers to load (recommended).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private API 
;;
;; These functions and variables are for internal use only. They may change
;; without notice.


(cl-defstruct pathogen-layer
  "Structure representing a layer configuration.
This is an internal structure. Access fields using provided functions."
  name           ; Layer name (symbol or string)
  path           ; Path to layer directory
  enabled        ; Whether layer is enabled
  variables      ; Plist of layer variables
  packages       ; List of packages to load
  files-loaded)  ; List of files that were loaded

;pathogen--layer-name-to-string (name)
;Internal: Convert layer name to string, stripping + prefix.

;pathogen--find-layer-path (layer-name)
;Internal: Find path to layer directory.

;pathogen--layer-put (layer-name layer-struct)
;Internal: Store layer struct in hash table.

;pathogen--layer-load-file (layer-struct file)
;Internal: Load a single file from layer.
;
;pathogen--layer-load-files (layer-struct)
;Internal: Load all files from layer in order.
;
;pathogen--layer-set-variables (layer-struct variables)
;Internal: Set layer variables from plist.


;;(defun pathogen--layer-name-to-string (name)
;;  "Convert layer NAME to string."
;;  (if (symbolp name)
;;      (symbol-name name)
;;    name))
;;
;;(defvar pathogen--layers-table (make-hash-table :test 'equal)
;;  "Hash table storing layer configurations.")
;;
;;
;;(defun pathogen--find-layer-path (layer-name)
;;  "Find and return the path to LAYER-NAME's directory."
;;  (let ((name-str (my-layer-name-to-string layer-name))
;;        (found-path nil))
;;    (dolist (base-dir my-layers-dirs)
;;      (let ((layer-path (expand-file-name (format "%s/" name-str) base-dir)))
;;        (when (and (not found-path) (file-exists-p layer-path))
;;          (setq found-path layer-path))))
;;    found-path))
;;
;;(defun pathogen--layer-get (layer-name)
;;  "Get layer configuration for LAYER-NAME."
;;  (gethash (pathogen--layer-name-to-string layer-name) pathogen--layers-table))
;;
;;(defun pathogen--layer-put (layer-name layer-struct)
;;  "Store LAYER-STRUCT for LAYER-NAME."
;;  (puthash (pathogen--layer-name-to-string layer-name) layer-struct pathogen--layers-table))
;;
;;(defun pathogen--layer-load-file (layer-struct file)
;;  "Load FILE for LAYER-STRUCT if it exists."
;;  (let ((filepath (expand-file-name file (pathogen-layer-path layer-struct))))
;;    (when (file-exists-p filepath)
;;      (load filepath)
;;      (push file (pathogen-layer-files-loaded layer-struct))
;;      t)))
;;
;;(defun pathogen--layer-load-files (layer-struct)
;;  "Load all configuration files for LAYER-STRUCT in order."
;;  (let ((files '("variables.el" "packages.el" "funcs.el" 
;;                 "config.el" "layers.el" "keybindings.el")))
;;    (dolist (file files)
;;      (pathogen--layer-load-file layer-struct file))))
;;
;;(defun pathogen--layer-set-variables (layer-struct variables)
;;  "Set VARIABLES for LAYER-STRUCT from plist."
;;  (let ((vars variables))
;;    (while vars
;;      (let* ((key (pop vars))
;;             (value (pop vars))
;;             (var-name (intern (format "my-%s/%s" 
;;                                      (pathogen--layer-name layer-struct)
;;                                      (substring (symbol-name key) 1)))))
;;        ;; Set the actual variable
;;        (set var-name value)
;;        ;; Store in layer struct
;;        (setf (pathogen--layer-variables layer-struct)
;;              (plist-put (pathogen--layer-variables layer-struct) key value))))))

;(defun pathogen--add-layers-dir (dir)
;  "Add DIR to the list of layer directories."
;  (add-to-list 'pathogen--layers-dirs (expand-file-name dir) t))
;
;(defun pathogen--find-layer-path (layer-name)
;  "Find and return the path to LAYER-NAME's directory.
;Searches through all directories in `my-layers-dirs'."
;  (let ((found-path nil))
;    (dolist (base-dir pathogen--layers-dirs)
;      (let ((layer-path (expand-file-name (format "%s/" layer-name) base-dir)))
;        (when (and (not found-path) (file-exists-p layer-path))
;          (setq found-path layer-path))))
;    found-path))
;
;(defun pathogen--load-layer-files (layer-path)
;  "Load all configuration files from LAYER-PATH in order."
;  (let ((files '("packages.el" "funcs.el" "config.el" 
;                 "layers.el" "keybindings.el")))
;    (dolist (file files)
;      (let ((filepath (expand-file-name file layer-path)))
;        (when (file-exists-p filepath)
;          (load filepath))))))

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

;(defun pathogen/load-layer (layer-name)
;  "Load a layer by LAYER-NAME."
;  (let ((layer-path (pathogen--find-layer-path layer-name)))
;    (if layer-path
;        (progn
;          (pathogen--load-layer-files layer-path)
;          (add-to-list 'pathogen--enabled-layers layer-name)
;          (message "✓ Loaded layer: %s" layer-name))
;      (warn "✗ Layer not found: %s" layer-name))))


(defun pathogen--layer-set-variables (layer-struct variables)
  "Set VARIABLES for LAYER-STRUCT from plist."
  (let ((vars variables))
    (while vars
      (let* ((key (pop vars))
             (value (pop vars))
             (var-name (intern (format "my-%s/%s" 
                                      (my-layer-name layer-struct)
                                      (substring (symbol-name key) 1)))))
        ;; Set the actual variable
        (set var-name value)
        ;; Store in layer struct
        (setf (my-layer-variables layer-struct)
              (plist-put (my-layer-variables layer-struct) key value))))))


;(defun pathogen/load-layers (layers)
;  "Load all LAYERS in order."
;  (dolist (layer layers)
;    (pathogen/load-layer layer)))
(provide 'layers)


