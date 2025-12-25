(require 'cl-lib)
;;; EIEIO: Enhanced Implementation of Emacs Interpreted Objects
;;
;; https://www.gnu.org/software/emacs/manual/html_mono/eieio.html
(require 'eieio)

(defgroup pathogen nil
  "Modular Emacs configuration"
  :group 'initialization)

(defcustom pathogen-germs-dirs
  (list (expand-file-name "germs" user-emacs-directory))
  "Directory containing pathogen module files."
  :type '(directory)
  :group 'pathogen)

(defun pathogen-add-germs-dir (dir)
  "Add DIR to the list of germ directories.
Germs in directories added LAST take precedence."
  (add-to-list 'pathogen-germs-dirs (expand-file-name dir) nil))

(defun pathogen--germ-name-to-string (name)
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

(defun pathogen--find-germ-path (layer-name)
  "Internal: Find and return the path to LAYER-NAME's directory."
  (let ((name-str (pathogen--germ-name-to-string layer-name))
        (original-str (if (symbolp layer-name) (symbol-name layer-name) layer-name))
        (found-path nil))
    (dolist (base-dir pathogen-germs-dirs)
      ;; Try the normalized name first (without + prefix)
      (let ((layer-path (expand-file-name (format "%s/" name-str) base-dir)))
        (when (and (not found-path) (file-exists-p layer-path))
          (setq found-path layer-path)))
      ;; If not found and original name has +, try with the + prefix
      (when (and (not found-path) (string-prefix-p "+" original-str))
        (let ((layer-path (expand-file-name (format "%s/" original-str) base-dir)))
          (when (file-exists-p layer-path)
            (setq found-path layer-path)))))
    found-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A "germ" represents a package or module that can be loaded ("infected") by
;; Pathogen.
;;
;; This is an internal class - users should interact with germs through
;; the provided API functions rather than directly accessing slots.
;;
;; Class slots:
;; - name:        Symbol identifier for the germ
;; - dependencies: List of germs this germ depends on
;; - path:        Filesystem path to germ directory
;; - variables:   Property list of germ-specific variables
;; - enabled-p:   Boolean indicating if germ is active
;; - loaded-p:    Internal flag tracking successful loading
(defclass pathogen-germ () ; No superclasses
  ((name :initarg :name
         :type symbol
         :custom symbol
	 :accessor pathogen-germ-name
         :documentation "Germ name.")
   (dependencies :initarg :dependencies
                 :initform nil
                 :type list
                 :accessor pathogen-germ-dependencies
                 :documentation "List of germ names (symbols) this germ depends on.")
   (path :initarg :path
         :initform ""
         :type string
         :custom string
	 :accessor pathogen-germ-path
         :documentation "Path to germ directory.")
   (variables :initarg :variables
	      :initform nil
	      :accessor pathogen-germ-variables
	      :documentation "Plist of germ variables")
   (enabled-p :initarg :enabled-p
              :initform nil
              :type boolean
	      :accessor pathogen-germ-enabled-p
              :documentation "Whether or not the germ is enabled.")
   (loaded-p :initarg :loaded-p
	     :initform nil
             :type boolean
             :accessor pathogen-germ-loaded-p
             :documentation "Internal flag: Has the germ successfully infected the host?"))
  "A class for Pathogen germs. This is an internal structure. Access
fields using provided functions.")

(defun pathogen--germ-load-files (germ)
  "Load all configuration files for GERM in order.
Returns t if all existing files loaded successfully, nil if any load failed."
  (let ((files '("variables.el" "functions.el" "config.el"))
        (all-clear t))
    (dolist (file files)
      (let ((filepath (expand-file-name file (pathogen-germ-path germ))))
        (when (file-exists-p filepath)
          ;; If load returns nil, set our flag to nil
          (unless (load filepath)
            (setq all-clear nil)))))
    all-clear))

(cl-defun pathogen-load-germ (germ-spec &key vars enabled deps)
  "Load a single germ from GERM-SPEC.

GERM-SPEC can be:
  - A symbol: 'python
  - A string: \"python\" or \"lang/python\"
  - A plist: '(:name python :variables (:backend lsp))

Keywords:
  :vars - Plist of variables to set for the layer
  :enabled   - Whether to enable the layer (default t)

Examples:
  (pathogen-load-germ 'python)
  (pathogen-load-germ '+lang/python :vars '(:backend lsp))
  (pathogen-load-germ '(:name python :vars (:backend lsp)))"
  
  (let* ((germ-name (cond
                     ;; Plist form
                     ((and (listp germ-spec) (plist-get germ-spec :name))
                      (plist-get germ-spec :name))
                     ;; Simple symbol or string
                     (t germ-spec)))
         (germ-vars (or vars
                        (and (listp germ-spec) 
                             (plist-get germ-spec :variables))))
         (germ-enabled (if (listp germ-spec)
                           (if (plist-member germ-spec :enabled-p)
                               (plist-get germ-spec :enabled-p)
                             t)
                         (if (null enabled) t enabled)))
	 (germ-dependencies (or deps
				(and (listp germ-spec)
				     (plist-get germ-spec :dependencies))))
         (germ-path (pathogen--find-germ-path germ-name))
         (name-str (pathogen--germ-name-to-string germ-name)))
    (if (and germ-enabled germ-path)
        (let ((germ-instance (make-instance 'pathogen-germ
                                            :name germ-name
                                            :path germ-path
                                            :enabled-p germ-enabled
					    :loaded-p nil
                                            :variables '())))
	  
	  ;; Load dependencies
	  ;; (message "AAAA Deps: %s" germ-dependencies)
	  (when germ-dependencies
	    (dolist (dep germ-dependencies)
	      (pathogen-load-germ dep)))
	  
          ;; Set variables before loading files
          (when germ-vars
            (pathogen--germ-set-variables germ-instance germ-vars))
          
          ;; Load layer files
          (let ((germ-load-status (pathogen--germ-load-files germ-instance)))
	    (if germ-load-status
		(progn
		  ;; Mark as loaded
		  (oset germ-instance :loaded-p germ-load-status)
		  (message "[Pathogen] Loaded germ: %s" germ-name))
	      (message "[Pathogen] Germ not loaded")))         
          germ-instance)
      (when germ-enabled
        (warn "✗ Germ not found: %s" germ-name)
        nil))))

(defun pathogen-load-germs (germs)
  "Load all GERMS in order.
Each element can be a simple name or a plist with configuration.

Example:
  (pathogen-load-layers '(base editing (+lang/python :variables (:backend lsp))))"
  (dolist (germ germs)
    (pathogen-load-germ germ)))

(define-derived-mode pathogen-germ-list-mode tabulated-list-mode "*Microscope: Germ List*"
  "Major mode for browsing pathogen germs."
  (setq tabulated-list-format [("Germ Name" 25 t)
                               ("Status"    12 t)
                               ("Path"      50 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun pathogen-get-all-germs ()
    (remove nil germs))

(defun pathogen-microscope ()
  "Display a list of all pathogen germs."
  (interactive)
  (let ((buffer (get-buffer-create "*Pathogen Germs*")))
    (with-current-buffer buffer
      (pathogen-germ-list-mode)
      ;; Use a proper function/lambda to map the data
      (setq tabulated-list-entries 
            (mapcar (lambda (germ)
                      ;; ID can be the germ name (string) or the germ object
                      (let ((name (format "%s" (pathogen-germ-name germ)))
                            (path (format "%s" (pathogen-germ-path germ)))
                            (status "Loaded")) ; Replace with your logic
                        (list germ (vector name status path))))
                    (pathogen-get-all-germs)))
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

;; List of layers
;; DNA?
;(setq pathogen-list-layers
;      '((:name +package-manager/elpaca :enabled-p t)
;	(:name +package-manager/straight :enabled-p nil)
;	(:name +emacs/org :enabled-p nil)
;	(:name +productivity/gtd :dependencies (+emacs/org))
;	))

;;; Load layers
;;(setq germs (mapcar #'pathogen-load-germ pathogen-list-layers))

;; (defmacro infect! (&rest germs)
;;   `(progn
;;      ,@(cl-loop for germ in germs
;; 		collect
;; 		(cl-destructuring-bind (name &key variables dependencies) germ
;; 		  ;; (let ((variables-names (mapcar #'car variables)))
;; 		  ;;   `(progn
;; 		  ;;      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 		  ;;      ;; 1. Define variables for this germ ;;
;; 		  ;;      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 		  ;;      ,@(mapcar (lambda (variable-definition)
;; 		  ;; 		   (let* ((variable-name (nth 0 variable-definition))
;; 		  ;; 			  (default-value (nth 1 variable-definition))
;; 		  ;; 			  (doc (when (> (length variable-definition) 2)
;; 		  ;; 				 (nth 2 variable-definition))))
;; 		  ;; 		     `(defvar ,variable-name ,default-value ,(or doc (format "Variable for germ %s" name)))))
;; 		  ;; 		 variables)
;; 		  ;;      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 		  ;;      ;; 2. Declare dependencies ;;
;; 		  ;;      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 		  ;;      ,@(when dependencies
;;                   ;;                  `((dolist (dep ',dependencies)
;;                   ;;                      (germ-declare-dependency ,(symbol-name name) dep))))
;; 		  ;;      ))
;; 		  )))
;; ;  `(setq *pathogen-germs* (mapcar #'pathogen-load-germ 
;; ;		       (,@config)))
;;   )

;; (defmacro infect! (&rest layers)
;;   "Define multiple slivers (layers) in a declarative way.
;; Each LAYER can be:
;;   1. Just a symbol: (layer-name)
;;   2. A full definition: (layer-name :key value ...)

;; When only a symbol is provided, default values are used:
;;   - No variables
;;   - No dependencies or conflicts
;;   - Always loads (no conditions)
;;   - No load function (just marks as loaded)
  
;; Examples:
;;   (define-slivers
;;     minimal-layer  ; Just a name, no parameters
;;     (full-layer    ; Full definition
;;      :vars ((some-var 42))
;;      :load (lambda () (message \"Loaded!\"))))"
;;   (declare (indent 0))
;;   `(progn
;;      ;; Initialize registry if needed (petri plate?)
;;      (unless (boundp 'sliver-registry)
;;        (defvar sliver-registry nil
;;          "Registry of all defined slivers."))
     
;;      ,@(cl-loop for layer in layers
;;                 collect 
;;                 (if (symbolp layer)
;;                     ;; Handle simple symbol case
;;                     `(progn
;;                        ;; Register the simple sliver
;;                        (add-to-list 'sliver-registry ',layer)
                       
;;                        ;; Define minimal load function
;;                        (defun ,(intern (format "sliver-%s-load" layer)) (&optional force)
;;                          ,(format "Load sliver '%s' (minimal)." layer)
;;                          (interactive "P")
;;                          (let ((should-load (or force (not (sliver-loaded-p ,(symbol-name layer))))))
;;                            (when should-load
;;                              (put ',layer 'sliver-loaded t)
;;                              (message "Sliver '%s' loaded (minimal)" ,(symbol-name layer)))
;;                            should-load))
                       
;;                        ;; Define minimal unload function
;;                        (defun ,(intern (format "sliver-%s-unload" layer)) ()
;;                          ,(format "Unload sliver '%s' (minimal)." layer)
;;                          (interactive)
;;                          (when (sliver-loaded-p ,(symbol-name layer))
;;                            (put ',layer 'sliver-loaded nil)
;;                            (message "Sliver '%s' unloaded" ,(symbol-name layer))))
                       
;;                        ;; Define toggle function
;;                        (defun ,(intern (format "sliver-%s-toggle" layer)) ()
;;                          ,(format "Toggle sliver '%s' (minimal)." layer)
;;                          (interactive)
;;                          (if (sliver-loaded-p ,(symbol-name layer))
;;                              (,(intern (format "sliver-%s-unload" layer)))
;;                            (,(intern (format "sliver-%s-load" layer)))))
                       
;;                        ;; Return the layer symbol
;;                        ',layer)
                  
;;                   ;; Handle full definition case
;;                   (cl-destructuring-bind (name &rest args) layer
;;                     (let* ((plist (if (keywordp (car args))
;;                                       args
;;                                     (error "Invalid layer definition for %s: %s" name args)))
;;                            (vars (plist-get plist :vars))
;;                            (deps (plist-get plist :deps))
;;                            (conflicts (plist-get plist :conflicts))
;;                            (if-cond (plist-get plist :if))
;;                            (unless-cond (plist-get plist :unless))
;;                            (when-cond (plist-get plist :when))
;;                            (load-fn (plist-get plist :load))
;;                            (unload-fn (plist-get plist :unload))
;;                            (before-load (plist-get plist :before-load))
;;                            (after-load (plist-get plist :after-load))
;;                            (var-names (mapcar #'car (or vars '()))))
                      
;;                       `(progn
;;                          ;; Define variables if provided
;;                          ,@(when vars
;;                              (mapcar (lambda (var-def)
;;                                        (let* ((var-name (nth 0 var-def))
;;                                               (default (nth 1 var-def))
;;                                               (doc (when (> (length var-def) 2)
;;                                                      (nth 2 var-def))))
;;                                          `(defvar ,var-name ,default
;;                                             ,(or doc (format "Variable for sliver '%s'" name)))))
;;                                      vars))
                         
;;                          ;; Declare dependencies if provided
;;                          ,@(when deps
;;                              `((dolist (dep ',deps)
;;                                  (sliver-declare-dependency ,(symbol-name name) dep))))
                         
;;                          ;; Declare conflicts if provided
;;                          ,@(when conflicts
;;                              `((dolist (conflict ',conflicts)
;;                                  (sliver-declare-conflict ,(symbol-name name) conflict))))
                         
;;                          ;; Store metadata
;;                          (put ',name 'sliver-metadata
;;                               (list :vars ',var-names
;;                                     :deps ',deps
;;                                     :conflicts ',conflicts
;;                                     :has-load ,(not (null load-fn))))
                         
;;                          ;; Define load function
;;                          (defun ,(intern (format "sliver-%s-load" name)) (&optional force)
;;                            ,(format "Load sliver '%s'." name)
;;                            (interactive "P")
;;                            (let* ((conditions (append 
;;                                                ,(when if-cond `((,if-cond)))
;;                                                ,(when unless-cond `((not ,unless-cond)))
;;                                                ,(when when-cond `(,when-cond))))
;;                                   (should-load (and (or force (eval `(and ,@conditions)))
;;                                                     (not (sliver-loaded-p ,(symbol-name name))))))
;;                              (when should-load
;;                                (message "Loading sliver '%s'..." ,(symbol-name name))
                               
;;                                ;; Run before-load hook if provided
;;                                ,@(when before-load `((funcall ,before-load)))
                               
;;                                ;; Set variables to current values (allows pre-configuration)
;;                                ,@(when vars
;;                                    (mapcar (lambda (var-name)
;;                                              `(setq ,var-name ,var-name))
;;                                            var-names))
                               
;;                                ;; Execute load function if provided
;;                                ,@(when load-fn `((funcall ,load-fn)))
                               
;;                                ;; Run after-load hook if provided
;;                                ,@(when after-load `((funcall ,after-load)))
                               
;;                                (put ',name 'sliver-loaded t)
;;                                (message "Sliver '%s' loaded" ,(symbol-name name))))
;;                              should-load))
                         
;;                          ;; Define unload function (if provided, else minimal)
;;                          ,(if unload-fn
;;                               `(defun ,(intern (format "sliver-%s-unload" name)) ()
;;                                  ,(format "Unload sliver '%s'." name)
;;                                  (interactive)
;;                                  (when (sliver-loaded-p ,(symbol-name name))
;;                                    (funcall ,unload-fn)
;;                                    (put ',name 'sliver-loaded nil)
;;                                    (message "Sliver '%s' unloaded" ,(symbol-name name))))
;;                             `(defun ,(intern (format "sliver-%s-unload" name)) ()
;;                                ,(format "Unload sliver '%s' (minimal)." name)
;;                                (interactive)
;;                                (when (sliver-loaded-p ,(symbol-name name))
;;                                  (put ',name 'sliver-loaded nil)
;;                                  (message "Sliver '%s' unloaded" ,(symbol-name name)))))
                         
;;                          ;; Define toggle function
;;                          (defun ,(intern (format "sliver-%s-toggle" name)) ()
;;                            ,(format "Toggle sliver '%s'." name)
;;                            (interactive)
;;                            (if (sliver-loaded-p ,(symbol-name name))
;;                                (,(intern (format "sliver-%s-unload" name)))
;;                              (,(intern (format "sliver-%s-load" name)))))
                         
;;                          ;; Register in global registry
;;                          (add-to-list 'sliver-registry ',name)
                         
;;                          ;; Return the layer symbol
;;                          ',name))))))

;;(macroexpand
;; '(infect! +package-manager/elpaca))


;;(+productivity/gtd :variables (:inbox "~/2.areas/orginbox.org")))
