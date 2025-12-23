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

(defclass pathogen-germ () ; No superclasses
  (
   (name :initarg :name
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
             :documentation "Internal flag: Has the germ successfully infected the host?")
   )
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
(setq pathogen-list-layers
      '((:name +package-manager/elpaca :enabled-p t)
	(:name +package-manager/straight :enabled-p nil)
	(:name +base/org :enabled-p nil)
	(:name +productivity/gtd :dependencies (+base/org))
	))

;;; Load layers
(setq germs (mapcar #'pathogen-load-germ pathogen-list-layers))
