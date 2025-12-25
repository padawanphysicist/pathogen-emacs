;;; infection.el --- Transmission Protocols & API -*- lexical-binding: t; -*-
(require 'pathogen-dna)
(require 'pathogen-incubator)

;;(cl-defun pathogen-create-germ (name &key vars enabled deps)
;;  "Factory to create and register a germ."
;;  (let ((instance (pathogen-germ :name name
;;                                :dependencies deps
;;                                :variables vars
;;                                :enabled-p (if (null enabled) t enabled))))
;;    (pathogen-dna-register instance)
;;    instance))

;;(cl-defun pathogen-create-germ (name &key vars enabled deps)
;;  "Factory to create and register a germ."
;;  (let ((instance (pathogen-germ :name name
;;                                :dependencies deps
;;                                :variables vars
;;                                :path (pathogen--find-germ-path name) ; <--- Add this!
;;                                :enabled-p (if (null enabled) t enabled))))
;;    (pathogen-dna-register instance)
;;    instance))

(cl-defun pathogen-create-germ (name &key vars enabled deps)
  "Factory to create and register a germ."
  (let ((instance (pathogen-germ :name name
                                :dependencies deps
                                :variables vars
                                :enabled-p (if (null enabled) t enabled)
                                ;; CRITICAL: This must be here!
                                :path (pathogen--find-germ-path name))))
    (pathogen-dna-register instance)
    instance))

(defmacro define-germ (name &rest props)
  "DSL to define a germ."
  `(pathogen-create-germ ',name ,@props))

(defun pathogen-lab-generate-germ (name)
  "Generate a new Germ directory and pre-fill files with namespaced wrappers."
  (interactive "sEnter Germ Name (e.g. +ui/fonts): ")
  (let* ((germ-dir (expand-file-name name (pathogen--get-lab-root)))
         (files '("genome.el" "enzymes.el" "symptoms.el"))
         (descriptions '("Base variables and data blueprints."
                         "Functional catalysts and custom logic."
                         "Outward expression and package configuration.")))
    
    (if (file-exists-p germ-dir)
        (error "Germ '%s' already exists!" name)
      (make-directory germ-dir t)
      
      (cl-loop for file in files
               for desc in descriptions
               do (let ((path (expand-file-name file germ-dir)))
                    (with-temp-file path
                      ;; Header
                      (insert (format ";;; %s --- %s -*- lexical-binding: t; -*-\n\n" file desc))
                      ;; The namespaced wrapper
                      (insert (format "(with-germ-data %s ()\n" name))
                      (insert "  \"Insert logic here.\"\n")
                      (insert "  )\n"))))
      
      (message "Infection Prepared: Germ '%s' synthesized with namespaced wrappers." name)
      (dired germ-dir))))


;; (defun pathogen-lab-generate-germ (name)
;;   "Generate a new Germ directory structure for NAME.
;; Creates genome.el, enzymes.el, and symptoms.el in the laboratory path."
;;   (interactive "sEnter Germ Name: ")
;;   (let* ((germ-dir (expand-file-name name (pathogen--get-lab-root)))
;;          (files '("genome.el" "enzymes.el" "symptoms.el"))
;;          (descriptions '("Base variables and data blueprints."
;;                          "Functional catalysts and custom logic."
;;                          "Outward expression and package configuration.")))
    
;;     (if (file-exists-p germ-dir)
;;         (error "Germ '%s' already exists in the laboratory!" name)
;;       ;; Create the physical environment
;;       (make-directory germ-dir t)
      
;;       ;; Synthesize the files
;;       (cl-loop for file in files
;;                for desc in descriptions
;;                do (let ((path (expand-file-name file germ-dir)))
;;                     (with-temp-file path
;;                       (insert (format ";;; %s --- %s -*- lexical-binding: t; -*-\n\n" file desc))
;;                       (insert (format ";;; Part of the '%s' germ.\n\n" name))
;;                       (insert "(provide '" (symbol-name (intern (format "%s-%s" name (file-name-base file)))) ")\n"))))
      
;;       (message "Infection Prepared: Germ '%s' has been synthesized at %s" name germ-dir)
;;       (dired germ-dir))))

(defun pathogen--get-lab-root ()
  "Return the root directory where Germs are stored."
  (expand-file-name "germs/" user-emacs-directory))

(defun pathogen--find-germ-path (name)
  "Convert a germ name (like +ui/fonts) into a path."
  (let ((name-str (symbol-name name)))
    ;; This will look in ~/.emacs.d/germs/+ui/fonts/
    (expand-file-name name-str (expand-file-name "germs/" user-emacs-directory))))

(cl-defun pathogen-create-germ (name &key vars deps (enabled t))
  "Factory to create and register a germ with path resolution."
  (let ((instance (pathogen-germ :name name
                                :dependencies deps
                                :variables vars
                                :enabled-p enabled
                                :path (pathogen--find-germ-path name))))
    (pathogen-dna-register instance)
    instance))

(defmacro infect! (&rest layers)
  `(progn
     (clrhash pathogen--genome)
     (dolist (layer ',layers)
       (let (name variables dependencies)
         (if (symbolp layer)
             (setq name layer)
           (setq name (car layer)
                 variables (plist-get (cdr layer) :variables)
                 dependencies (plist-get (cdr layer) :deps))) ; Capture :deps
         
         (pathogen-create-germ name 
                               :vars variables 
                               :deps dependencies)))
     (pathogen-propagate)))

;(defmacro infect! (&rest layers)
;  "Declarative layer definition with Ghost Germ tracing."
;  `(progn
;     ;; Clear the genome before a fresh layer load to prevent stale ghosts
;     (clrhash pathogen--genome)
;     (dolist (layer ',layers)
;       (let (name variables)
;         (cond
;          ;; Case 1: Simple symbol (+ui/themes)
;          ((symbolp layer) 
;           (setq name layer))
;          ;; Case 2: List with variables ((+ui/fonts :variables ...))
;          ((listp layer)
;           (setq name (car layer)
;                 variables (plist-get (cdr layer) :variables)))
;          (t (warn "[Pathogen Trace] Invalid layer format: %s" layer)))
;
;         (when name
;           (pathogen-create-germ name :vars variables))))
;     
;     (message "[Pathogen Trace] Sequence calculated: %s" (pathogen-sequence-dna))
;     (pathogen-propagate)))
;
;(defmacro infect! (&rest layers)
;  "Declaratively define and enable multiple germs with namespacing."
;  `(progn
;     (dolist (layer ',layers)
;       (let (name variables dependencies)
;         (if (symbolp layer)
;             (setq name layer)
;           (setq name (car layer)
;                 variables (plist-get (cdr layer) :variables)
;                 dependencies (plist-get (cdr layer) :deps)))
;         
;         (pathogen-create-germ name 
;                               :vars variables 
;                               :deps dependencies 
;                               :enabled t)))
;     (pathogen-sequence-dna)
;     (pathogen-propagate))) ; Automatically trigger infection after definition
;; (defmacro infect! (&rest layers)
;;   "Declaratively define and enable multiple germs.
;; Accepts symbols (simple germs) or lists (germs with properties).
;; Example: (pathogen-layers! +ui/themes (+ui/fonts :variables '(:size 12)))"
;;   `(progn
;;      (dolist (layer ',layers)
;;        (let (name variables dependencies enabled)
;;          ;; Determine if layer is a symbol or a list
;;          (if (symbolp layer)
;;              (setq name layer
;;                    enabled t)
;;            (setq name (car layer)
;;                  enabled t
;;                  variables (plist-get (cdr layer) :variables)
;;                  dependencies (plist-get (cdr layer) :deps)))
         
;;          ;; Create the germ and register it
;;          (pathogen-create-germ name 
;;                                :vars variables 
;;                                :deps dependencies 
;;                                :enabled enabled)))
;;      ;; After defining all, calculate the sequence
;;      (pathogen-sequence-dna)
;;      (message "[Pathogen] %d layers synthesized into the genome." (length ',layers))))

(defmacro with-germ-data (germ-name vars &rest body)
  "Create a local scope for GERM-NAME using VARS.
VARS should be a list of short symbols.
Example: (with-germ-data +ui/fonts (default-preset) (message default-preset))"
  (declare (indent 2))
  (let ((mappings (mapcar (lambda (v)
                            (list v (intern (format "%s-%s" germ-name v))))
                          vars)))
    `(cl-symbol-macrolet ,mappings
       ,@body)))

(provide 'pathogen-infection)
