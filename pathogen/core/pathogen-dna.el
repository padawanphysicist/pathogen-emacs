;;; dna.el --- The Genetic Blueprint of Germs -*- lexical-binding: t; -*-
(require 'eieio)
(require 'cl-lib)


;; (defclass pathogen-germ ()
;;   ((name :initarg :name :type symbol :accessor pathogen-germ-name)
;;    (dependencies :initarg :dependencies :initform nil :type list :accessor pathogen-germ-dependencies)
;;    (path :initarg :path :initform "" :type string :accessor pathogen-germ-path)
;;    (variables :initarg :variables :initform nil :type list :accessor pathogen-germ-variables)
;;    (enabled-p :initarg :enabled-p :initform t :type boolean :accessor pathogen-germ-enabled-p)
;;    (loaded-p :initform nil :type boolean :accessor pathogen-germ-loaded-p))
;;   "The base class for a Pathogen feature layer.")

(defclass pathogen-germ ()
  ((name :initarg :name :type symbol :accessor pathogen-germ-name)
   (dependencies :initarg :dependencies :initform nil :type list :accessor pathogen-germ-dependencies)
   (path :initarg :path :initform "" :type string :accessor pathogen-germ-path)
   (variables :initarg :variables :initform nil :type list :accessor pathogen-germ-variables)
   (enabled-p :initarg :enabled-p :initform t :type boolean :accessor pathogen-germ-enabled-p)
   ;; Add the :initarg here!
   (loaded-p :initarg :loaded-p :initform nil :type boolean :accessor pathogen-germ-loaded-p))
  "The base class for a Pathogen feature layer.")

;(defun pathogen-dna-register (germ-obj)
;  (puthash (pathogen-germ-name germ-obj) germ-obj pathogen--genome))
(defun pathogen-dna-register (germ)
  "Store GERM in the global genome with real-time tracing."
  (let ((name (pathogen-germ-name germ)))
    (message "[Pathogen Trace] Registering: %s (Path: %s)" 
             name (pathogen-germ-path germ))
    (puthash name germ pathogen--genome)))

(defun pathogen-dna-get (name)
  "Retrieve a germ, handling potential quoting issues."
  (let ((clean-name (if (and (listp name) (eq (car name) 'quote))
                        (cadr name)
                      name)))
    (gethash clean-name pathogen--genome)))
;
;(defun pathogen-dna-get (name)
;  (gethash name pathogen--genome))

(defun pathogen-dna-all-names ()
  "Return a list of all germ names currently in the genome."
  (let (names)
    (when pathogen--genome
      (maphash (lambda (name _obj) (push name names)) pathogen--genome))
    names))

(provide 'pathogen-dna)
