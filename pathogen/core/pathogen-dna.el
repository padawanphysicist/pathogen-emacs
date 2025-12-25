;;; dna.el --- The Genetic Blueprint of Germs -*- lexical-binding: t; -*-
(require 'eieio)
(require 'cl-lib)

(defvar pathogen--genome (make-hash-table :test 'equal)
  "The global registry of all discovered Germs.")

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

(defun pathogen-dna-register (germ-obj)
  (puthash (pathogen-germ-name germ-obj) germ-obj pathogen--genome))

(defun pathogen-dna-get (name)
  (gethash name pathogen--genome))

(defun pathogen-dna-all-names ()
  "Return a list of all germ names currently in the genome."
  (let (names)
    (when pathogen--genome
      (maphash (lambda (name _obj) (push name names)) pathogen--genome))
    names))

(provide 'pathogen-dna)
