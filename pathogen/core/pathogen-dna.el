;;; dna.el --- The Genetic Blueprint of Germs -*- lexical-binding: t; -*-

;; Description: Defines the structural proteins of the system.
;; Contains the `pathogen-germ` class and primitive data types.

;;; Code:

;;; EIEIO: Enhanced Implementation of Emacs Interpreted Objects
;;
;; https://www.gnu.org/software/emacs/manual/html_mono/eieio.html
(require 'eieio)

;;; GNU Emacs Common Lisp Emulation
;;
;; https://www.gnu.org/software/emacs/manual/html_mono/cl.html
(require 'cl-lib)

(defvar pathogen--genome (make-hash-table :test 'equal)
  "The global registry of all discovered Germs.
A hash table mapping germ names (symbols) to `pathogen-germ' objects.")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defmethod pathogen-infect ((germ pathogen-germ))
  "Activate the GERM by setting its variables and loading its files."
  (with-slots (name variables path enabled-p loaded-p) germ
    (cond
     ((not enabled-p)
      (message "[Pathogen] Germ %s is dormant (disabled), skipping." name))
     
     (loaded-p
      (message "[Pathogen] Germ %s is already active." name))
     
     (t
      ;; 1. Set variables (The Incubation phase)
      (when variables
        (pathogen--germ-set-variables germ variables))
      
      ;; 2. Load the files (The Infection phase)
      (let ((success (pathogen--germ-load-files germ)))
        (if success
            (progn
              (oset germ :loaded-p t)
              (message "[Pathogen] Successfully infected host with: %s" name))
          (warn "[Pathogen] Failed to load files for germ: %s" name)))
      success))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pathogen-dna-register (germ-obj)
  "Store a GERM-OBJ in the global genome registry."
  (cl-check-type germ-obj pathogen-germ)
  (puthash (pathogen-germ-name germ-obj) germ-obj pathogen--genome)
  (message "Pathogen [DNA]: Registered %s" (pathogen-germ-name germ-obj)))

(defun pathogen-dna-get (name)
  "Retrieve a germ object from the genome by its symbol NAME."
  (gethash name pathogen--genome))

(defun pathogen-dna-all-names ()
  "Return a list of all germ names currently in the genome."
  (let (names)
    (maphash (lambda (k _v) (push k names)) pathogen--genome)
    names))


(provide 'pathogen-dna)
