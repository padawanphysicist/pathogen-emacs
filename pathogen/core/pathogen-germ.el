;;; pathogen-germ.el --- Germ class definitions and DNA injection -*- lexical-binding: t; fill-column: 79; -*-

;; Copyright (C) 2025 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27) (pathogen-logging) (pathogen-genome))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library defines the core 'pathogen-germ' EIEIO class, representing 
;; the modular units (germs) of the Pathogen Emacs configuration.
;;
;; It handles the "DNA Injection" phase, where germ-specific variables 
;; are interned into the global namespace, and the "Expression Phase," 
;; where core configuration files (genome/enzymes) are loaded.

;;; Code:

(require 'pathogen-logging)
(require 'pathogen-genome)

(require 'eieio)
(require 'cl-lib)

(defvar pathogen--visiting-stack nil
  "Temporary stack to detect circular dependencies during germ loading.")

(defvar pathogen--loading-stack nil
  "List of germs currently being loaded to detect circular dependencies.")

(defclass pathogen-germ ()
  ((name
    :initarg :name
    :type symbol
    :accessor pathogen-germ-name)
   (dependencies
    :initarg :dependencies
    :initform nil
    :type list
    :accessor pathogen-germ-dependencies)
   (path
    :initarg :path
    :initform ""
    :type string
    :accessor pathogen-germ-path)
   (variables
    :initarg :variables
    :initform nil
    :type list
    :accessor pathogen-germ-variables)
   (enabled-p
    :initarg :enabled-p
    :initform t
    :type boolean
    :accessor pathogen-germ-enabled-p)
   (loaded-p
    :initarg :loaded-p
    :initform nil
    :type boolean
    :accessor pathogen-germ-loaded-p))
  "The base class for a Pathogen feature layer.")

(defun pathogen-germ--get-path (germ)
  (let* ((name-str (format "%s" (if (symbolp germ) germ (car germ))))
         ;; Ensure we are working with a list even if a single string is provided
         (dirs (if (listp pathogen-germs-directories) 
                   pathogen-germs-directories 
                 (list pathogen-germs-directories)))
         ;; Find the first directory that exists
         (found-dir (seq-find (lambda (d) 
                                (file-directory-p (expand-file-name name-str d)))
                              dirs)))
    ;; Return the found path, or fallback to the first dir if none exist
    (expand-file-name name-str (or found-dir (car dirs)))))

(defun pathogen-germ--get-dependencies (germ)
  "Extract dependencies from a file's ';; @dependencies:' comment line."
  (let ((filename (format "%s/%s" (pathogen-germ--get-path germ) (car pathogen-germ-core-files)))
	(deps '()))
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	;; Search for the pattern: ;; @dependencies: followed by everything to end of line
	(when (re-search-forward ";; @dependencies:[ \t]*\\(.*\\)$" nil t)
          (let ((deps-string (match-string 1)))
            ;; Split by comma, trim whitespace from each element, and remove empty strings
            (setq deps
                  (mapcar #'string-trim 
                          (split-string deps-string "," t)))))))
    deps))

(cl-defmethod pathogen-germ--set-path! ((obj pathogen-germ))
  "Convert a germ name (like +ui/fonts) into a path."
  (oset
   obj path
   (pathogen-germ--get-path (pathogen-germ-name obj))))

(cl-defun pathogen-germ--set-dependencies! ((obj pathogen-germ))
  "Extract dependencies from a file's ';; @dependencies:' comment line."
  (oset obj dependencies (pathogen-germ--get-dependencies (pathogen-germ-name obj))))

(cl-defmethod pathogen-germ--register ((obj pathogen-germ))
  "Register the germ in the global genome automatically."
  (pathogen/log 'debug "🦠 Registering: %s (Path: %s)" (pathogen-germ-name obj) (pathogen-germ-path obj))
  (puthash (pathogen-germ-name obj) obj *pathogen-genome*)
  t)

(cl-defmethod pathogen-germ--infect ((germ pathogen-germ))
  "Inject DNA into the global namespace, ensuring overrides take hold.
Returns t if injection and loading were successful, nil otherwise."
  (with-slots (name path variables dependencies enabled-p loaded-p) germ
    ;; 🧬 DNA Injection Phase
    (cl-loop for (var val) on variables by #'cddr
             do (let* ((var-name (substring (symbol-name var) 1))
                       (prefixed-name (format "%s-%s" name var-name))
                       (sym (intern prefixed-name)))
                  (set sym val)
                  (pathogen/log 'debug "🦠 Injected %s = %s" prefixed-name val)))

    ;; 🦠 Expression Phase
    (let ((core-files pathogen-germ-core-files)
          (all-loaded t)) ; Start with the assumption of success
      (dolist (f core-files)
        (let ((full-path (expand-file-name f path)))
          (if (file-exists-p full-path)
              ;; load returns t on success. If it fails, all-loaded becomes nil.
	      (progn
                (unless (load full-path nil 'message)
                  (setq all-loaded nil))
	        (pathogen/log 'info "Loaded: %s" full-path))
            ;; If a core file is missing, you might consider that a failure
            (pathogen/log 'warning "⚠️ [%s] Missing core file: %s" (pathogen-germ-name germ) f))))
      
      ;; Final expression determines the return value of the method
      (setf loaded-p all-loaded)
      all-loaded)))

(provide 'pathogen-germ)
;;; pathogen-germ.el ends here
