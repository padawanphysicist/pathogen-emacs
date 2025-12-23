;;; layers.el --- Setup configuration layer core functions -*- lexical-binding: t; fill-column: 79; -*-
;;
;; Copyright (C) 2021 Victor Santos
;;
;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module provides the core functions of Pathogen.
;;
;;; Code:

;;; EIEIO: Enhanced Implementation of Emacs Interpreted Objects
;;
;; https://www.gnu.org/software/emacs/manual/html_mono/eieio.html
(require 'eieio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API: User-facing functions
;;
(defun pathogen-add-layers-dir (dir)
  "Add DIR to the list of layer directories.
Layers in directories added first take precedence."
  (add-to-list 'pathogen-layers-dirs (expand-file-name dir) t))

;;; Pathogen Layer Structure
;;
;; A layer is a directory containing these files (loaded in order):
;;
;; File          Purpose
;; ----          -------
;; variables.el  - Declare all configurable variables and defcustom forms
;; functions.el  - Define helper functions and internal utilities  
;; config.el     - Package configuration, keybindings, and mode setup
;;
;; This organization separates interface (variables), implementation (functions),
;; and integration (config) for maintainable configuration layers.

(defvar pathogen--layers-table (make-hash-table :test 'equal)
  "Internal hash table storing layer configurations.
Do not access directly. Use `pathogen-layer-get' instead.")

(defclass pathogen-layer () ; No superclasses
  ((name :initarg :name
         :type symbol
         :custom symbol
	 :accessor pathogen-layer-name
         :documentation "Layer name.")
   (path :initarg :path
         :initform ""
         :type string
         :custom string
	 :accessor pathogen-layer-path
         :documentation "Path to layer directory.")
   (variables :initarg :variables
	      :initform nil
	      :accessor pathogen-layer-variables
	      :documentation "Plist of layer variables")
   (enabled-p :initarg :enabled-p
         :initform nil
         :type symbol
         :custom symbol
	 :accessor pathogen-layer-enabled-p
         :documentation "Whether or not the layer is enabled."))
  "A class for Pathogen layers. This is an internal structure. Access
fields using provided functions.")

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
                           (if (plist-member layer-spec :enabled-p)
                               (plist-get layer-spec :enabled-p)
                             t)
                         (if (null enabled) t enabled)))
         (layer-path (pathogen--find-layer-path layer-name))
         (name-str (pathogen--layer-name-to-string layer-name)))
    (list layer-name layer-vars layer-enabled)
    (if (and layer-enabled layer-path)
        (let ((layer-instance (make-instance 'pathogen-layer
                                            :name layer-name
                                            :path layer-path
                                            :enabled-p t
                                            :variables '())))
          ;; ;; Store layer
          ;; (pathogen--layer-put name-str layer-instance)
          
          ;; Set variables before loading files
          (when layer-vars
            (pathogen--layer-set-variables layer-instance layer-vars))
          
          ;; ;; Run pre-init hook
          ;; (run-hook-with-args 'pathogen-layer-pre-init-hook layer-instance)
          
          ;; Load layer files
          (pathogen--layer-load-files layer-instance)
          
          ;; ;; Mark as loaded
          ;; (add-to-list 'pathogen--enabled-layers name-str)
          
          ;; ;; Run post-init hook
          ;; (run-hook-with-args 'pathogen-layer-post-init-hook layer-instance)
          
          (message "[Pathogen] Loaded layer: %s" layer-name)
          layer-instance)
      (when layer-enabled
        (warn "✗ Layer not found: %s" layer-name)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private/Internal functions (pathogen--)
;;
;;

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
        (original-str (if (symbolp layer-name) (symbol-name layer-name) layer-name))
        (found-path nil))
    (dolist (base-dir pathogen-layers-dirs)
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

(defun pathogen--layer-put (layer-name layer-struct)
  "Internal: Store LAYER-STRUCT for LAYER-NAME."
  (puthash (pathogen--layer-name-to-string layer-name) layer-struct pathogen--layers-table))

(defun pathogen--layer-load-file (layer-struct file)
  "Internal: Load FILE for LAYER-STRUCT if it exists."
  (let ((filepath (expand-file-name file (pathogen-layer-path layer-struct))))
    (when (file-exists-p filepath)
      (load filepath)
      t)))

(defun pathogen--layer-load-files (layer-struct)
  "Internal: Load all configuration files for LAYER-STRUCT in order."
  (let ((files '("variables.el" "functions.el" "config.el")))
    (dolist (file files)
      (pathogen--layer-load-file layer-struct file))))

(defun pathogen--layer-set-variables (layer variables)
  "Internal: Set VARIABLES for LAYER-STRUCT from plist."
  (let ((vars variables)
        (layer-name (pathogen--layer-name-to-string (pathogen-layer-name layer))))
    (while vars
      (let* ((key (pop vars))
             (value (pop vars))
             ;; Replace / with - for variable names
             (var-base (replace-regexp-in-string "/" "-" layer-name))
             (var-name (intern (format "pathogen-%s/%s" 
                                      var-base
                                      (substring (symbol-name key) 1)))))
        ;; Set the actual variable
        (set var-name value)
        ;; Store in layer struct
        (setf (pathogen-layer-variables layer)
              (plist-put (pathogen-layer-variables layer) key value))))))

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

(message "[Pathogen] Loaded layer configuration module.")
(provide 'layers)
;;; layers.el ends here
