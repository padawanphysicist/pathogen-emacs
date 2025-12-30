;;; pathogen-microscope.el --- Inspection tools for the Pathogen genome -*- lexical-binding: t; fill-column: 79; -*-

;; Copyright (C) 2025 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27) (pathogen-logging) (pathogen-genome))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library provides diagnostic and inspection utilities for Pathogen.
;; It allows the user to peer into the loaded '*pathogen-genome*' to
;; verify which germs are registered, active, or currently under observation.
;;
;; Think of this as a "read-only" interface for debugging the internal
;; state of the configuration's DNA.

;;; Code:

(require 'pathogen-logging)
(require 'pathogen-genome)

(defun pathogen--microscope-get-germs ()
  "Return a list of all germ names currently in the genome."
  (let (names)
    (when *pathogen-genome*
      (maphash (lambda (name _obj) (push name names)) *pathogen-genome*))
    names))

(provide 'pathogen-microscope)
;;; pathogen-microscope.el ends here
