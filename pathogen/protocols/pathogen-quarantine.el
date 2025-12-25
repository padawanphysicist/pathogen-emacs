;;; quarantine.el --- Containment & Error Handling -*- lexical-binding: t; -*-
(require 'pathogen-dna)

(defvar pathogen-quarantine-list nil
  "List of germs that failed to load or had failing dependencies.")

(defun pathogen-quarantine-check-deps (germ)
  "Check if all dependencies of GERM have been successfully loaded.
Returns t if safe, nil if any dependency is missing or failed."
  (let ((safe t))
    (dolist (dep-name (pathogen-germ-dependencies germ))
      (let ((dep-obj (pathogen-dna-get dep-name)))
        (unless (and dep-obj (pathogen-germ-loaded-p dep-obj))
          (setq safe nil))))
    safe))

(provide 'pathogen-quarantine)
