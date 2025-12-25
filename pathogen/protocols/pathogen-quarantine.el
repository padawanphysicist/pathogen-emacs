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

(defun pathogen-display-quarantine-report ()
  "Generate a visual report of all quarantined Germs."
  (interactive)
  (when pathogen-quarantine-list
    (with-current-buffer (get-buffer-create "*Pathogen Quarantine*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "🧬 PATHOGEN QUARANTINE REPORT\n")
        (insert "==============================\n\n")
        (insert "The following germs failed to express and have been isolated:\n\n")
        
        (dolist (germ-name pathogen-quarantine-list)
          (let ((err (gethash germ-name pathogen-biopsy-report "Unknown Error")))
            (insert (format "🔴 [ %s ]\n" germ-name))
            (insert (format "   Reason: %s\n" err))
            (insert (format "   Path:   %s\n\n" (pathogen--find-germ-path germ-name)))))
        
        (insert "------------------------------\n")
        (insert "Press 'q' to dismiss this laboratory report.\n")
        (view-mode)
        (local-set-key (kbd "q") 'kill-buffer-and-window)
        (display-buffer (current-buffer))))))

(provide 'pathogen-quarantine)
