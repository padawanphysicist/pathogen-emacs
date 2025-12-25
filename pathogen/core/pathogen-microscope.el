;;; microscope.el --- Laboratory Diagnostics -*- lexical-binding: t; -*-
(require 'pathogen-dna)
(require 'pathogen-quarantine)

(defun pathogen-microscope ()
  "Display a diagnostic report of the Pathogen system."
  (interactive)
  (let ((buf (get-buffer-create "*Pathogen Microscope*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "PATHOGEN DIAGNOSTIC REPORT\n" 'face 'bold))
        (insert (format "Host: %s | Time: %s\n" (system-name) (current-time-string)))
        (insert "------------------------------------------\n\n")
        
        (dolist (germ-name (pathogen-dna-all-names))
          (let* ((germ (pathogen-dna-get germ-name))
                 (status (cond
                          ((memq germ-name pathogen-quarantine-list) 
                           (propertize "QUARANTINED" 'face 'error))
                          ((pathogen-germ-loaded-p germ) 
                           (propertize "ACTIVE" 'face 'success))
                          ((not (pathogen-germ-enabled-p germ)) 
                           (propertize "DORMANT" 'face 'warning))
                          (t "UNKNOWN"))))
            (insert (format "[%-12s] %s\n" status germ-name))
            (when (pathogen-germ-dependencies germ)
              (insert (format "  DNA: %s\n" (pathogen-germ-dependencies germ))))))
        
        (insert "\nSummary:\n")
        (insert (format "- Total Strains: %d\n" (length (pathogen-dna-all-names))))
        (insert (format "- Quarantined:   %d\n" (length pathogen-quarantine-list))))
      (read-only-mode 1))
    (display-buffer buf)))

(provide 'pathogen-microscope)
