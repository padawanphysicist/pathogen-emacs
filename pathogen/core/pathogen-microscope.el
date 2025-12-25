;;; microscope.el --- Laboratory Diagnostics -*- lexical-binding: t; -*-
(require 'pathogen-dna)
(require 'pathogen-quarantine)

(defun pathogen--format-tree (name depth visited)
  "Recursively format a dependency tree for NAME at DEPTH."
  (let ((germ (pathogen-dna-get name))
        (indent (make-string (* depth 3) ?\s)))
    (unless (gethash name visited)
      (puthash name t visited)
      (insert (format "%s└─ %s %s\n" 
                      indent 
                      name 
                      (if (pathogen-germ-loaded-p germ) "🧬" "💤")))
      ;; Visit children (germs that depend on this one)
      (maphash (lambda (child-name child-germ)
                 (when (member name (pathogen-germ-dependencies child-germ))
                   (pathogen--format-tree child-name (+ depth 1) visited)))
               pathogen--genome))))

(defun pathogen-microscope ()
  "Inspect the Pathogen Genome with a dependency tree view."
  (interactive)
  (with-current-buffer (get-buffer-create "*Pathogen Microscope*")
    (let ((inhibit-read-only t)
          (visited (make-hash-table :test 'equal)))
      (erase-buffer)
      (insert "🔬 PATHOGEN GENOME MICROSCOPE\n")
      (insert "================================\n\n")
      
      (insert "CORE GENOME TREE\n")
      (insert "----------------\n")
      ;; Start with germs that have NO dependencies (the roots)
      (maphash (lambda (name germ)
                 (unless (pathogen-germ-dependencies germ)
                   (pathogen--format-tree name 0 visited)))
               pathogen--genome)
      
      (insert "\nLAB REGISTRY DETAILS\n")
      (insert "--------------------\n")
      (maphash
       (lambda (key germ)
         (insert (format "%-20s | %s | %s\n" 
                         key 
                         (if (pathogen-germ-loaded-p germ) "ACTIVE" "DORMANT")
                         (pathogen-germ-path germ))))
       pathogen--genome)
      
      (view-mode)
      (local-set-key (kbd "q") 'kill-buffer-and-window)
      (display-buffer (current-buffer)))))


;(defun pathogen-microscope ()
;  "Display a diagnostic report of the Pathogen system."
;  (interactive)
;  (let ((buf (get-buffer-create "*Pathogen Microscope*")))
;    (with-current-buffer buf
;      (let ((inhibit-read-only t))
;        (erase-buffer)
;        (insert (propertize "PATHOGEN DIAGNOSTIC REPORT\n" 'face 'bold))
;        (insert (format "Host: %s | Time: %s\n" (system-name) (current-time-string)))
;        (insert "------------------------------------------\n\n")
;        
;        (dolist (germ-name (pathogen-dna-all-names))
;          (let* ((germ (pathogen-dna-get germ-name))
;                 (status (cond
;                          ((memq germ-name pathogen-quarantine-list) 
;                           (propertize "QUARANTINED" 'face 'error))
;                          ((pathogen-germ-loaded-p germ) 
;                           (propertize "ACTIVE" 'face 'success))
;                          ((not (pathogen-germ-enabled-p germ)) 
;                           (propertize "DORMANT" 'face 'warning))
;                          (t "UNKNOWN"))))
;            (insert (format "[%-12s] %s\n" status germ-name))
;            (when (pathogen-germ-dependencies germ)
;              (insert (format "  DNA: %s\n" (pathogen-germ-dependencies germ))))))
;        
;        (insert "\nSummary:\n")
;        (insert (format "- Total Strains: %d\n" (length (pathogen-dna-all-names))))
;        (insert (format "- Quarantined:   %d\n" (length pathogen-quarantine-list))))
;      (read-only-mode 1))
;    (display-buffer buf)))

(provide 'pathogen-microscope)
