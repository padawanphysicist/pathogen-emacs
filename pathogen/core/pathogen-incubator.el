;;; incubator.el --- Feature Cultivation & Loading -*- lexical-binding: t; -*-
(require 'pathogen-dna)

(defvar pathogen-germ-load-sequence '("genome.el" "enzymes.el" "symptoms.el")
  "The chronological expression of a Germ's biological payload.")

;;; incubator.el --- Robust Expression Logic -*- lexical-binding: t; -*-

(cl-defmethod pathogen-infect ((germ pathogen-germ))
  "Inject DNA into the global namespace, ensuring overrides take hold."
  (with-slots (name variables path) germ
    ;; 🧬 DNA Injection Phase
    (cl-loop for (var val) on variables by #'cddr
             do (let* ((var-name (substring (symbol-name var) 1))
                       (prefixed-name (format "%s-%s" name var-name))
                       (sym (intern prefixed-name)))
                  ;; Use 'set' to force the value even if defvar exists
                  (set sym val)
		  (message "🧬 [DNA] Injected %s = %s" prefixed-name val)))

    ;; 🦠 Expression Phase
    ;; We only load genome and enzymes here. 
    ;; Symptoms are loaded later by pathogen-propagate.
    (let ((core-files '("genome.el" "enzymes.el")))
      (dolist (f core-files)
        (let ((full-path (expand-file-name f path)))
          (when (file-exists-p full-path)
            (load full-path nil 'nomessage)))))))

;;(cl-defmethod pathogen-infect ((germ pathogen-germ))
;;  "Inject variables with prefixes and load germ files."
;;  (with-slots (name variables path) germ
;;    ;; 1. The "DNA Injection" phase
;;    ;; Iterates through (:idle-delay 0.3) and creates +core/reflex-idle-delay
;;    (cl-loop for (var val) on variables by #'cddr
;;             do (let* ((var-name (substring (symbol-name var) 1)) ;; remove the ':'
;;                       (prefixed-name (format "%s-%s" name var-name))
;;                       (sym (intern prefixed-name)))
;;                  (set sym val)
;;		  ;; ADD THIS LINE FOR DEBUGGING:
;;		  (message "DEBUG: Injecting DNA [%s] with value [%s]" sym val)
;;		  (message "🧬 [DNA] Injected %s = %s" prefixed-name val)))
;;
;;    ;; 2. The "Expression" phase
;;    (let ((files '("genome.el" "enzymes.el" "symptoms.el")))
;;      (dolist (f files)
;;        (let ((full-path (expand-file-name f path)))
;;          (when (file-exists-p full-path)
;;            (load full-path nil 'nomessage)))))))

;(cl-defmethod pathogen-infect ((germ pathogen-germ))
;  "Directly infect the host, injecting namespaced variables first."
;  (with-slots (name enabled-p loaded-p variables) germ
;    (when (and enabled-p (not loaded-p))
;      ;; --- The Namespacing Injection ---
;      (cl-loop for (var val) on variables by #'cddr
;               do (let* ((clean-var (substring (symbol-name var) 1))
;                         ;; Result: 'germ-name-variable-name
;                         (scoped-symbol (intern (format "%s-%s" 
;                                                       (symbol-name name) 
;                                                       clean-var))))
;                    (set scoped-symbol val)
;                    (message "[Pathogen] Injected: %s" scoped-symbol)))
;      
;      ;; Proceed to load files
;      (if (pathogen--germ-load-files germ)
;          (progn
;            (oset germ :loaded-p t)
;            t)
;        (progn
;          (push name pathogen-quarantine-list)
;          nil)))))

;; (cl-defmethod pathogen-infect ((germ pathogen-germ))
;;   "Directly infect the host, injecting variables first."
;;   (with-slots (name enabled-p loaded-p variables) germ
;;     (when (and enabled-p (not loaded-p))
;;       ;; Inject variables into the global scope
;;       (cl-loop for (var val) on variables by #'cddr
;;                do (set (intern (substring (symbol-name var) 1)) val))
      
;;       ;; Now load the files
;;       (when (pathogen--germ-load-files germ)
;;         (oset germ :loaded-p t)
;;         t))))

;; (cl-defmethod pathogen-infect ((germ pathogen-germ))
;;   "Directly infect the host with the GERM."
;;   (with-slots (name enabled-p loaded-p path) germ
;;     (cond
;;      (loaded-p 
;;       (message "[Pathogen] %s is already active." name) t)
;;      ((not enabled-p) 
;;       (message "[Pathogen] %s is dormant." name) nil)
;;      (t
;;       (let ((result (pathogen--germ-load-files germ)))
;;         (if result
;;             (progn (oset germ :loaded-p t) t)
;;           (progn (push name pathogen-quarantine-list) nil)))))))



(defun pathogen--germ-load-files (germ)
  "Expression logic: Loads available files. Not all files are mandatory."
  (let ((dir (pathogen-germ-path germ))
        (name (pathogen-germ-name germ))
        (components '("genome.el" "enzymes.el" "symptoms.el"))
        (found-at-least-one nil))
    
    (if (and dir (file-directory-p dir))
        (condition-case err
            (progn
              (dolist (file components)
                (let ((full-path (expand-file-name file dir)))
                  (when (file-exists-p full-path)
                    (load full-path nil 'nomessage)
                    (setq found-at-least-one t))))
              ;; We return T if the directory existed, even if empty.
              ;; Or return found-at-least-one if you want to be stricter.
              t)
          (error
           (warn "[Pathogen] Expression crash in %s: %s" name err)
           nil))
      (progn
        (warn "[Pathogen] Directory missing for %s at %s" name dir)
        nil))))
;;
;;
;;(cl-defmethod pathogen-infect ((germ pathogen-germ))
;;  "Directly infect the host with the GERM."
;;  (with-slots (name enabled-p loaded-p) germ
;;    (if (and enabled-p (not loaded-p))
;;        (when (pathogen--germ-load-files germ)
;;          (oset germ :loaded-p t)
;;          (message "[Pathogen] %s successfully expressed." name)
;;          t)
;;      nil)))
;;
;;;;(cl-defmethod pathogen-infect ((germ pathogen-germ))
;;;;  "Activate the GERM."
;;;;  (with-slots (name variables path enabled-p loaded-p) germ
;;;;    (when (and enabled-p (not loaded-p))
;;;;      ;; Note: Replace with your actual file loading logic
;;;;      (message "[Pathogen] Infecting with %s..." name)
;;;;      (oset germ :loaded-p t)
;;;;      t)))
;;
;;
;;
;;
;;;;; incubator.el --- Feature Cultivation & Loading -*- lexical-binding: t; -*-
;;
;;(defun pathogen--germ-load-files (germ)
;;  "Expression logic: Loads available files. Not all files are mandatory."
;;  (let ((dir (pathogen-germ-path germ))
;;        (name (pathogen-germ-name germ))
;;        ;; Our defined sequence
;;        (components '("genome.el" "enzymes.el" "symptoms.el")))
;;    
;;    (if (and dir (file-directory-p dir))
;;        (progn
;;          (dolist (file components)
;;            (let ((full-path (expand-file-name file dir)))
;;              (when (file-exists-p full-path)
;;                (condition-case err
;;                    (load full-path nil 'nomessage)
;;                  (error
;;                   (warn "[Pathogen] Error in %s/%s: %s" name file err)
;;                   ;; Return nil to trigger Quarantine if a file is BROKEN
;;                   (cl-return-from pathogen--germ-load-files nil))))))
;;          ;; If we reached here, the directory existed and no errors occurred
;;          t)
;;      ;; Directory missing is a failure
;;      (warn "[Pathogen] Laboratory directory missing for: %s" name)
;;      nil)))

;;(defun pathogen--germ-load-files (germ)
;;  "Sequentially express the components of the GERM."
;;  (let ((dir (pathogen-germ-path germ))
;;        (name (pathogen-germ-name germ))
;;        (success t))
;;    (if (and dir (file-directory-p dir))
;;        (progn
;;          (dolist (component pathogen-germ-load-sequence)
;;            (let ((file (expand-file-name component dir)))
;;              (when (file-exists-p file)
;;                (condition-case err
;;                    (load file nil 'nomessage)
;;                  (error 
;;                   (setq success nil)
;;                   (warn "[Pathogen] Expression error in %s/%s: %s" name component err))))))
;;          success)
;;      (warn "[Pathogen] Directory not found for %s: %s" name dir)
;;      nil)))

(defun pathogen-diagnose-germ (germ-name)
  "Print all live variables associated with GERM-NAME."
  (interactive "sGerm name (e.g. +ui/organoid): ")
  (let ((found-vars nil)
        (prefix (concat germ-name "-")))
    (mapatoms
     (lambda (atom)
       (when (string-prefix-p prefix (symbol-name atom))
         (push (cons atom (if (boundp atom) (symbol-value atom) "VOID")) 
               found-vars))))
    (if found-vars
        (with-current-buffer (get-buffer-create "*Pathogen Diagnosis*")
          (erase-buffer)
          (insert (format "🔬 Diagnostic Report for Germ: %s\n" germ-name))
          (insert "==========================================\n\n")
          (dolist (pair found-vars)
            (insert (format "%-30s : %s\n" (car pair) (cdr pair))))
          (display-buffer (current-buffer)))
      (message "❌ No variables found for germ: %s" germ-name))))

(defun pathogen--topological-sort (genome)
  "Sort germs in GENOME by their dependencies."
  (let ((sorted '())
        (visited (make-hash-table :test 'equal))
        (temp-marks (make-hash-table :test 'equal)))
    (cl-labels ((visit (name)
                  (when (gethash name temp-marks)
                    (error "🧬 [Mutation] Circular dependency detected in germ: %s" name))
                  (unless (gethash name visited)
                    (puthash name t temp-marks)
                    (let ((germ (gethash name genome)))
                      (when germ
                        (dolist (dep (slot-value germ 'dependencies))
                          (visit dep))))
                    (puthash name nil temp-marks)
                    (puthash name t visited)
                    (push name sorted))))
      (maphash (lambda (name _germ) (visit name)) genome)
      (reverse sorted))))

(provide 'pathogen-incubator)
