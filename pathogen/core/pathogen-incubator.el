;;; incubator.el --- Feature Cultivation & Loading -*- lexical-binding: t; -*-
(require 'pathogen-dna)

(defvar pathogen-germ-load-sequence '("genome.el" "enzymes.el" "symptoms.el")
  "The chronological expression of a Germ's biological payload.")

;;; incubator.el --- Robust Expression Logic -*- lexical-binding: t; -*-

(cl-defmethod pathogen-infect ((germ pathogen-germ))
  "Directly infect the host, injecting namespaced variables first."
  (with-slots (name enabled-p loaded-p variables) germ
    (when (and enabled-p (not loaded-p))
      ;; --- The Namespacing Injection ---
      (cl-loop for (var val) on variables by #'cddr
               do (let* ((clean-var (substring (symbol-name var) 1))
                         ;; Result: 'germ-name-variable-name
                         (scoped-symbol (intern (format "%s-%s" 
                                                       (symbol-name name) 
                                                       clean-var))))
                    (set scoped-symbol val)
                    (message "[Pathogen] Injected: %s" scoped-symbol)))
      
      ;; Proceed to load files
      (if (pathogen--germ-load-files germ)
          (progn
            (oset germ :loaded-p t)
            t)
        (progn
          (push name pathogen-quarantine-list)
          nil)))))

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

(provide 'pathogen-incubator)
