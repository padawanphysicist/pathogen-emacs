;;; infection.el --- Transmission Protocols & API -*- lexical-binding: t; -*-
(require 'pathogen-dna)
(require 'pathogen-incubator)

(cl-defun pathogen-create-germ (name &key vars enabled deps)
  "Factory to create and register a germ."
  (let ((instance (pathogen-germ :name name
                                :dependencies deps
                                :variables vars
                                :enabled-p (if (null enabled) t enabled))))
    (pathogen-dna-register instance)
    instance))

(defmacro define-germ (name &rest props)
  "DSL to define a germ."
  `(pathogen-create-germ ',name ,@props))

(defun pathogen-lab-generate-germ (name)
  "Generate a new Germ directory structure for NAME.
Creates genome.el, enzymes.el, and symptoms.el in the laboratory path."
  (interactive "sEnter Germ Name: ")
  (let* ((germ-dir (expand-file-name name (pathogen--get-lab-root)))
         (files '("genome.el" "enzymes.el" "symptoms.el"))
         (descriptions '("Base variables and data blueprints."
                         "Functional catalysts and custom logic."
                         "Outward expression and package configuration.")))
    
    (if (file-exists-p germ-dir)
        (error "Germ '%s' already exists in the laboratory!" name)
      ;; Create the physical environment
      (make-directory germ-dir t)
      
      ;; Synthesize the files
      (cl-loop for file in files
               for desc in descriptions
               do (let ((path (expand-file-name file germ-dir)))
                    (with-temp-file path
                      (insert (format ";;; %s --- %s -*- lexical-binding: t; -*-\n\n" file desc))
                      (insert (format ";;; Part of the '%s' germ.\n\n" name))
                      (insert "(provide '" (symbol-name (intern (format "%s-%s" name (file-name-base file)))) ")\n"))))
      
      (message "Infection Prepared: Germ '%s' has been synthesized at %s" name germ-dir)
      (dired germ-dir))))

(defun pathogen--get-lab-root ()
  "Return the root directory where Germs are stored."
  (expand-file-name "germs/" user-emacs-directory))

(provide 'pathogen-infection)
