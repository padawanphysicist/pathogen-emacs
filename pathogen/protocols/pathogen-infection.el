(require 'pathogen-dna)

(cl-defun pathogen-create-germ (germ-spec &key vars enabled deps)
  "Parse GERM-SPEC and return a registered `pathogen-germ' instance."
  (let* ((name (cond ((and (listp germ-spec) (plist-get germ-spec :name))
                     (plist-get germ-spec :name))
                    (t germ-spec)))
         (final-vars (or vars (and (listp germ-spec) (plist-get germ-spec :variables))))
         (final-deps (or deps (and (listp germ-spec) (plist-get germ-spec :dependencies))))
         (is-enabled (if (listp germ-spec)
                         (if (plist-member germ-spec :enabled-p) 
                             (plist-get germ-spec :enabled-p) t)
                       (if (null enabled) t enabled)))
         (path (pathogen--find-germ-path name)))
    
    (if (not path)
        (progn (warn "✗ Germ not found in laboratory: %s" name) nil)
      
      (let ((instance (pathogen-germ :name (if (stringp name) (intern name) name)
                                    :path path
                                    :variables final-vars
                                    :dependencies final-deps
                                    :enabled-p is-enabled)))
        ;; Register in the Genome (from dna.el)
        (pathogen-dna-register instance)
        instance))))
