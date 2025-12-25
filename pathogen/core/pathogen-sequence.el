;;; sequence.el --- Genomic Sequencing & DAG Logic -*- lexical-binding: t; -*-
(require 'pathogen-dna)

(defun pathogen-sequence-dna ()
  "Returns a linear list of germ objects in the correct load order."
  (let (sorted-list temp-marks permanent-marks)
    (cl-labels ((visit (node-name)
                  (let ((germ (pathogen-dna-get node-name)))
                    (when (memq node-name temp-marks)
                      (error "Circular dependency: %s" node-name))
                    (when (and germ (not (memq node-name permanent-marks)))
                      (push node-name temp-marks)
                      (dolist (dep (pathogen-germ-dependencies germ))
                        (visit dep))
                      (pop temp-marks)
                      (push node-name permanent-marks)
                      (push germ sorted-list)))))
      (maphash (lambda (name _obj) (visit name)) pathogen--genome)
      (reverse sorted-list))))

(provide 'pathogen-sequence)
