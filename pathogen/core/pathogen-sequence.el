;;; sequence.el --- Genomic Sequencing & DAG Logic -*- lexical-binding: t; -*-
(require 'pathogen-dna)

(defun pathogen-sequence-dna ()
  "Return germ names sorted by their dependency requirements."
  (let ((sorted nil)
	(visited (make-hash-table :test 'equal)))
    (cl-labels ((visit (name)
		       (progn
			 (when (eq (gethash name visited) 'busy)
			   (error "🧬 Circular DNA detected! Germ %s depends on itself via a loop" name)))
		       (let ((germ (pathogen-dna-get name)))
			 (when (and germ (not (gethash name visited)))
			   (puthash name 'busy visited)
			   ;; Recursively visit all dependencies first
			   (dolist (dep (pathogen-germ-dependencies germ))
			     (visit dep))
			   (puthash name 'done visited)
			   (push name sorted)))
		       ))
	       ;; Iterate over all germs in the genome
	       (maphash (lambda (name _) (visit name)) pathogen--genome)
	       (reverse sorted))))

;(defun pathogen-sequence-dna ()
;  "Return a list of Germ NAMES (symbols) currently in the genome."
;  (let (names)
;    (maphash (lambda (name _germ-object) 
;               (push name names)) 
;             pathogen--genome)
;    names))

;(defun pathogen-sequence-dna ()
;  "Returns a linear list of germ objects in the correct load order."
;  (let (sorted-list temp-marks permanent-marks)
;    (cl-labels ((visit (node-name)
;                  (let ((germ (pathogen-dna-get node-name)))
;                    (when (memq node-name temp-marks)
;                      (error "Circular dependency: %s" node-name))
;                    (when (and germ (not (memq node-name permanent-marks)))
;                      (push node-name temp-marks)
;                      (dolist (dep (pathogen-germ-dependencies germ))
;                        (visit dep))
;                      (pop temp-marks)
;                      (push node-name permanent-marks)
;                      (push germ sorted-list)))))
;      (maphash (lambda (name _obj) (visit name)) pathogen--genome)
;      (reverse sorted-list))))

(provide 'pathogen-sequence)
