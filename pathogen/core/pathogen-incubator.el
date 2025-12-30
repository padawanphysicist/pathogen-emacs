;;; pathogen-incubator.el --- Dependency resolution and germ loading -*- lexical-binding: t; fill-column: 79; -*-

;; Copyright (C) 2025 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27) (pathogen-logging) (pathogen-germ))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library serves as the orchestration layer for Pathogen Emacs.
;; It performs a topological sort on the germ dependency graph (DAG)
;; to ensure that features are loaded in the correct order.
;;
;; Key features:
;; 1. Cycle detection to prevent infinite loading loops.
;; 2. Automatic instantiation of 'pathogen-germ' objects.
;; 3. The 'infect!' macro, which serves as the primary entry point for 
;;    initializing the user configuration.

;;; Code:

(require 'pathogen-logging)
(require 'pathogen-germ)

;; Topological sort of package DAG
(defun pathogen-incubator--topological-sort-with-cycle-check (data)
  (let ((adj-list (make-hash-table :test 'equal))
        (visited '())
        (stack '())
        (result '()))
    
    ;; 1. Standardize Adjacency List
    (dolist (entry data)
      (let ((node (symbol-name (car entry)))
            ;; Flatten/extract dependencies, filtering out nil
            (deps (seq-filter #'identity (flatten-tree (cdr entry)))))
        (puthash node 
                 (mapcar (lambda (d) (if (symbolp d) (symbol-name d) d)) deps)
                 adj-list)))

    (cl-labels ((visit (node)
                  (cond
                   ((member node stack)
                    (error "Circular dependency detected: %s -> %s" 
                           (mapconcat 'identity (reverse stack) " -> ") 
                           node))
                   ((not (member node visited))
                    (push node stack)
                    (let ((deps (gethash node adj-list)))
                      (dolist (dep deps)
                        (visit dep)))
                    (pop stack)
                    (push node visited)
                    (push node result)))))

      ;; 2. Process all defined nodes
      (dolist (entry data)
        (visit (symbol-name (car entry))))

      (seq-map (lambda (x) (intern x))
      (nreverse result)))))

(defun pathogen-incubator--infect (germs)
  ;; 1. Get list of germ per dependency order
  (let* ((germs-names-given-by-user (seq-map (lambda (x) (if (symbolp x) x (car x))) germs))
	 (germs-names-with-dependencies (seq-map
					 (lambda (x)
					   `(,x ,(seq-map 'intern (pathogen-germ--get-dependencies x))))
					 germs-names-given-by-user))
	 (germs-list (seq-map
		      (lambda (x)
			(if (member x germs-names-given-by-user)
			    (seq-find (lambda (y)
					(if (symbolp y)
					    (eq x y)
					  (equal x (car y)))) germs)
			  x))
		      (pathogen-incubator--topological-sort-with-cycle-check germs-names-with-dependencies))))

    (cl-every #'identity ;; Check all are true
	      (seq-map
	       (lambda (x)
		 (let ((load-status (pathogen-germ--infect x)))
		   (oset x loaded-p load-status)
		   load-status))
	       (seq-map
		(lambda (x)
		  (pathogen-germ
		   :name (if (symbolp x) x (car x))
		   :path (pathogen-germ--get-path x)
		   :dependencies (pathogen-germ--get-dependencies x)
		   :variables (if (symbolp x) nil (cdr x))
		   :enabled-p t
		   :loaded-p nil))
		germs-list)))))

;; The heavy lifter
(defun infect--run (germs)
  (clrhash *pathogen-genome*)
  (let ((infection-status-p (pathogen-incubator--infect germs)))
    (if infection-status-p
	(pathogen/log 'info "Infection completed!")
      (pathogen/log 'error "Infection did not spread correctly. Check `pathogen/display-logs'"))
    infection-status-p))

;; The syntax sugar
(defmacro infect! (&rest germs)
  `(infect--run ',germs))

(provide 'pathogen-incubator)
;;; pathogen-incubator.el ends here
