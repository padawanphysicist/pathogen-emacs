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

(provide 'pathogen-infection)
