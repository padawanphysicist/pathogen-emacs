;;; Completion framework configuration. -*- lexical-binding: t; -*-

(defvar +completion/compleseus-engine 'vertico
  "Default completion engine. (vertico)")

(pathogen--defvars-with-aliases!
 +completion/compleseus
 ;; Vertico
 (vertico-cycle t)
 (read-file-name-completion-ignore-case t)
 (read-buffer-completion-ignore-case t)
 ;; Orderless
 (completion-styles '(orderless))
 (completion-category-defaults nil)
 (completion-category-overrides '((file (styles partial-completion))))
 (orderless-matching-styles '(orderless-initialism orderless-regexp))
 ;; Corfu
 (corfu-auto t)
 (corfu-auto-delay 0.2)
 (corfu-auto-prefix 2)
 (corfu-cycle t)
 (corfu-quit-no-match 'separator)
 (corfu-preview-current nil)
 (corfu-preselect 'prompt))
