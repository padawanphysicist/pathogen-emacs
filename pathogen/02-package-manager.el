;;; 02-package-manager.el --- Setup package manager -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Victor Santos
;;
;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Setup package manager
;;
;;; Code:

(defvar bootstrap-version) ;; For straight.el

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)
  ;; Integrate with use-package
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (setq straight-use-package-by-default t)
  (straight-use-package 'use-package))

(provide '02-package-manager)
;;; 02-package-manager.el ends here
