;;; init.el --- Emacs main initialization -*- lexical-binding: t; -*-
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
;;  Code that you want to execute when you start Emacs.
;;
;;
;;; Code:

(require 'vars)
(require 'core)
(require 'package-manager)

(pathogen/set-font)
(pathogen/set-smooth-scrolling)
(pathogen/misc-settings)
(pathogen/ui-hooks)
(pathogen/configure-builtin-plugins)
