;;; pathogen-user-config.el --- Load custom user configuration -*- lexical-binding: t; -*-
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
;; Facilities for loading your additional configuration
;;
;;; Code:

(defvar pathogen-user-config-hook nil
  "User defined additional layer.")

(add-hook 'pathogen-user-config-hook
	  (lambda ()
            (when (file-exists-p pathogen/user-config)
                (load pathogen/user-config nil 'nomessage))))

(provide 'pathogen-user-config.el)
;;; pathogen-user-config.el ends here
