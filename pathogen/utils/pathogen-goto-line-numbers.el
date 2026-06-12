;;; pathogen-goto-line-numbers.el --- Display line numbers only during goto-line -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Victor Santos

;; Author: Victor Santos
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, tools
;; URL: https://codeberg.org/padawanphysicist/pathogen-emacs

;;; Commentary:

;; This package temporarily enables `display-line-numbers-mode` when the
;; `goto-line` command is invoked, and automatically disables it once
;; the line is chosen or the command is canceled.

;;; Code:

;;; pathogen-goto-line-numbers.el --- Display line numbers only during goto-line -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pathogen

;; Author: Pathogen
;; Version: 1.2
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/pathogen/pathogen-goto-line-numbers

;;; Commentary:

;; This package temporarily enables `display-line-numbers-mode` when the
;; `goto-line` command is invoked, and forces it to be disabled once
;; the line is chosen or the command is canceled.

;;; Code:

(defgroup pathogen-goto-line-numbers nil
  "Temporarily display line numbers during `goto-line`."
  :prefix "pathogen-goto-line-numbers-"
  :group 'convenience)

(defun pathogen-goto-line-numbers--wrapped-goto-line ()
  "Enable line numbers during `goto-line` and force disable them afterward."
  (interactive)
  ;; Always ensure line numbers are on when prompting
  (display-line-numbers-mode 1)
  (unwind-protect
      (call-interactively #'goto-line)
    ;; Always disable line numbers after the command finishes or is canceled
    (display-line-numbers-mode -1)))

;;;###autoload
(define-minor-mode pathogen-goto-line-numbers-mode
  "Toggle temporary line numbers during `goto-line`."
  :global t
  :group 'pathogen-goto-line-numbers
  (if pathogen-goto-line-numbers-mode
      (progn
        ;; Disable global line numbers if they are active
        (when (bound-and-true-p global-display-line-numbers-mode)
          (global-display-line-numbers-mode -1))
        ;; Remap the default 'goto-line' to our custom function
        (global-set-key [remap goto-line] #'pathogen-goto-line-numbers--wrapped-goto-line))
    ;; Restore the original mapping when the mode is disabled
    (global-set-key [remap goto-line] nil)))

(provide 'pathogen-goto-line-numbers)
;;; pathogen-goto-line-numbers.el ends here
