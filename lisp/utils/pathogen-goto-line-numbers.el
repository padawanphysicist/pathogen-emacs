;;; pathogen-goto-line-numbers.el --- Display line numbers only during goto-line -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs "27.2"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://codeberg.org/padawanphysicist/pathogen-emacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package temporarily enables `display-line-numbers-mode` when the
;; `goto-line` command is invoked, and automatically disables it once
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
