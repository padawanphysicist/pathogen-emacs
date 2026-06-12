;;; pathogen-editing.el --- Text Editing and Transformation -*- lexical-binding: t; -*-

;; Multiple-cursors: Edit multiple lines simultaneously
(use-package multiple-cursors
  :ensure t
  :config
  (defun mc/toggle-cursor-at-point ()
    "Add or remove a fake cursor at the current point."
    (interactive)
    (if multiple-cursors-mode
        (message "Cannot toggle cursors while `multiple-cursors-mode` is active.")
      (let ((existing (mc/fake-cursor-at-point)))
        (if existing
            (mc/remove-fake-cursor existing)
          (mc/create-fake-cursor-at-point)))))
  
  (add-to-list 'mc/cmds-to-run-once 'mc/toggle-cursor-at-point)
  (add-to-list 'mc/cmds-to-run-once 'multiple-cursors-mode)
  
  :bind (("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-S-SPC"       . mc/toggle-cursor-at-point)
         ("<C-S-return>"  . multiple-cursors-mode)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-c C-S-c"   . mc/edit-lines)
         ("C-c C->"       . mc/skip-to-next-like-this)
         ("M-p"           . mc/skip-to-previous-like-this)))

;; Visual-regexp-steroids: Visual feedback for regex replace
(use-package visual-regexp-steroids
  :ensure t  
  :bind (("C-c q" . vr/query-replace)))

(provide 'pathogen-editing)
;;; pathogen-editing.el ends here
