(defun pathogen-productivity-gtd/capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(defun pathogen-productivity-gtd/open-inbox ()
  (interactive)
  (find-file pathogen-productivity-gtd/inbox))
