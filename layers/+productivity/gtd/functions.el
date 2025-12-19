(defun pathogen-productivity-gtd/capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(defun pathogen-productivity-gtd/open-inbox ()
  (interactive)
  (find-file pathogen-productivity-gtd/inbox))

(defun pathogen-productivity-gtd/show-today-scheduled ()
  "Display the Org agenda for the current date only."
  (interactive)
  (let ((org-agenda-span 'day)
        (org-scheduled-past-days 0)
        (org-agenda-start-day ".")) ; "." represents today
    (org-agenda-list)))
