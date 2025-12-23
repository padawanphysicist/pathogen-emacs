(defun pathogen-productivity-gtd/log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  ;; Use todo keyword as a variable
  (when (and (string= (org-get-todo-state) "PROG") 
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(defun pathogen-productivity-gtd/capture-inbox ()
  (interactive)
  (org-capture nil "i"))

(defun pathogen-productivity-gtd/open-inbox ()
  (interactive)
  (find-file pathogen-productivity-gtd/inbox))

(defun pathogen-productivity-gtd/archive-to-done-file ()
  "Archive the current Org subtree to 'done.org' in the same directory."
  (interactive)
  (let ((org-archive-location
         (format "%s::* %s" pathogen-productivity-gtd/done
                 pathogen-productivity-gtd/done-heading-title)))
    (org-archive-subtree-default)
    (message "Subtree archived to done.org")))

(defun pathogen-productivity-gtd/show-today-scheduled ()
  "Display the Org agenda for the current date only."
  (interactive)
  (let ((org-agenda-files (list pathogen-productivity-gtd/inbox))
        (org-agenda-span 'day)
        (org-scheduled-past-days 7)
        (org-deadline-past-days 7)
        (org-agenda-start-day ".")) ; "." represents today
    (org-agenda-list)))

(defun pathogen-productivity-gtd/show-tomorrow-scheduled ()
  "Display the Org agenda for the next day only."
  (interactive)
  (let ((org-agenda-files (list pathogen-productivity-gtd/inbox))
        (org-agenda-span 'day)
        (org-scheduled-past-days 7)
        (org-deadline-past-days 7)
        (org-agenda-start-day "+1"))  ; "+1" represents tomorrow
    (org-agenda-list)))
