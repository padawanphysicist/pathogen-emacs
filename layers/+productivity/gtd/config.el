(use-package org
  :custom
  (
   (org-log-into-drawer t)
   (org-log-done 'note)
  )
  ;; (org-archive-location (format "%s::* %s" pathogen-productivity-gtd/done pathogen-productivity-gtd/done-heading-title))
  )

(use-package org-capture
  :after org
  :ensure nil
  :config  
  (add-to-list 'org-capture-templates
               `("i" "Inbox" entry (file pathogen-productivity-gtd/inbox)
                 ,(concat "* %?\n"
			              ":PROPERTIES:\n"
			              ":ID: %(org-id-new)\n"
			              ":CREATED: %U\n"
			              ":END:\n"
                          "%i")
                 :prepend t)))
(use-package emacs
  :ensure nil
  :bind ("C-c i" . #'pathogen-productivity-gtd/capture-inbox))
