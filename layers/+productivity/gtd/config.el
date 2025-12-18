(use-package org-capture
  :after org
  :ensure nil
  :config
  (add-to-list 'org-capture-templates
               `("i" "Inbox" entry (file pathogen-productivity-gtd/inbox)
                 ,(concat "* TODO %?\n"
			              ":PROPERTIES:\n"
			              ":ID: %(org-id-new)\n"
			              ":CREATED: %U\n"
			              ":END:\n"
                          "%i"))))
(use-package emacs
  :ensure nil
  :bind ("C-c i" . #'pathogen-productivity-gtd/capture-inbox))
