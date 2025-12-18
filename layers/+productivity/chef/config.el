(use-package org-chef
  :ensure t
  :after org-capture
  :config
  (dolist (template
         `(("c" "Cookbook" entry (file pathogen-productivity-chef/recipes)
            "%(org-chef-get-recipe-from-url)"
            :empty-lines 1)
           ("z" "Protocol Cookbook" entry (file pathogen-productivity-chef/recipes)
            "%(org-chef-get-recipe-string-from-url \"%:link\")"
            :empty-lines 1)
           ("m" "Manual Cookbook" entry (file pathogen-productivity-chef/recipes)
            "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n    %?\n** Directions\n\n")))
  (add-to-list 'org-capture-templates template))
  ;; (add-to-list 'org-capture-templates
  ;;       `(("c" "Cookbook" entry (file pathogen-productivity-chef/recipes)
  ;;          "%(org-chef-get-recipe-from-url)"
  ;;          :empty-lines 1)
  ;;         ("z" "Protocol Cookbook" entry (file pathogen-productivity-chef/recipes)
  ;;          "%(org-chef-get-recipe-string-from-url \"%:link\")"
  ;;          :empty-lines 1)
  ;;         ("m" "Manual Cookbook" entry (file pathogen-productivity-chef/recipes)
  ;;          "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))
  )

(elpaca-wait)
