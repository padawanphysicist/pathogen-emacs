(setq default-family "Inconsolata LGC Nerd Font Mono")
(setq default-variable-pitch-family "Inconsolata LGC Nerd Font Propo")
(setq my-font-presets
      `((regular
         :default-family ,default-family
         :default-height 190
         :variable-pitch-family ,default-variable-pitch-family
         :variable-pitch-height 1.1
         :line-spacing 0.1)))

(infect!
  +bootstrap/package-manager ;; Use default value
  (+core/reflex :variables (:idle-delay 0.2
                            :enable-avy t
                            :enable-snippets t))
  +ui/organoid
  +sys/metabolism
  +sys/homeostasis
  +core/impulses

  +ui/filetree
  +vcs/git
  +virt/docker

  (+ui/dashboard :variables (:show-dashboard t))
  ;;+ui/themes
  ;;(+ui/fonts :variables (list :default-preset 'regular :presets my-font-presets))
  ;;+emacs/better-defaults
  ;;+completion/compleseus
  ;;(+emacs/org :variables (list :base-directory "~/2.areas/org"))
  ;;(+productivity/gtd :variables (list :inbox "~/2.areas/org/gtd/inbox.org" :done "~/2.areas/org/gtd/done.org" ))
  ;;;;+filetree/dirvish
  ;;;;+filetree/neotree
  ;;;;; +pathogen/tabs
  ;;;;+tools/terminal
  ;;;;+productivity/todo
  ;;;;; +productivity/chef
  ;;(+productivity/memex :variables (list :base-directory "~/2.areas/org"))
  ;;;;+virtualization/docker
  ;;;;+lang/markdown
  ;;;;+lang/yaml
  ;;;;(+llm/ollama :variables (list :chat-model "deepseek-coder:6.7b" :embedding-model "all-minilm:33m"))
  ;;;;+source-control/git
 )
