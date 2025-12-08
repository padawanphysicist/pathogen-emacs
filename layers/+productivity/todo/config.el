;; Highlight TODO and similar keywords in comments and strings
;; https://github.com/tarsius/hl-todo
(use-package hl-todo
  :custom
  ((hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF"))))
  :config
  (global-hl-todo-mode))
