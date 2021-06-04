;;; 04-setup-keybindings.el --- Basic keybindings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Victor Santos
;;
;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define leader-key 
;;
(defmacro pathogen|declare-prefix (prefix name)
  "Convenience macro for defining/modifying prefixes."
  `(pathogen-leader-def
    ,prefix (list :ignore t :which-key ,name)))

(defmacro pathogen|set-key (binding func &rest rest)
  `(pathogen-leader-def
    ,binding
    ,(if rest
         `(list ,func ,@rest)
       func)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main prefixes

;; help
(pathogen|declare-prefix "h" "help")
(pathogen|declare-prefix "hd" "describe")
(pathogen|set-key "hdf" 'describe-function :wk "function")
(pathogen|set-key "hdv" 'describe-variable :wk "variable")
(pathogen|set-key "hdk" 'describe-key :wk "key")
(pathogen|set-key "hdd" 'describe-bindings :wk "bindings")
(pathogen|set-key "hdm" 'describe-mode :wk "mode")
(pathogen|set-key "hdg" 'general-describe-bindings :wk "general-bindings")

;; file
(pathogen|declare-prefix "f" "file")
(pathogen|set-key "ff" 'helm-find-files :wk "find")
(pathogen|set-key "fs" 'save-buffer :wk "save")
(pathogen|set-key "fr" 'helm-recentf :wk "recent")
	
;; buffer
(pathogen|declare-prefix "b" "buffer")
(pathogen|set-key "bb" 'helm-mini :wk "switch")
(pathogen|set-key "bd" 'kill-buffer-and-window :wk "delete")

;; window
(pathogen|declare-prefix "w" "window")
(pathogen|set-key "wm" 'delete-other-windows :wk "maximize")
(pathogen|set-key "wv" 'split-window-right :wk "vertical split")
(pathogen|set-key "ws" 'split-window-below :wk "horizontal split")
(pathogen|set-key "wd" 'delete-window :wk "delete")
(pathogen|set-key "wu" 'winner-undo :wk "undo")
(pathogen|set-key "wr" 'winner-redo :wk "redo")
;; Window moving by number (TODO create function for this!)
(push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
      which-key-replacement-alist)
(push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
(pathogen-leader-def "0" 'winum-select-window-0)
(pathogen-leader-def "1" 'winum-select-window-1)
(pathogen-leader-def "2" 'winum-select-window-2)
(pathogen-leader-def "3" 'winum-select-window-3)
(pathogen-leader-def "4" 'winum-select-window-4)
(pathogen-leader-def "5" 'winum-select-window-5)
(pathogen-leader-def "6" 'winum-select-window-6)
(pathogen-leader-def "7" 'winum-select-window-7)
(pathogen-leader-def "8" 'winum-select-window-8)
(pathogen-leader-def "9" 'winum-select-window-9)
(pathogen|set-key "wl" 'evil-window-right)
(pathogen|set-key "wh" 'evil-window-left)
(pathogen|set-key "wk" 'evil-window-up)
(pathogen|set-key "wj" 'evil-window-down)

;; toggle
(pathogen|declare-prefix "t" "toggle")
(pathogen|set-key "tn" 'display-line-numbers-mode :wk "show line numbers")
(pathogen|set-key "tw" 'whitespace-mode :wk "show whitespace")
(pathogen|set-key "tl" 'visual-line-mode :wk "show visual lines")

;; quit
(pathogen|declare-prefix "q" "quit")
(pathogen|set-key "qq" 'save-buffers-kill-terminal :wk "save and quit")

;; Jumping
(pathogen|declare-prefix "j" "jump")
(pathogen|set-key "jj" 'avy-goto-char-timer :wk "to char")
   
;; Following Spacemacs, this prefix should be reserved to the user only
(pathogen|declare-prefix "o" "user")

(provide '04-setup-keybindings)
;;; 04-setup-keybindings.el ends here
