;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit
;;
;;
;; https://magit.vc/
;;
;; A Git Porcelain inside Emacs
;; https://github.com/progfolio/elpaca/issues/324
;;
(use-package transient)
(use-package magit
  :after transient
  :bind (("C-x g" . magit-status)))

