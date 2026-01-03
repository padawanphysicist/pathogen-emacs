;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; avy
;;
;;
;; https://github.com/abo-abo/avy
;;
;; Easy navigation within buffers
;;
(use-package avy
  :ensure t    
  :bind (("M-j" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)
         ("M-g l" . +reflex/jump-to-line)
         ("M-g M-g" . avy-goto-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual regex search on steroids
;;
;;
;; https://github.com/benma/visual-regexp-steroids.el
;; Deferred: only loads when keybinding is used
;;
(use-package visual-regexp-steroids
  :ensure t  
  :defer t
  :bind
  (("C-c q" . vr/query-replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enable transposing frames
;;
;; https://melpa.org/#/transpose-frame
;;
;; Deferred: only loads when transpose-frame commands are called
;;
(use-package transpose-frame
  :ensure t
  :defer t
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; winum
;;
;;
;; https://github.com/deb0ch/emacs-winum
;;
(use-package winum
  :ensure t  
  :init
  (setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-`") 'winum-select-window-by-number)
      (define-key map (kbd "C-²") 'winum-select-window-by-number)
      (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
      (define-key map (kbd "M-1") 'winum-select-window-1)
      (define-key map (kbd "M-2") 'winum-select-window-2)
      (define-key map (kbd "M-3") 'winum-select-window-3)
      (define-key map (kbd "M-4") 'winum-select-window-4)
      (define-key map (kbd "M-5") 'winum-select-window-5)
      (define-key map (kbd "M-6") 'winum-select-window-6)
      (define-key map (kbd "M-7") 'winum-select-window-7)
      (define-key map (kbd "M-8") 'winum-select-window-8)
      map))
  :config
  (winum-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popwin
;;
;;
;; https://github.com/emacsorphanage/popwin
;;
;; Nice popup management
;;
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit
;;
;;
;; https://magit.vc/
;;
;; A Git Porcelain inside Emacs
(use-package transient :ensure t)
(use-package magit
  :after transient
  :bind (("C-x g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document Viewing
;;
;;
;; https://github.com/politza/pdf-tools
;;
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'pdf-view-mode-hook (lambda () (linum-mode -1)))
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; Hide compilation buffers upon sucess
(defun hide-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without errors or warnings.
The status is passed as STRING. BUFFER is the buffer in question."
  (if (and (string-match-p "finished" string)
           ;; Check for the presence of "error" in the buffer
           (not (with-current-buffer buffer (search-forward-regexp "error" nil t)))
           ;; Optionally, check for "warning" as well
           (not (with-current-buffer buffer (search-forward-regexp "warning" nil t))))
      ;; If successful, bury the buffer and delete its window
      (run-with-timer 0.1 nil (lambda (buf)
                                (bury-buffer buf)
                                (let ((win (get-buffer-window buf 'visible)))
                                  (when win (delete-window win))))
                      buffer)
    ;; If errors/warnings present, simply show a message and keep the buffer open
    (message "Compilation finished with errors or warnings. Check *compilation* buffer.")))

(add-hook 'compilation-finish-functions #'hide-compile-buffer-if-successful)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multiple cursors 
;;
;;
;; https://melpa.org/#/multiple-cursors
;;
(use-package multiple-cursors
  :ensure t
  :config
  (defun mc/toggle-cursor-at-point ()
    "Add or remove a cursor at point."
    (interactive)
    (if multiple-cursors-mode
        (message "Cannot toggle cursor at point while `multiple-cursors-mode' is active.")
      (let ((existing (mc/fake-cursor-at-point)))
        (if existing
            (mc/remove-fake-cursor existing)
          (mc/create-fake-cursor-at-point)))))
  (add-to-list 'mc/cmds-to-run-once 'mc/toggle-cursor-at-point)
  (add-to-list 'mc/cmds-to-run-once 'multiple-cursors-mode)
  :bind (;; Mouse and custom bindings
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-S-SPC" . mc/toggle-cursor-at-point)
         ("<C-S-return>" . multiple-cursors-mode)
         ;; Standard multiple-cursors bindings
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-c C->" . mc/skip-to-next-like-this)
         ("C-c C-<" . mc/skip-to-previous-like-this)))


(provide 'better-editing)
;;; better-editing.el ends here
