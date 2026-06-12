;;; pathogen-icr-native.el --- Pure Built-in Completion Substrate -*- lexical-binding: t; -*-

;; Philosophy: "Completion is not a UI widget; it is a programmable substrate."
;; This configuration replaces the VOMPECCC stack using only standard, built-in
;; Emacs features (available in modern Emacs 28, 29, and 30+).

;;; Code:

;; ==========================================================================
;; 1. INTERFACE & MATCHING (Vertico/Orderless Alternatives)
;; ==========================================================================

(use-package emacs
  :init
  ;; Enforce icomplete to display candidates vertically immediately
  (setq icomplete-scroll t)
  
  ;; Enable the native vertical interactive completion UI
  (icomplete-vertical-mode 1)
  
  :custom
  ;; Configure native completion styles to mimic Orderless.
  ;; 'flex' provides fuzzy/out-of-order matching; 'partial-completion' handles paths.
  (completion-styles '(flex partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion flex))))
  
  ;; Case-insensitivity configurations for smooth matching
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :bind
  (:map icomplete-minibuffer-map
        ("<down>" . icomplete-forward-completions)
        ("<up>"   . icomplete-backward-completions)
        ("C-n"    . icomplete-forward-completions)
        ("C-p"    . icomplete-backward-completions)))

;; ==========================================================================
;; 2. METADATA ANNOTATIONS (Marginalia Alternative)
;; ==========================================================================

(use-package emacs
  :custom
  ;; Display rich annotations (docstrings, file permissions) in the minibuffer.
  (completion-detailed 1))


;; ==========================================================================
;; 3. HISTORY & SORTING (Prescient Alternative)
;; ==========================================================================

(use-package savehist
  :init
  ;; Persist minibuffer history across sessions to enable "frecency" sorting.
  (savehist-mode 1)
  :custom
  (history-length 100)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t))


;; ==========================================================================
;; 4. IN-BUFFER COMPLETION (Corfu/Cape Alternatives)
;; ==========================================================================

(use-package emacs
  :custom
  ;; Enable indentation + completion using the TAB key dynamically.
  (tab-always-indent 'complete)
  
  ;; Instruct the standard completion-at-point system to show suggestions.
  (completion-auto-select t)
  (completion-auto-help 'visible)
  
  ;; Keep the completion buffer visible near point without stealing focus window.
  (completion-help-at-point t))


;; ==========================================================================
;; 5. COMMANDS & NAVIGATION (Consult Alternative)
;; ==========================================================================

(use-package emacs
  :bind (
         ;; Modern interactive buffer and bookmark switching
         ("C-x b" . switch-to-buffer)
         ("C-x r b" . bookmark-jump)
         
         ;; Built-in project-aware commands (leveraging project.el)
         ;; Highly efficient alternatives to consult-find and consult-ripgrep
         ("M-s d" . project-find-file)
         ("M-s r" . project-find-regexp)
         ("M-g i" . imenu)
         
         ;; Native Minibuffer History navigation bindings
         :map minibuffer-local-map
         ("M-s" . next-history-element)
         ("M-r" . previous-history-element))
  :config
  ;; Ensure candidate list wraps around cleanly when scrolling
  (setq completion-auto-wrap t))

(provide 'pathogen-icr-native)
;;; pathogen-icr-native.el ends here
