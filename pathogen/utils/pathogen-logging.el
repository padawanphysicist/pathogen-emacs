;;; pathogen-logging.el --- Logging facilities for Pathogen Emacs -*- lexical-binding: t; fill-column: 79; -*-

;; Copyright (C) 2025 Victor Santos

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Package-Requires: ((emacs 27))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides logging facilities and message formatting utilities.
;; It handles log levels, output redirection, and persistent logging
;; for debugging and auditing purposes within the Pathogen Emacs ecosystem.

;;; Code:

(defvar pathogen-log-list '()
  "A list of logs where each entry is (timestamp level message).")

(defun pathogen-get-propertized-level (level)
  "Return the LEVEL string with appropriate faces."
  (cond
   ((string= level "ERROR") (propertize level 'font-lock-face '(:foreground "red" :weight bold)))
   ((string= level "WARNING") (propertize level 'font-lock-face '(:foreground "orange")))
   ((string= level "DEBUG") (propertize level 'font-lock-face '(:foreground "grey")))
   ((string= level "INFO")  (propertize level 'font-lock-face '(:foreground "blue")))
   (t level)))

(define-derived-mode pathogen-logging-view-mode tabulated-list-mode "Pathogen-Log-View"
  "Major mode for displaying Pathogen logs."
  (setq tabulated-list-format [("Timestamp" 26 t)
                               ("Level"     10 t)
                               ("Message"   0  f)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun pathogen/display-logs ()
  "Display the contents of `my-log-list` with color coding."
  (interactive)
  (let ((buffer (get-buffer-create "*Pathogen Logs*")))
    (with-current-buffer buffer
      (pathogen-logging-view-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (log)
                      (let ((timestamp (nth 0 log))
                            (level (nth 1 log))
                            (msg (nth 2 log)))
                        (list nil (vector timestamp 
                                          (pathogen-get-propertized-level level) 
                                          msg))))
                    (reverse pathogen-log-list)))
      (tabulated-list-print t))
    (switch-to-buffer buffer)))

(defun pathogen/log (level format-string &rest args)
  "Logs a MESSAGE with a specific severity LEVEL."
  (let* ((msg (apply #'format format-string args))
	 (entry (list (format-time-string "[%Y-%m-%d %H:%M:%S:%3N]")
                     (upcase (symbol-name level))
                     msg)))
    (push entry pathogen-log-list)
    (cond
     ((eq level 'warning) (warn "%s" msg))
     ((eq level 'error) (error "%s" msg)))))

(provide 'pathogen-logging)
;;; pathogen-logging.el ends here
