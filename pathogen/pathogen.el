(let ((default-directory (file-name-directory load-file-name)))
  (add-to-list 'load-path (expand-file-name "core" default-directory))
  (add-to-list 'load-path (expand-file-name "protocols" default-directory)))

(defvar pathogen--genome (make-hash-table :test 'equal)
  "The global registry of all discovered Germs.")


(require 'pathogen-dna)
(require 'pathogen-sequence)
(require 'pathogen-infection)
(require 'pathogen-quarantine)
(require 'pathogen-microscope)

(defvar pathogen-config-file
  (let ((env-path (getenv "PATHOGEN_CONFIG_FILE")))
    (expand-file-name (or env-path "~/.pathogen.el")))
  "The file containing the pathogen-layers! declaration.")

(defun pathogen-propagate ()
  "Express germs with deep lookup logging."
  (let ((order (pathogen-sequence-dna)))
    (dolist (germ-name order)
      (let ((germ (pathogen-dna-get germ-name)))
        (message "[Pathogen Trace] Attempting lookup for: %S -> Found: %s" 
                 germ-name (if germ "YES" "NO"))
        (if germ
            (pathogen-infect germ)
          (warn "[Pathogen] Ghost Germ detected: %S" germ-name))))))

;(defun pathogen-propagate ()
;  "Sequence and express all enabled Germs, with safety checks."
;  (let ((order (pathogen-sequence-dna)))
;    (dolist (germ-name order)
;      (let ((germ (pathogen-dna-get germ-name)))
;        (if (null germ)
;            (warn "[Pathogen] Ghost Germ detected: %s is in the sequence but not the registry!" germ-name)
;          (pathogen-infect germ))))
;    
;    ;; Trigger Report if failures occurred
;    (when pathogen-quarantine-list
;      (pathogen-display-quarantine-report))))

;(defun pathogen-propagate ()
;  "Sequences the DNA and infects the host with safety checks."
;  (interactive)
;  (setq pathogen-quarantine-list nil) ; Reset quarantine for reload
;  (let ((order (pathogen-sequence-dna)))
;    (dolist (germ order)
;      (let ((name (pathogen-germ-name germ)))
;        (if (pathogen-quarantine-check-deps germ)
;            (condition-case err
;                (unless (pathogen-infect germ)
;                  (push name pathogen-quarantine-list))
;              (error
;               (warn "[Pathogen] Infection failed for %s: %s" name err)
;               (push name pathogen-quarantine-list)))
;          (warn "[Pathogen] Quarantining %s (Dependency failure)" name)
;          (push name pathogen-quarantine-list)))))
;  ;; Auto-show report if something is quarantined
;  (when pathogen-quarantine-list
;    (pathogen-microscope)))

;(defun pathogen-propagate ()
;  "Sequence and express all enabled Germs, then check for failures."
;  (let ((order (pathogen-sequence-dna)))
;    (dolist (germ-name order)
;      (let ((germ (pathogen-dna-get germ-name)))
;        (pathogen-infect germ)))
;    
;    ;; Trigger Alert if needed
;    (if pathogen-quarantine-list
;        (progn
;          (message "[Pathogen] Infection incomplete. %d germs quarantined." 
;                   (length pathogen-quarantine-list))
;          (pathogen-display-quarantine-report))
;      (message "[Pathogen] Host fully infected. System stable."))))

;; 4. Trigger the External Configuration
(defun pathogen-load-config ()
  "Load the layers defined in the PATHOGEN_CONFIG_FILE."
  (if (file-exists-p pathogen-config-file)
      (progn
        (message "[Pathogen] Loading configuration from: %s" pathogen-config-file)
        (load pathogen-config-file nil 'nomessage))
    (warn "[Pathogen] Configuration file not found: %s" pathogen-config-file)))

(provide 'pathogen)
;;; pathogen.el --- Final Assembly
