(add-to-list 'load-path (expand-file-name "pathogen" (file-name-directory load-file-name)))
(require 'pathogen)

;; --- STEP 1: Initialize the Registry ---
;(defvar pathogen--genome (make-hash-table :test 'equal))

(pathogen-load-config)
;(pathogen-propagate)
