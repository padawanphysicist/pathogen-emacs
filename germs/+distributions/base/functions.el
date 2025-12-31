(defun +distributions/base-make-scratch-unkillable ()                                         
  "Prevents the *scratch* buffer from being killed."                                
  (if (not (equal (buffer-name) "*scratch*"))                                       
      t                                                                             
    (message "The *scratch* buffer is a vital organ. Cannot kill.")                 
    nil))

(defun +distributions/base-crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
