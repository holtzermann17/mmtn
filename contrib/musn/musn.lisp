;; Multi-User Semantic Networks (MUSN)
;; Copyright (C) 2007 Joseph Corneli
;; Copyright transfered to public domain.

;; Set up our MMTN sessions to act as a MUSN prompt.

(in-package musn)

(defun input (input-stream)
  (loop (let ((input (read-line input-stream)))
          (if input
              (mmtn::client-message
               "~S~%" (handler-case
                          (eval (read
                                 (make-string-input-stream input)))
                        (error () "Lisp Error!")))
              (progn (send-event
                      (make-instance 'stop-event :client *current-client*))
                     (loop (sleep 1)))))))