;; users for MUSN
;; Copyright (C) 2007 Joseph Corneli
;; Copyright transfered to the public domain.

(in-package musn)

(defvar user-client-correspondence nil)

;; Maybe things could be set up so that the `input' function
;; just gives you a MUSN prompt, at which point, if you aren't
;; logged in, it will know that, and as you to log in.  (But
;; that seems like a weird work-around, since I just want to
;; get things set up at the outset.)
(defun login ()
  "Prompt someone with a login.
Just loop forever if they can't supply a valid user-name."
  (mmtn::client-message "user name: ")
  (let ((user (mmtn::client-read)))
    (cond ((relation-present-p user "is a" "user")
           (mmtn::client-message "logging in as ~a~%" user)
           (format t "logging in as ~a~%" user)
           (cons (cons user *current-client*) 
                 user-client-correspondence))
          (t
           (format t "Unknown user \"~a\"~%" user)
           (login)))))

;; this one is going to have to interact with MMTN --
;; in other words, we want to maintain some user-to-client
;; correspondence
(defun list-logged-in-users ())

;; like `client-message' or `log-message'
(defun user-message ())