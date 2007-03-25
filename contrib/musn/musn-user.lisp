;; user commands for MUSN
;; Copyright (C) 2007 Joseph Corneli
;; Copyright transfered to the public domain.

;; I like the idea of creating a separate package with user-level
;; commands for MUSN.

(in-package musn-user)

;; Note: this is different from simply disconnecting the client.  I
;; don't yet know how to do one simple function that will both log you
;; out and disconnect the client :(.
(defun logout ()
  (setq musn::client-to-user-correspondence
        (remove-if
         (lambda (pair) (equal (car pair)
                               mmtn::*current-client*))
         musn::client-to-user-correspondence)))

(defun users ()
  "Return a list of all logged in users."
  (let (users)
    (dolist (pair musn::client-to-user-correspondence)
      (let ((un (cdr pair)))
      (unless (member un users :test #'equal)
        (setq users (cons un users)))))
    users))

(defun current-user ()
  (cdr (assoc mmtn::*current-client*
              musn::client-to-user-correspondence)))

;; It might be nice to send the message to a different stream or
;; something... wait until the user who is being messaged has finished
;; typing?  At least this doesn't screw up what they are trying to
;; type.
(defun message-user (username message)
  (let ((pairs (remove-if-not
                (lambda (pair) (equal (cdr pair)
                                      username))
                musn::client-to-user-correspondence)))
    (map nil (lambda (pair)
               (mmtn::client-message-1 (car pair) 
                                       (format 
                                        nil "~%~a> ~a~%" (current-user) message)))
         pairs)))