;; Multi-User Semantic Networks (MUSN)
;; Copyright (C) 2007 Joseph Corneli
;; Copyright transfered to public domain.

;; Set up our MMTN sessions to act as a MUSN prompt.

(in-package musn)

(defun welcome ()
  (mmtn::client-message "Welcome to Monster Mountain/MUSN.~%user name: "))

;; at first, we want the interpreter to be in a mode where it reads user
;; names.

;; then once an acceptable user name has been read, we want it to
;; switch into a mode where it evaluates code.

;; I'm not sure how best to do this, since different clients are going
;; to have to be associated with different modes of code evaluation at
;; different points in time, and I don't yet know whether there are
;; any suitable client-local variables in which to store settings.  I
;; can *create* some sort of client-local phenomenon with things like
;; the client-to-user correspondence I was talking about before.

;; Probably there is a handy way to do this with macros, but for now,
;; I'm going to just use a ghastly cond form.

;; Note: I would like to definitely remove clients from this
;; correspondence when clients disconnect.  For now, I suppose I will
;; have to provide a way for users to log out (see musn-user.lisp).

(defvar client-to-user-correspondence nil)

(defun input (input-stream)
  (loop (let ((input (read-line input-stream)))
          (if input
              (mmtn::client-message
               "~S~%MUSN> " 
               (cond
                 ((assoc mmtn::*current-client*
                         client-to-user-correspondence)
                  (handler-case
                      (eval (read
                             (make-string-input-stream input)))
                    (error () "Lisp Error!")))
                 ;; output here is sort of dumb
                 (t (setq client-to-user-correspondence
                          (cons (cons mmtn::*current-client*
                                      input)
                                client-to-user-correspondence)))))
              (progn (mmtn::send-event
                      (make-instance 'stop-event :client mmtn::*current-client*))
                     (loop (sleep 1)))))))