;; users for MUSN
;; Copyright (C) 2007 Joseph Corneli
;; Copyright transfered to the public domain.

(in-package musn)

(defun login ()
  "Prompt someone with a login."
  (format *query-io* "user name: ")
  (force-output *query-io*)
  (let ((user (read-line *query-io*)))
    (unless (relation-present-p user "is a" "user")
      (format t "Unknown user: ~a" user)
      (y-or-n-p "Add to list?"))))

;; this one is going to have to interact with MMTN --
;; in other words, we want to maintain some user-to-client
;; correspondence
(defun list-logged-in-users ())

;; like `client-message' or `log-message'
(defun user-message ())