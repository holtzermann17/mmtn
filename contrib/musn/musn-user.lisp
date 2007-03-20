;; user commands for MUSN
;; Copyright (C) 2007 Joseph Corneli
;; Copyright transfered to the public domain.

;; I like the idea of creating a separate package with user-level
;; commands for MUSN.

(in-package musn-user)

;; this is different from disconnecting the client.
(defun logout ()
  (setq musn::client-to-user-correspondence
        (remove-if
         (lambda (pair) (equal (car pair)
                               mmtn::*current-client*))
         musn::client-to-user-correspondence)))