;; Joe, I grant you the copyright on this file. Put your copyright
;; stuff here.

(defsystem "musn"
    :version "0.0.0"
    :author "Joe Corneli <jcorneli@planetmath.org>"
    :licence "Joe, pick a license."
    :depends-on (:mmtn :postmodern)
    :components ((:file "packages")
                 (:file "database" :depends-on ("packages"))))
