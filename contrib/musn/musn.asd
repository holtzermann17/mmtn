(defsystem "musn"
    :version "0.0.0"
    :author "Joe Corneli <jcorneli@planetmath.org>"
    :licence "Public Domain"
    :depends-on (:mmtn :postmodern)
    :components ((:file "packages")
                 (:file "musn" :depends-on ("packages"))
                 (:file "database" :depends-on ("packages"))
                 (:file "users" :depends-on ("database"))))
