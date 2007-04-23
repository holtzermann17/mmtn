;;; Preliminaries

(asdf:operate 'asdf:load-op :elephant)

#| Starting with some general questions...

* Can I make a table of triples, where each slot is something
arbitrary, not some specific thing like a "string"?  -- I think the
answer is: yes, just don't specify the :type option.

 - However, it is probably better to make a table of Things, and make
the Triples refer to id numbers of Things.

* Can I make it so that the id number is automatically incremented
when I add things to this particular table of items, like we did with
SQL?

 - Yes; but I have to make the auto-incrementer "by hand".  The same
apparently goes for other SQL features like identifying all of the
entries in a table that match a given predicate.

 |#

;; do I need to specifically list the relid, if that's what I want the
;; btree to be indexed by?
;;
;; -- Maybe what I need to do is put TRIPLES into precisely one slot,
;; and then index that one slot.  I don't think that it is completely
;; necessary for the triples themselves to be persistent, but I will
;; want to check this out.

(defpackage elephant-autoindex (:use :cl :elephant))
(in-package :elephant-autoindex)

(elephant:open-store '(:bdb "/home/joe/musndb")) ; OR -
; on my machine at home:
(elephant:open-store '(:bdb "/Users/jcorneli/musndb"))

;; And to recover after a crash or otherwise, do
(elephant:open-store '(:bdb "/Users/jcorneli/musndb") :recover t)

;;; Counter

;; I can maintain my own count on the number of Things in the
;; database.  As long as this number is strictly increasing, then this
;; counter can be used to maintain uniqueness of ID numbers.
;; Accordingly (you only want to do this One Time):

(add-to-root "thing-autoindex" 0)

;;; Things

;; DESIGN NOTE: I want to key this class by the data -- since I want
;; that data to be unique -- and store id numbers as a secondary slot.
;; However, since I am storing the data in the key, it seems a little
;; strange to *also* have the data present in the value.  Maybe the
;; only value that is needed is the id number?  I don't know what
;; makes for good style.  (Furthermore, it might be nice if I could
;; reliably use things built in to Elephant to keep track of id
;; numbers, e.g. see "Class Indices" in the documentation.)
(defclass thing ()
  ((data :accessor thing-data :initarg :data)
   (id :accessor thing-id :initarg :id))
  (:metaclass persistent-metaclass))

;; so, this is how the indexed b-tree is created -- but how is it
;; retrieved in a new LISP session?
(defvar *things* (with-transaction () (make-indexed-btree)))

(defun add-thing (data)
  "Add a new Thing to the permanent collection and return this Thing.
Things are also kept unique by their DATA.  Each Thing will have a
unique id number, generated when it is created by incrementing the
current thing-autoindex."
  (with-transaction ()
    (let ((id (add-to-root "thing-autoindex"
                           (1+ (get-from-root "thing-autoindex")))))
      (setf (get-value data *things*)
            (make-instance 'thing :data data :id id)))))

;; Setting up lookup by ID number... 

(defun key-by-id (secondary-db primary value)
  (declare (ignore secondary-db primary))
  (let ((id (thing-id value)))
    (when id
      (values t id))))

(with-transaction ()
  (add-index *things*
             :index-name 'by-id
             :key-form 'key-by-id
             :populate t))

; do, for example: (get-value 5 *by-id*)
(defvar *by-id* (get-index *things* 'by-id))

;;; Triples

;; Note: right now you can store anything in these triples.
;; Later it would probably enhance "best practices" to insist
;; that people just store *numbers* in them, and that these
;; numbers will point at Things.  

(defclass triple ()
  ((beginning  :accessor triple-beginning :initarg :beginning)
   (middle     :accessor triple-middle    :initarg :middle)
   (end        :accessor triple-end       :initarg :end))
  (:metaclass persistent-metaclass))

;; Triples are just another kind of Thing.
(defun add-triple (beginning middle end)
  "Add a new Triple to the permanent collection and return this Triple."
  (add-thing (make-instance 'triple :beginning beginning :middle middle :end end)))

;;; Indexes for selecting triples according to their constituents.

(add-index *things* :index-name 'triples-beginning
                    :key-form '(lambda (index k v)
                                (declare (ignore index v))
                                (if (subtypep (type-of k) 'triple) 
                                    (values t (triple-beginning k))
                                    (values nil nil)))
                    :populate t)

(defun match-triples-beginning (beginning)
  (let ((results (list nil)))
    (map-index (lambda (k v pk) 
                 (declare (ignore k pk)) 
                 (setq results (nconc results (list v))))
               (get-index *things* 'triples-beginning)
               :value beginning)
    (cdr results)))

(add-index *things* :index-name 'triples-middle
                    :key-form '(lambda (index k v)
                                (declare (ignore index v))
                                (if (subtypep (type-of k) 'triple)
                                    (values t (triple-middle k))
                                    (values nil nil)))
                    :populate t)

(defun match-triples-middle (middle)
  (let ((results (list nil)))
    (map-index (lambda (k v pk) 
                 (declare (ignore k pk)) 
                 (setq results (nconc results (list v))))
               (get-index *things* 'triples-middle)
               :value middle)
    (cdr results)))

(add-index *things* :index-name 'triples-end
                    :key-form '(lambda (index k v)
                                (declare (ignore index v))
                                (if (subtypep (type-of k) 'triple)
                                    (values t (triple-end k))
                                    (values nil nil)))
                    :populate t)

(defun match-triples-end (end)
  (let ((results (list nil)))
    (map-index (lambda (k v pk) 
                 (declare (ignore k pk)) 
                 (setq results (nconc results (list v))))
               (get-index *things* 'triples-end)
               :value end)
    (cdr results)))

;;; Utility

(defun print-things ()
  (map-btree (lambda (k v) (format t "~a | ~a/~a~%" 
                                   k
                                   (let ((data (thing-data v)))
                                     (cond ((eq (type-of data) 'triple)
                                            (format-triple data))
                                           (t data)))
                                   (thing-id v)))
             *things*))

(defun format-triple (triple)
  (format nil "[~a ~a ~a]" 
          (triple-beginning triple) (triple-middle triple)  (triple-end triple)))