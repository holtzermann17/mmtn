;;; Elephant Preliminaries

(asdf:operate 'asdf:load-op :elephant)
(elephant:open-store '(:bdb "/home/joe/musndb"))

;; I can maintain my own count on the number of relations in the
;; database.  As long as this number is strictly increasing, then this
;; counter can be used to maintain uniqueness of ID numbers.
;; Accordingly:

;;; You only want to do this One Time:

(add-to-root "triple-autoindex" 0)

#| Starting with some general questions...

* Can I make a table of triples, where each slot is something
arbitrary, not some specific thing like a "string"?  -- I think the
answer is: yes, just don't specify the :type option.

* Can I make it so that the id number is automatically incremented
when I add things to this particular table of items, like we did with
SQL?

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

;; Note: right now you can store anything in these triples.
;; Later it would probably enhance "best practices" to insist
;; that people just store *numbers* in them, and that these
;; numbers will point at other Things, and Things will be stored
;; in some other location(s).
(defclass triple ()
  ((beginning  :accessor triple-beginning :initarg :beginning)
   (middle     :accessor triple-middle    :initarg :middle)
   (end        :accessor triple-end       :initarg :end))
  (:metaclass persistent-metaclass))

(defvar *triples* (with-transaction () (make-indexed-btree)))

;; things are supposed to be keyed by their ID number...
(defun add-triple (beginning middle end)
  (with-transaction ()
              ; this index will have to be incremented somewhere in this
              ; function
    (let ((id (get-from-root "triple-autoindex")))
      (setf (get-value id *triples*)
            (make-instance 'triple :beginning beginning :middle middle :end end))
      (add-to-root "triple-autoindex" (1+ id)))))

;;; Now some secondary indexes for the three constituents of the
;;; triples:

(defun key-by-beginning (secondary-db primary value)
  (declare (ignore secondary-db primary))
  (let ((beginning (triple-beginning value)))
    (when beginning
      (values t beginning))))

(with-transaction ()
  (add-index *triples*
             :index-name 'by-beginning
             :key-form 'key-by-beginning
             :populate t))

(defvar *by-beginning* (get-index *triples* 'by-beginning))

(defun key-by-middle (secondary-db primary value)
  (declare (ignore secondary-db primary))
  (let ((middle (triple-middle value)))
    (when middle
      (values t middle))))

(with-transaction ()
  (add-index *triples*
             :index-name 'by-middle
             :key-form 'key-by-middle
             :populate t))

(defvar *by-middle* (get-index *triples* 'by-middle))

(defun key-by-end (secondary-db primary value)
  (declare (ignore secondary-db primary))
  (let ((end (triple-end value)))
    (when end
      (values t end))))

(with-transaction ()
  (add-index *triples*
             :index-name 'by-end
             :key-form 'key-by-end
             :populate t))

(defvar *by-end* (get-index *triples* 'by-end))

;;; We are very interested in duplicates, so here is some code to
;;; collect all of the duplicate items for each of the positions:

(defun all-triples-with-beginning (beginning)
  (with-btree-cursor (curs *by-beginning*)
    (loop for (more? k v) =
         (multiple-value-list (cursor-set curs "Birthday"))
         then (multiple-value-list (cursor-next-dup curs))
         do
         (unless more? (return t))
         (multiple-value-bind (s m h d mo y)
             (decode-universal-time (ap-date v))
           (declare (ignore s m h))
           (format t "~A/~A/~A~%" mo d y)))))