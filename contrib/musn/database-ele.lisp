;; database-ele.lisp -- wrapping the Elephant database for MUSN
;; Copyright (C) 2007 Joseph Corneli <jcorneli@planetmath.org>
;; Copyright wholly transfered to the public domain in the USA and
;; worldwide wherever this is possible.  Contact me for other
;; arrangements if necessary.

;;; Documentation

;; This code stores a collection of Things, some of which are Triples.
;; Things are kept unique by contents and have unique id numbers, but
;; they are otherwise arbitrary.  Triples refer to the ids of Things.

;; You can look Things up by value or by id number:

; (get-value "foo" *things*)
; (get-value 5 *by-id*)

;; You can look up Triples matching certain criteria, e.g. use
;; (match-triples-beginning 5) find those Triples whose beginning slot
;; is 5.

;;; Questions: 

;; If we make lots of attachments (e.g. thing of a million Triples all
;; asserting "/foo/ is an article"), then we might be better served by
;; working with classes and indexing them.  (Quoting from the elephant
;; manual: "what if we have thousands of friends? Aside from never
;; getting work done, our get-instances-by-class will be doing a great
;; deal of consing, eating up lots of memory and wasting our
;; time. Fortunately there is a more efficient way of dealing with all
;; the instances of a class." -- I don't see the definition of this
;; function anywhere, but it sounds bad, and we want to avoid things
;; like that.)

;;; Preliminaries

(asdf:operate 'asdf:load-op :elephant)

(defpackage elephant-autoindex (:use :cl :elephant))
(in-package :elephant-autoindex)

(elephant:open-store '(:bdb "/home/joe/musndb")) ; OR -
; on my machine at home:
(elephant:open-store '(:bdb "/Users/jcorneli/musndb"))

;; And to recover after a crash or otherwise, do
(elephant:open-store '(:bdb "/home/joe/musndb") :recover t) ; OR -
(elephant:open-store '(:bdb "/Users/jcorneli/musndb") :recover t)

;;; Things

;; DESIGN NOTE: I want to key this class by the data -- since I want
;; that data to be unique -- and store id numbers as a secondary slot.
;; However, since I am storing the data in the key, it seems a little
;; strange to *also* have the data present in the value.  Maybe the
;; only value that is needed is the id number?  I don't know what
;; makes for good style.  One problem is with look-up; see `nl-triple'
;; for something that would need a work-around.  (Furthermore, it
;; might be nice if I could reliably use things built-in in Elephant
;; to keep track of id numbers, e.g. see "Class Indices" in the
;; documentation.)
(defclass thing ()
  ((data :accessor thing-data :initarg :data)
   (id :accessor thing-id :initarg :id))
  (:metaclass persistent-metaclass))

;; so, this is how the indexed b-tree is created -- but how is it
;; retrieved in a new LISP session?
(defvar *things* (with-transaction () (make-indexed-btree)))

;; NOTE: This keeps Things unique by name - and if a name exists, then
;; it has a fixed id number forever.  There is an alternative
;; possibility, namely to keep things unique by id number.  I'm not
;; sure it matters much.
(defun add-thing (data)
  "Add a new Thing to the permanent collection and return this Thing.
Things are also kept unique by their DATA.  Each Thing will have a
unique id number, generated when it is created by incrementing the
current thing-autoindex.  If adding fails because the Thing is already
present in the store, return nil."
  (with-transaction ()
    (let ((extant-data (get-value data *things*)))
      (unless extant-data
        (let ((id (add-to-root "thing-autoindex"
                               (1+ (get-from-root "thing-autoindex")))))
          (setf (get-value data *things*)
                (make-instance 'thing :data data :id id)))))))

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

(defvar *by-id* (get-index *things* 'by-id))

;;; Triples

;; Note: right now you can store anything in these triples.  Later it
;; would probably enhance "best practices" to insist that people just
;; store *numbers* in them, and that these numbers will point at
;; Things (FYI, this is what I always do in practice with the
;; functions below).

(defclass triple ()
  ((beginning  :accessor triple-beginning :initarg :beginning)
   (middle     :accessor triple-middle    :initarg :middle)
   (end        :accessor triple-end       :initarg :end))
  (:metaclass persistent-metaclass))

(defun add-triple (begid midid endid)
  "Add a new Triple [BEGID MIDID ENDID] to the collection of Things.
Return this Triple as a Thing."
  (add-thing (make-instance 'triple :beginning begid :middle midid :end endid)))

;;; Indexes for selecting triples according to their constituents.

(add-index *things* :index-name 'triples-beginning
                    :key-form '(lambda (index k v)
                                (declare (ignore index v))
                                (if (subtypep (type-of k) 'triple) 
                                    (values t (triple-beginning k))
                                    (values nil nil)))
                    :populate t)

;; This is much more succinct now that there is a `:collect' keyword.
(defun match-triples-beginning (beginning)
  (map-index (lambda (k v pk) (declare (ignore k pk)) v)
             (get-index *things* 'triples-beginning)
             :value beginning
             :collect t))

(defun match-triples-beginning-nl (beginning)
  (match-triples-beginning (thing-id (get-value beginning *things*))))

(add-index *things* :index-name 'triples-middle
                    :key-form '(lambda (index k v)
                                (declare (ignore index v))
                                (if (subtypep (type-of k) 'triple)
                                    (values t (triple-middle k))
                                    (values nil nil)))
                    :populate t)

(defun match-triples-middle (middle)
  (map-index (lambda (k v pk) (declare (ignore k pk)) v)
             (get-index *things* 'triples-middle)
             :value middle
             :collect t))

(defun match-triples-middle-nl (middle)
  (match-triples-middle (thing-id (get-value middle *things*))))

(add-index *things* :index-name 'triples-end
                    :key-form '(lambda (index k v)
                                (declare (ignore index v))
                                (if (subtypep (type-of k) 'triple)
                                    (values t (triple-end k))
                                    (values nil nil)))
                    :populate t)

(defun match-triples-end (end)
  (map-index (lambda (k v pk) (declare (ignore k pk)) v)
             (get-index *things* 'triples-end)
             :value end
             :collect t))

(defun match-triples-end-nl (end)
  (match-triples-end (thing-id (get-value end *things*))))

;; I don't know a good function for indexing *any* slot, or any
;; particular *combo* of slots; but if I did, I would write functions
;; like `match-triples-containing' and `match-triples--middle-end'
;; etc.

;;; Utility

(defun format-triple (triple)
  (format nil "[~a ~a ~a]" 
          (triple-beginning triple) (triple-middle triple) (triple-end triple)))

;; This function is written using `get-value', and I don't know if
;; there is any corresponding `get-key' or any way to get the key when
;; I look things up using `*by-id*'.  You'd think there might be,
;; given that I was able to get ahold of key data with functions like
;; `match-triples-end' etc.
(defun nl-triple (triple)
  (let ((beg (thing-data (get-value (triple-beginning triple) *by-id*)))
        (mid (thing-data (get-value (triple-middle triple) *by-id*)))
        (end (thing-data (get-value (triple-end triple) *by-id*))))
    (format nil "[~a ~a ~a]" 
            (printable-triple-or-data beg)
            (printable-triple-or-data mid)
            (printable-triple-or-data end))))

;; Printing is done in a mildly recursive way so that analogies
;; between triples are pretty-printed.
(defun printable-triple-or-data (data)
  (cond ((eq (type-of data) 'triple)
         (nl-triple data))
        (t data)))

;; Not sure about the return value... 
(defun print-things (things)
  "THINGS is a list of things to print."
  (map nil (lambda (thing) (format t "~a/~a~%"
                                   (printable-triple-or-data (thing-data thing))
                                   (thing-id thing)))
       things))

(defun print-everything ()
  (map-btree (lambda (k v) (format t "~a | ~a/~a~%" 
                                   k
                                   (printable-triple-or-data (thing-data v))
                                   (thing-id v)))
             *things*))

;;; More convenient way to add triples.

;; (It might be nice to have a mixed-modality way to add triples,
;; combining id numbers and other things.  I suppose one aesthetic
;; interpretation of that way of doing things is that id numbers are
;; "already in the store", even thought that is, of course, a
;; convenient fiction -- we could obviously have a *number* in the
;; store, and this number would very likely have some *other* id
;; number.)
(defun add-triple-nl (beginning middle end)
  "Add the triple corresponding to [BEGINNING MIDDLE END]." 
  (let ((begid (thing-id (or (get-value beginning *things*)
                             (add-thing beginning))))
        (midid (thing-id (or (get-value middle *things*)
                             (add-thing middle))))
        (endid (thing-id (or (get-value end *things*)
                             (add-thing end)))))
    (add-triple begid midid endid)))

;;; Theories

;; I want some convenient way to add (and then work with respect to)
;; some new btree, instead of the global store.  This will allow me to
;; make "little theories" with their own sets of triples.  

;; So, I will assert the following : A Theory is just a collection of
;; Things, some of which may be Triples.  It will be convenient to
;; place such a collection of Things into a btree.

;; This probably means that all of the functions that went before will
;; have to take an optional Theory argument; either that, or perhaps I
;; can write an `in-theory' or `with-theory' macro that will reroute
;; the output of the above commands into the named Theory in question.

;; Note, there could be different kinds of currency/commonality
;; between theories, for example, "tom" might refer to the same Tom in
;; Theory A and Theory B, or it might not.  

;; This is the reason for suggesting that we let theories contain
;; things that aren't triples -- but I will want to think about this
;; to be sure: it may be convenient to insist that basic things go
;; into the common store, and only triples go into the theories -- but
;; that seems a bit strange.  On the other hand, a theory might be
;; defined by the assertions it makes about things, not so much by
;; where the things are.  So, if the set of assertions is kept
;; distinct, then the theory would be distinct too.  

;; It does seem to me that we will want to be able to add something to
;; a new large/efficient collection, but that may be the only aspect
;; of a "theory" that needs to be formally encoded at this level.

;;; Persistence (You only want to do this Once.)

;; NOTE: I'm not sure whether the indices `triples-beginning' etc.
;; mentioned above also need to be stored in the root.

;; I can maintain my own count on the number of Things in the
;; database.  As long as this number is strictly increasing, then this
;; counter can be used to maintain uniqueness of ID numbers.
;; Accordingly (you only want to do this One Time):

(add-to-root "thing-autoindex" 0)
(add-to-root '*things* *things*)
(add-to-root '*by-id* *by-id*)