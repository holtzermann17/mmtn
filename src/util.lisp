;; Monster Mountain server
;; Copyright (C) 2006 Nick Thomas
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of version 2 of the GNU General Public
;; License as published by the Free Software Foundation; the terms of
;; any later version are NOT APPLICABLE.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;; Miscellaneous domain-independent bits and pieces.

(in-package mmtn)

(defmacro fn (&body body)
  "Shortcuts a no-arguments lambda."
  `#'(lambda () ,@body))

;;; XXX-jac: only used in this file
(defmacro fna (&body body)
  "Shortcuts a one-argument lambda."
  `#'(lambda (a) ,@body))

;;; XXX-jac: never used
(defmacro fnab (&body body)
  "Shortcuts a two-argument lambda."
  `#'(lambda (a b) ,@body))

;;; XXX-jac: never used
(defmacro aif (condition then &optional else)
  "Paul Graham's anaphoric IF."
  `(let ((it ,condition))
    (if it ,then ,else)))

(defun format-ip-addr (addr)
  "Formats an IP address in the SB-BSD-SOCKETS format."
  (format nil "~D.~D.~D.~D"
	  (elt addr 0) (elt addr 1) (elt addr 2) (elt addr 3)))

(defun rl ()
  "Reloads the system."
  (asdf:operate 'asdf:load-op :mmtn))

;;; XXX-jac: never used
(deftype nullable (type)
  "Equivalent to (OR NULL TYPE)."
  `(or null ,type))

(defmacro locked-setf (lock &rest args)
  "Acquires a lock, then performa a SETF."
  `(with-lock-held (,lock) (setf ,@args)))

;; XXX: This is left over from when we were using SB-THREAD. Do we
;; still need it?
;; XXX-jac: doesn't look like it; only ever used in this file.
(defmacro make-thread-with-specials (specials fun &key name)
  "Like MAKE-THREAD, except that it causes the bindings of a number of listed
special variables to carry over to the new thread. 25 characters."
  (let ((syms (mapcar (fna (declare (ignore a)) (gensym)) specials)))
    `(let ,(mapcar #'list syms specials)
      (make-thread (fn (let ,(mapcar #'list specials syms) (funcall ,fun)))
       :name ,name))))

;; XXX-jac: only ever used in this file.
(defvar *standard-specials*
  '(*standard-output*)
  "The standard list of special variables that should carry over.")

;; XXX-jac: never used
(defmacro make-thread-with-standard-specials (fun &key name)
  "Like MAKE-THREAD-WITH-SPECIALS, but the specials list is fixed to
*STANDARD-SPECIALS*. 32 characters."
  `(make-thread-with-specials ,*standard-specials* ,fun :name ,name))

;; XXX-jac: never used
(defun whitespace-p (char)
  "True if the given character is whitespace. False otherwise."
  (or (eq #\Space char) (eq #\Tab char) (eq #\Return char) (eq #\Newline char)))

;; XXX-jac: all of the following functions are not used outside of
;; this file.

(defun read-while (predicate &optional (stream *standard-input*))
  "Reads characters from STREAM while they satisfy PREDICATE. Stops at EOF."
  (let ((string-buf (make-array '(10) :element-type 'character :adjustable t
				:fill-pointer 0)))
    (loop (let* ((char (peek-char nil stream nil)))
	    (cond ((or (null char) (not (funcall predicate char)))
		   (return (concatenate 'string string-buf)))
		  (t (vector-push-extend (read-char stream) string-buf)))))))

(defun skip-while (predicate &optional (stream *standard-input*))
  "Discards characters from STREAM while they satisfy PREDICATE. Stops at EOF."
  (loop (let* ((char (peek-char nil stream nil)))
	  (if (or (null char) (not (funcall predicate char)))
	      (return)
	      (read-char stream)))))

(defun read-until (predicate &optional (stream *standard-input*))
  "Same as READ-WHILE, but negates PREDICATE."
  (read-while (complement predicate) stream))

(defun skip-until (predicate &optional (stream *standard-input*))
  "Same as SKIP-WHILE, but negates PREDICATE."
  (skip-while (complement predicate) stream))
