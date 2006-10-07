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

(defmacro fna (&body body)
  "Shortcuts a one-argument lambda."
  `#'(lambda (a) ,@body))

(defmacro fnab (&body body)
  "Shortcuts a two-argument lambda."
  `#'(lambda (a b) ,@body))

(defmacro aif (cond then &optional else)
  "Paul Graham's anaphoric IF."
  `(let ((it ,cond))
    (if it ,then ,else)))

(defun format-ip-addr (addr)
  "Formats an IP address in the SB-BSD-SOCKETS format."
  (format nil "~D.~D.~D.~D"
	  (elt addr 0) (elt addr 1) (elt addr 2) (elt addr 3)))

(defun rl ()
  "Reloads the system."
  (asdf:operate 'asdf:load-op :mmtn))

(deftype nullable (type)
  "Equivalent to (OR NULL TYPE)."
  `(or null ,type))

(defmacro mutex-setf (mutex &rest args)
  "Acquires a mutex, then performa a SETF."
  `(with-recursive-lock (,mutex) (setf ,@args)))

(defmacro make-thread-with-specials (specials fun &key name)
  "Like MAKE-THREAD, except that it causes the bindings of a number of listed
special variables to carry over to the new thread. 25 characters."
  (let ((syms (mapcar (fna (declare (ignore a)) (gensym)) specials)))
    `(let ,(mapcar #'list syms specials)
      (make-thread (fn (let ,(mapcar #'list specials syms) (funcall ,fun)))
       :name ,name))))

(defvar *standard-specials*
  '(*standard-output*)
  "The standard list of special variables that should carry over.")

(defmacro make-thread-with-standard-specials (fun &key name)
  "Like MAKE-THREAD-WITH-SPECIALS, but the specials list is fixed to
*STANDARD-SPECIALS*. 32 characters."
  `(make-thread-with-specials ,*standard-specials* ,fun :name ,name))
