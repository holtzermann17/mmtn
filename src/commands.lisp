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

;; Implements the command interpreter.

(in-package mmtn)

(defclass command ()
  ((arg-spec :initarg :arg-spec :reader command-arg-spec :type list)
   (function :initarg :function :reader command-function :type function)
   (documentation :initarg :documentation :reader command-documentation
		  :type string)))

(defclass parser ()
  ((name :initarg :name :reader parser-name :type symbol)
   (function :initarg :function :reader parser-function :type function)))

(defparameter *command-table* (make-hash-table :test 'equal)
  "Maps command names to COMMAND objects.")

(defparameter *parsers* (make-hash-table :test 'eq)
  "Maps parser names to PARSER objects.")

(defmacro defcommand (name arg-spec documentation &body body)
  "Defines a new command."
  (validate-arg-spec arg-spec)
  (let ((lambda-list (mapcar #'car arg-spec)))
    `(setf (gethash ,name *command-table*)
	    (make-instance 'command
	     :arg-spec ',arg-spec
	     :function #'(lambda ,lambda-list ,@body)
	     :documentation ,documentation))))

(defun validate-arg-spec (arg-spec)
  "Raises an error if the given ARG-SPEC is invalid."
  (mapc (fna (if (not (listp a))
		 (error "Each element of the arg-spec must be of the
form (NAME PARSER). You gave: ~S." a))
	     (if (not (symbolp (car a)))
		 (error "The car of the arg-spec must be an argument name."))
	     (if (not (gethash (cdr a) *parsers*))
		 (error "Undefined parser: ~S." a)))
        arg-spec))

(defmacro defparser (name &body body)
  "Defines a new parser."
  `(setf (gethash ,name *parsers*)
    (make-instance 'parser :name ',name
     :function (fn ,@body))))

(defun exec-command (command)
  "Parses and executes a command."
  (with-input-from-string (*standard-input* command)
    (let ((command-name (parse-command-name)))
      (if command-name
	  (let ((command (gethash command-name *command-table*)))
	    (if command
		(apply (command-function command)
		       (parse-command-args (command-arg-spec command)))
		(client-message "I don't know how to ~A.~%" command)))))))

(defun parse-command-name ()
  "Parses and returns a command name. Also chomps whitespace. Returns
nil on an empty line."
  (let ((char (peek-char nil *standard-input* nil)))
    (cond ((null char) nil)
	  ((alphanumericp char)
	   (prog1 (read-until #'whitespace-p)
	     (skip-while #'whitespace-p)))
	  (t (format nil "~C" (read-char))))))

(defun parse-command-args (arg-spec)
  "Parses arguments from *STANDARD-INPUT* according to ARG-SPEC."
  (mapcar (fna (let* ((parser-name (cdr a))
		      (parser (gethash a *parsers*)))
		 (if parser
		     (funcall parser)
		     (error "No such parser: ~A" a))))
	  arg-spec))
