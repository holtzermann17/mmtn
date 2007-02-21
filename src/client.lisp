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

(in-package mmtn)

(defclass client ()
  ((thread :accessor client-thread)
   (listener-thread :accessor client-listener-thread)
   (socket :reader client-socket :initarg :socket :type usocket)
   (event-queue :reader client-event-queue :initform (make-empty-queue)
		:type queue)
   (need-input-lock :reader client-need-input-lock :initform (make-lock))
   (need-input-p :accessor client-needs-input-p :initform nil)
   (input-handler :accessor client-input-handler :initform nil
		  :type (nullable function))
   (input-queue :reader client-input-queue :initform (make-empty-queue)
		:type queue)
   (listener-done-lock :reader client-listener-done-lock :initform (make-lock))
   (listener-done-p :accessor client-listener-done-p :initform nil)
   (listener-done-condition :reader client-listener-done-condition
			:initform (make-condition-variable))
   (ip-addr :reader client-ip-addr :initarg :ip-addr :type string)))

(defclass event () ()
  (:documentation "The superclass for events."))

(defclass input-event (event)
  ((text :reader input-event-text :initarg :text :type string))
  (:documentation "Indicates that the user has sent input."))

(defclass stop-event (event)
  ((client :reader stop-event-client :initarg :client :type client)))

(defgeneric process-event (event)
  (:documentation "Processes an event."))

(defgeneric needs-to-know-p (client event)
  (:documentation "True if the given client must recieve the given broadcast
event. False otherwise."))

(defmethod needs-to-know-p (client event)
  "The default implementation for NEEDS-TO-KNOW-P always returns T."
  t)

;; XXX: Using a list for the clients makes for rather inefficient
;; removal. This may have to be changed.
(defvar %*clients* ()
  "A list of all clients.")

(defvar %*clients-lock* (make-lock)
  "The lock for %*clients*.")

(defvar *current-client* nil
  "The current client. Only defined inside a client thread.")

(defun add-client (sock)
  "Adds a new client connected to the given socket."
  (let ((client (make-instance
		 'client :socket sock
		 :ip-addr (format-ip-addr (get-peer-address sock)))))
    (message :info "New client: ~A" (client-ip-addr client))
    (with-lock-held (%*clients-lock*) (push client %*clients*))
    (setf (client-listener-thread client)
	  (make-thread-with-standard-specials
	   (fn (run-client-listener client))
	   :name (format nil "mmtn-client-listener-~A"
			 (client-ip-addr client))))
    (setf (client-thread client)
	  (make-thread-with-standard-specials
	   (fn (let ((*current-client* client))
		 (run-client)))
	   :name (format nil "mmtn-client-~A"
			 (client-ip-addr client))))
    client))

(defun remove-client (&optional (client *current-client*))
  "Shuts down the given client."
  (send-event (make-instance 'stop-event :client client) client))

(defmethod process-event ((event stop-event))
  (when (eq *current-client* (stop-event-client event))
    (let ((client *current-client*))
      (with-lock-held ((client-listener-done-lock client))
	(destroy-thread (client-listener-thread client))
	(loop do (condition-wait (client-listener-done-condition client)
				 (client-listener-done-lock client))
	      until (client-listener-done-p client)))
      (socket-close (client-socket client))
      (locked-setf %*clients-lock* %*clients*
		   (delete client (the list %*clients*) :test #'eq))
      (message :info "Client ~A disconnected" (client-ip-addr client))
      ;; XXX: Remove this, figure out some other way to signal termination.
      (sb-ext:quit))))

(defun for-each-client (fun)
  (declare (function fun))
  "Calls a given function on each client."
  (mapc fun (with-lock-held (%*clients-lock*) (copy-list %*clients*))))

(defun run-client ()
  "Runs the client event loop."
  ;; This is defined in protcol.lisp.
  (client-main)
  (loop (let ((event (blocking-dequeue (client-event-queue *current-client*))))
	  (process-event event))))

(defun send-event (event &optional (client *current-client*))
  "Sends an event to a single client: the current, by default."
  (enqueue (client-event-queue client) event))

(defun broadcast-event (event)
  "Sends an event to all clients that satisfy (NEEDS-TO-KNOW-P CLIENT EVENT)."
  XXX)

(defun run-client-listener (client)
  "Listens on a client's socket and sends input events."
  (let ((input-stream (socket-stream (client-socket client))))
    (unwind-protect
	 (loop named main-loop
	       do (let ((line (read-line input-stream nil)))
		    (if (null line)
			(progn (remove-client client)
			       ;; Just do nothing until we're
			       ;; interrupted and told to exit.
			       (loop (sleep 1)))
			(if (with-lock-held ((client-need-input-lock client))
			      (client-needs-input-p client))
			    (send-event (make-instance 'input-event :text line)
					client)
			    (enqueue (client-input-queue client) line)))))
      (locked-setf (client-listener-done-lock client)
		   (client-listener-done-p client) t)
      (condition-notify (client-listener-done-condition client)))))

(defmethod process-event ((event input-event))
  (with-lock-held ((client-need-input-lock *current-client*))
    (setf (client-needs-input-p *current-client*) nil))
  (funcall (the function (client-input-handler *current-client*))
	   (input-event-text event)))

(defmacro with-client-input (var &body body)
  "Reads a line from the client and executes BODY with the string
bound to VAR. Abstracts out all of the event handling
nonsense. ACHTUNG! This should ALWAYS ALWAYS ALWAYS be called
tail-recursively so that flow control immediately returns to the event
loop from the event handler. In other words, it must be the last
expression in the function, and all calls above the function must be
tail calls. Life would be simpler if we had continuations."
  ;; Okay, so you want to know what will happen if you don't return to
  ;; the event loop after calling this? Fine, I'll tell you. The body
  ;; won't be executed, and things will not work at all. Then the body
  ;; will executed when you return to the event loop and input
  ;; arrives. Avoid these wacky semantics, and just don't do it.
  `(%with-client-input #'(lambda (,var) ,@body t)))

(defun %with-client-input (fun)
  (declare (function fun))
  (let ((input-lock (client-need-input-lock *current-client*)))
    (acquire-lock input-lock)
    (aif (dequeue (client-input-queue *current-client*))
	 (progn (release-lock input-lock)
		(funcall fun it))
	 (progn (setf (client-needs-input-p *current-client*) t)
		(setf (client-input-handler *current-client*) fun)
		(release-lock input-lock)
		nil))))

;; XXX: I don't remember what this is for, but it looks useful.
(defmacro def-input-processor (name input-var function-var lambda-list
			       &body body)
  "Defines a new input processor macro."
  (let ((function-name (intern (format nil "%~A" (symbol-name name))
			       (symbol-package name)))) 
  `(progn
    (defmacro ,name (var ,@lambda-list &body body)
      `(,',function-name #'(lambda (,var) ,@body)))
    (defun ,function-name ,(cons function-var lambda-list)
      (declare (function ,function-var))
      (with-client-input ,input-var ,@body)))))

(defun client-message (format &rest args)
  "Sends a message to the client from a client thread."
  (let ((stream (socket-stream (client-socket *current-client*))))
    (apply #'format stream format args)
    (finish-output stream)))
