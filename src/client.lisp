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
  ((thread :accessor client-thread :type (nullable sb-thread))
   (listener-thread :accessor client-listener-thread
		    :type (nullable sb-thread))
   (socket :reader client-socket :initarg :socket :type socket)
   (sock-stream :reader client-sock-stream :initarg :sock-stream)
   (event-queue :reader client-event-queue :initform (make-empty-queue)
		:type queue)
   (need-input-mutex :reader client-need-input-mutex
		     :initform (make-mutex) :type mutex)
   (need-input-p :accessor client-needs-input-p :initform nil)
   (input-handler :accessor client-input-handler :initform nil
		  :type (nullable function))
   (input-queue :reader client-input-queue :initform (make-empty-queue)
		:type queue)
   (listener-done-mutex :reader client-listener-done-mutex
			:initform (make-mutex) :type mutex)
   (listener-done-p :accessor client-listener-done-p :initform nil)
   (listener-done-queue :reader client-listener-done-queue
			:initform (make-waitqueue) :type waitqueue)
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

(defvar %*clients-mutex* (make-mutex)
  "The mutex for %*clients*.")

(defvar *current-client* nil
  "The current client. Only defined inside a client thread.")

(defun add-client (sock)
  "Adds a new client connected to the given socket."
  (let ((client (make-instance
		 'client :socket sock
		 :sock-stream (socket-make-stream sock :input t :output t
						  :buffering :line)
		 :ip-addr (format-ip-addr (socket-peername sock)))))
    (message :info "New client: ~A" (client-ip-addr client))
    (with-mutex (%*clients-mutex*) (push client %*clients*))
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
      (with-mutex ((client-listener-done-mutex client))
	(terminate-thread (client-listener-thread client))
	(loop do (condition-wait (client-listener-done-queue client)
			      (client-listener-done-mutex client))
	      until (client-listener-done-p client)))
      (socket-close (client-socket client))
      (mutex-setf %*clients-mutex* %*clients*
		  (delete client (the list %*clients*) :test #'eq))
      (message :info "Client ~A disconnected" (client-ip-addr client))
      (quit))))

(defun for-each-client (fun)
  (declare (function fun))
  "Calls a given function on each client."
  (mapc fun (with-mutex (%*clients-mutex*) (copy-list %*clients*))))

(defun run-client ()
  "Runs the client event loop."
  (client-main)
  (loop (let ((event (blocking-dequeue (client-event-queue *current-client*))))
	  (process-event event))))

(defun client-main ()
  "Starts the client going. This function should be redefined in main.lisp.
The stub definition is in client.lisp."
  ;; This SLEEP is a pretty kludgy way to make sure that things are
  ;; initialized before we take them down.
  (sleep 1)
  (client-message 
"This MUD server has not been properly configured. Please contact the
operators and tell them to fix it. Bye!~%")
  (remove-client))

(defun send-event (event &optional (client *current-client*))
  "Sends an event to a single client: the current, by default."
  (enqueue (client-event-queue client) event))

(defun broadcast-event (event)
  "Sends an event to all clients that satisfy (NEEDS-TO-KNOW-P CLIENT EVENT)."
  XXX)

(defun run-client-listener (client)
  "Listens on a client's socket and sends input events."
  (let ((input-stream (client-sock-stream client)))
    (unwind-protect
	 (loop named main-loop
	       do (let ((line (read-line input-stream nil)))
		    (if (null line)
			(progn (remove-client client)
			       (return-from main-loop))
			(if (with-mutex ((client-need-input-mutex client))
			      (client-needs-input-p client))
			    (send-event (make-instance 'input-event :text line)
					client)
			    (enqueue (client-input-queue client) line)))))
      (mutex-setf (client-listener-done-mutex client)
		  (client-listener-done-p client) t)
      (condition-notify (client-listener-done-queue client)))))

(defmethod process-event ((event input-event))
  (funcall (the function (client-input-handler *current-client*))
	   (input-event-text event)))

(defmacro with-client-input (var &body body)
  "Reads a line from the client and executes BODY with the string bound to
VAR. Abstracts out all of the event handling nonsense. Returns T if the body
was executed, and NIL otherwise."
  `(%with-client-input #'(lambda (,var) ,@body)))

(defun %with-client-input (fun)
  (declare (function fun))
  (let ((input-mutex (client-need-input-mutex *current-client*)))
    (get-mutex input-mutex)
    (aif (dequeue (client-input-queue *current-client*))
	 (progn (release-mutex input-mutex)
		(funcall fun it)
		t)
	 (progn (setf (client-needs-input-p *current-client*) t)
		(setf (client-input-handler *current-client*) fun)
		(release-mutex input-mutex)
		nil))))

(defun client-message (format &rest args)
  "Sends a message to the client from a client thread."
  (apply #'format (client-sock-stream *current-client*) format args))