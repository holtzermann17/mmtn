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
  ((server :accessor client-server :initarg :server)
   (thread :accessor client-thread)
   (listener-thread :accessor client-listener-thread)
   (socket :reader client-socket :initarg :socket :type usocket)
   (event-queue :reader client-event-queue :initform (make-empty-queue)
		:type queue)
   (ip-addr :reader client-ip-addr :initarg :ip-addr :type string)
   (exit-requested-p :accessor client-exit-requested-p :initform nil)))

(defclass event () ()
  (:documentation "The superclass for events."))

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

(defvar *current-client* nil
  "The current client. Only defined inside a client thread.")

(defun add-client (server sock)
  "Adds a new client connected to the given socket, running under the
given server."
  (let ((client (make-instance
		 'client :socket sock
		 :ip-addr (format-ip-addr (get-peer-address sock))
                 :server server)))
    (log-message :info "New client: ~A" (client-ip-addr client))
    (with-lock-held ((server-clients-lock server))
      (push client (server-clients server)))
    (setf (client-listener-thread client)
	  (make-thread-with-standard-specials
	   (fn (let ((*current-client* client))
                 (funcall
                  (server-client-input-function (client-server client))
                  (socket-stream (client-socket client)))))
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
    (let* ((client *current-client*)
           (server (client-server client)))
      (with-lock-held ((client-listener-done-lock client))
	(destroy-thread (client-listener-thread client))
	(loop do (condition-wait (client-listener-done-condition client)
				 (client-listener-done-lock client))
	      until (client-listener-done-p client)))
      (socket-close (client-socket client))
      (locked-setf (server-clients-lock server) (server-clients server)
		   (delete client (the list (server-clients server))
                           :test #'eq))
      (log-message :info "Client ~A disconnected" (client-ip-addr client))
      (setf (client-exit-requested-p client) t))))

(defun for-each-client (server fun)
  (declare (function fun))
  "Calls a given function on each client in the given server."
  ;; XXX: This is pretty bad, in terms of scalability.
  (mapc fun (with-lock-held ((server-clients-lock server))
              (copy-list (server-clients server)))))

(defun run-client ()
  "Runs the client event loop."
  (funcall (server-client-main-function (client-server *current-client*)))
  (loop until (client-exit-requested-p *current-client*)
        do (let ((event (blocking-dequeue
                         (client-event-queue *current-client*))))
             (process-event event))))

(defun send-event (event &optional (client *current-client*))
  "Sends an event to a single client: the current, by default."
  (enqueue (client-event-queue client) event))

(defun broadcast-event (event)
  "Sends an event to all clients that satisfy (NEEDS-TO-KNOW-P CLIENT EVENT)."
  XXX)

;; this should be generalized so that it takes the client as argument
;; (see `send-event' above)
(defun client-message (format &rest args)
  "Sends a message to the client from a client thread."
  (let ((stream (socket-stream (client-socket *current-client*))))
    (apply #'format stream format args)
    (finish-output stream)))

(defun client-read ()
  "Read a line coming from the client thread."
  (read-line (socket-stream (client-socket *current-client*))))

(defun client-message-1 (client format &rest args)
  "Sends a message to a given client."
  (let ((stream (socket-stream (client-socket (or client
                                                  *current-client*)))))
    (apply #'format stream format args)
    (finish-output stream)))