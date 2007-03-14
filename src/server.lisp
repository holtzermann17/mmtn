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

(defvar *default-address* #(0 0 0 0)
  "The default IP address for servers to bind to.")

(defvar *servers* (make-hash-table)
  "The list of servers, as a hash table from port to server object.")

(defclass server ()
  ((thread :accessor server-thread :initarg :thread)
   (socket :accessor server-socket :initarg :socket :type usocket)
   ;; XXX: Using a list for the clients makes for rather inefficient
   ;; removal. This may have to be changed.
   (clients :accessor server-clients :initform ())
   (clients-lock :accessor server-clients-lock :initform (make-lock))
   (client-main-function :initarg :client-main-function
                         :accessor server-client-main-function)
   (client-input-function :initarg :client-input-function
                          :accessor server-client-input-function)))

(defun start-server (&key port
                     client-main-function
                     client-input-function
                     (address *default-address*))
  (let* ((sock (socket-listen address port))
         (server (make-instance
                  'server
                  :client-main-function client-main-function
                  :client-input-function client-input-function
                  :socket sock)))
    (setf (server-thread server)
          (make-thread-with-standard-specials
           (fn (loop (add-client server (socket-accept sock))))))
    (setf (gethash port *servers*) server)))

(defun stop-server (port)
  (let ((server (gethash port *servers*)))
    (if (null server)
        (log-message :warning "Server not running, not stopping anything.")
        (progn
          (log-message :warning "Stopping server now!")
          (log-message :info "Not accepting new connections...")
          (destroy-thread (server-thread server))
          (socket-close (server-socket server))
          (log-message :info "Disconnecting all clients...")
          (for-each-client server #'remove-client)))))
