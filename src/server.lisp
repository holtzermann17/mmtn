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

(defvar *server-thread* ()
  "Holds the server thread.")

(defvar *server-sock* ()
  "Holds the server socket.")

(defun start-server ()
  (setf *server-sock* (socket-listen *address* *port*))
  (setf *server-thread*
        (make-thread-with-standard-specials
         (fn (loop (add-client (socket-accept *server-sock*))))
         :name "mmtn-server")))

(defun stop-server ()
  (if (null *server-thread*)
      (log-message :warning "Server not running, not stopping anything.")
      (progn (log-message :warning "Stopping server now!")
	     (log-message :info "Not accepting new connections...") 
	     (destroy-thread *server-thread*)
	     (socket-close *server-sock*)
	     (setf *server-sock* nil)
	     (setf *server-thread* nil)
	     (log-message :info "Disconnecting all clients...")
	     (for-each-client #'remove-client))))
