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

;; Thread-safe queues.

;; XXX: Using lists for the queues should be sufficient for queues
;; that remain small, but will demonstrate some bad performance
;; characteristics for larger queues.

(in-package mmtn)

(defclass queue ()
  ((contents :accessor queue-contents :initform ())
   (mutex :reader queue-mutex :initform (make-mutex) :type mutex)
   (waitqueue :reader queue-waitqueue :initform (make-waitqueue)
	      :type waitqueue)))

(defun %queue-empty-p (queue)
  (null (queue-contents queue)))

(defun %dequeue (queue)
  (pop (queue-contents queue)))

(defun %enqueue (queue obj)
  (setf (queue-contents queue) (nconc (queue-contents queue) (list obj)))
  queue)

(defun make-empty-queue ()
  "Returns a freshly created empty queue."
  (make-instance 'queue))

(defun enqueue (queue obj)
  "Adds an element to the queue."
  (with-mutex ((queue-mutex queue))
    (%enqueue queue obj)
    (condition-notify (queue-waitqueue queue))))

(defun dequeue (queue &optional default)
  "Removes an element from the queue. Returns DEFAULT If the queue is empty."
  (with-mutex ((queue-mutex queue))
    (if (%queue-empty-p queue)
	default
	(%dequeue queue))))

(defun blocking-dequeue (queue)
  "Removes an element from the queue. If the queue is empty, blocks until
another thread adds an element."
  (with-mutex ((queue-mutex queue))
    (if (%queue-empty-p queue)
	(loop (condition-wait (queue-waitqueue queue) (queue-mutex queue))
	      (unless (%queue-empty-p queue)
		(return (%dequeue queue))))
	(%dequeue queue))))

(defun queue-empty-p (queue)
  "True if the given queue is empty; false otherwise."
  (with-mutex ((queue-mutex queue))
    (%queue-empty-p queue)))