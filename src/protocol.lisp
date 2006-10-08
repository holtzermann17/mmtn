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

;; Implements the various versions of the mmtn protocol.

(in-package mmtn)

(defvar *client-version* nil
  "The client's version. Only defined in client threads.")

(defconstant +max-client-version+ 1
  "The maximum supported client version.")

(defun client-main ()
  "Starts the client running."
  (with-client-input version
    (let ((*client-version*
	   (or (min +max-client-version+
		    (max 0 (parse-integer version :junk-allowed t)))
	       1)))
      (client-message "~A~%" *client-version*)
      (command-loop))))

(defun command-loop ()
  "Iteratively reads and processes commands."
  (with-client-input line
    ;; XXX
    (client-message "~A~%" line) 
    (command-loop)))
