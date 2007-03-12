;; database interaction for Monster Mountain
;; Copyright (C) 2007 Joseph Corneli
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

;; I am defining a package mmtn-db to support interaction between the
;; Monster Mountain core and a database backend.  I'm hoping I can set
;; things up so that we can easily swap out databases and
;; database-drivers.  Right now, I'm setting things up to use the
;; Postmodern database-driver and PostGreSQL.

(in-package mmtn-db)

;; I searched google groups for "defmacro defalias"; combined ideas
;; from the comp.lang.lisp threads "multiple return values" and
;; "How can I use same definition with different name?".  I don't
;; know if this is the best version, but it works the same way as
;; the elisp `defalias' so, I guess it will work for me for now.
(defmacro defalias (new old)
  `(defun ,new (&rest args) (apply ',old args))) 

(defun postmodern-add-string (str)
  "Add STR to the strs table."
  ;; it would be handy if we could *return* the index corresponding to
  ;; new newly added string (instead of `nil').
  (execute (:insert-into 'strs :set 'string str)))

(defalias add-string postmodern-add-string)

(defun postmodern-string-present-p (str)
  "Return the id# of STR, if present; nil otherwise."
  (query (:select 'strid :from 'strs :where (:= 'string str)) :single))

(defalias string-present-p postmodern-string-present-p)

(defun postmodern-id-to-string (id)
  "Return the string corresponding to ID, if present; nil otherwise."
  (query (:select 'string :from 'strs :where (:= 'strid id)) :single))

(defalias id-to-string postmodern-id-to-string)

(defun postmodern-add-relation (mid beg end)
  "Add a relation, also adding strings in the relation (as needed)."
  (let ((mid-id (or (string-present-p mid)
                    (progn (add-string mid)
                           (string-present-p mid))))
        (beg-id (or (string-present-p beg)
                    (progn (add-string beg)
                           (string-present-p beg))))
        (end-id (or (string-present-p end)
                    (progn (add-string end)
                           (string-present-p end)))))
    (execute (:insert-into 'rels :set 'beginningid beg-id
                                      'middleid mid-id
                                      'endid end-id))))

(defalias add-relation postmodern-add-relation)

(defun postmodern-get-outbound-relations (str)
  (let ((id (string-present-p str)))
    (when id
      (mapcar (lambda (relation)
                (mapcar #'id-to-string relation))
              (query (:select 'beginningid 'middleid 'endid
                      :from 'rels
                      :where (:= 'beginningid id)) :rows)))))

(defalias get-outbound-relations postmodern-get-outbound-relations)

(defun postmodern-get-inbound-relations (str)
  (let ((id (string-present-p str)))
    (when id
      (mapcar (lambda (relation)
                (mapcar #'id-to-string relation))
              (query (:select 'beginningid 'middleid 'endid
                      :from 'rels
                      :where (:= 'endid id)) :rows)))))

(defalias get-inbound-relations postmodern-get-inbound-relations)

(defun postmodern-get-bound-relations (str)
  (let ((id (string-present-p str)))
    (when id
      (mapcar (lambda (relation)
                (mapcar #'id-to-string relation))
              (query (:select 'beginningid 'middleid 'endid
                      :from 'rels
                      :where (:= 'middleid id)) :rows)))))

(defalias get-bound-relations postmodern-get-bound-relations)
