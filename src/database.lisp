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

(defun mmtn-postmodern-add-string (str)
  "Add STR to the strs table."
  ;; it would be handy if we could *return* the index corresponding to
  ;; new newly added string (instead of `nil').
  (execute (:insert-into 'strs :set 'string str)))

(defalias mmtn-db-add-string mmtn-postmodern-add-string)

(defun mmtn-postmodern-string-present-p (str)
  "Return the id# of STR, if present; nil otherwise."
  (query (:select 'strid :from 'strs :where (:= 'string str)) :single))

(defalias mmtn-db-string-present-p mmtn-postmodern-string-present-p)

(defun mmtn-postmodern-id-to-string (id)
  "Return the string corresponding to ID, if present; nil otherwise."
  (query (:select 'string :from 'strs :where (:= 'strid id)) :single))

(defalias mmtn-db-id-to-string mmtn-postmodern-id-to-string)

(defun mmtn-postmodern-add-relation (mid beg end)
  "Add a relation, also adding strings in the relation (as needed)."
  (let ((mid-id (or (mmtn-db-string-present-p mid)
                    (progn (mmtn-db-add-string mid)
                           (mmtn-db-string-present-p mid))))
        (beg-id (or (mmtn-db-string-present-p beg)
                    (progn (mmtn-db-add-string beg)
                           (mmtn-db-string-present-p beg))))
        (end-id (or (mmtn-db-string-present-p end)
                    (progn (mmtn-db-add-string end)
                           (mmtn-db-string-present-p end)))))
    (execute (:insert-into 'rels :set 'beginningid beg-id
                                      'middleid mid-id
                                      'endid end-id))))

(defalias mmtn-db-add-relation mmtn-postmodern-add-relation)

(defun mmtn-postmodern-get-outbound-relations (str)
  (let ((id (mmtn-db-string-present-p str)))
    (when id
      (mapcar (lambda (relation)
                (mapcar #'mmtn-db-id-to-string relation))
              (query (:select 'beginningid 'middleid 'endid
                      :from 'rels
                      :where (:= 'beginningid id)) :rows)))))

(defalias mmtn-db-get-outbound-relations mmtn-postmodern-get-outbound-relations)

(defun mmtn-postmodern-get-inbound-relations (str)
  (let ((id (mmtn-db-string-present-p str)))
    (when id
      (mapcar (lambda (relation)
                (mapcar #'mmtn-db-id-to-string relation))
              (query (:select 'beginningid 'middleid 'endid
                      :from 'rels
                      :where (:= 'endid id)) :rows)))))

(defalias mmtn-db-get-inbound-relations mmtn-postmodern-get-inbound-relations)

(defun mmtn-postmodern-get-bound-relations (str)
  (let ((id (mmtn-db-string-present-p str)))
    (when id
      (mapcar (lambda (relation)
                (mapcar #'mmtn-db-id-to-string relation))
              (query (:select 'beginningid 'middleid 'endid
                      :from 'rels
                      :where (:= 'middleid id)) :rows)))))

(defalias mmtn-db-get-bound-relations mmtn-postmodern-get-bound-relations)