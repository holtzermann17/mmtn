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

(defsystem "mmtn"
    :version "0.0.0"
    :author "Nick Thomas <jesuswaffle@gmail.com>"
    :licence "GPLv2"
    :depends-on (:usocket :bordeaux-threads)
    :components ((:file "packages")
		 (:file "config" :depends-on ("packages"))
		 (:file "util" :depends-on ("packages" "config"))
		 (:file "logger" :depends-on ("util"))
		 (:file "queue" :depends-on ("util"))
		 (:file "client" :depends-on ("util" "queue"))
		 (:file "server" :depends-on ("util" "logger" "client"))
		 (:file "protocol" :depends-on ("util" "client"))
		 (:file "commands" :depends-on ("protocol" "util"))))
