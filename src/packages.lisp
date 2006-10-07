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

;; Package definitions.

(unless (member :sbcl *features*)
  (error "Sorry, mmtn only runs under SBCL. Please install it and try again."))

(unless (member :sb-thread *features*)
  (error "Sorry, mmtn requires multithreading support. Please
recompile your SBCL with SB-THREAD enabled."))

(defpackage :mmtn
  (:use :cl :sb-thread :sb-bsd-sockets :sb-ext))
