;; copyright (c) 1997-2005 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2002-2012 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: grid-invoke.cl,v 2.6 2007/04/17 21:54:39 layer Exp $

(in-package :user)

(in-package :user)

;;;;;;;;;;;;;;Sample server startup code for grid example (cl)
;;;;;;;;;;;;;;see the file README-grid-cl for loading instructions


(defun load-grid ()
  (format t "Compiling the IDL file: ~s~%" (merge-pathnames "../idl/grid.idl" *load-pathname*))
  (corba:idl (merge-pathnames "../idl/grid.idl" *load-pathname*)) ; compile the IDL
  (format t "Loading the grid implementation code in ~s~%"(merge-pathnames "grid-implementation.cl" *load-pathname*))
  (load (merge-pathnames "grid-implementation.cl" *load-pathname*))
  (format t "Evaluate (grid-server <filename>) to run the server and (grid-client <filename> to to run the client~%")
  (format t "For example, you could use the calls~%   (grid-server ~s)~% and ~%   (grid-client ~s)~%"
	  (namestring (merge-pathnames "grid.ior" *load-truename*))
	  (namestring (merge-pathnames "grid.ior" *load-truename*))))

(load-grid)
;; grid-implementation is bound to a grid implementation.
;; it is used by the function grid-server
(defparameter grid-implementation nil)

;; grid-proxy is bound to a proxy for a grid, used by the grid-client
(defparameter grid-proxy nil)

;; This function instantiates the grid-implementation class and writes the IOR of the
;; created instance out to a file named "grid"

(defun grid-server(filename)
  (format t "grid-server: Creating an instance of the grid-implementation in the variable named: ~s~%" 'grid-implementation)
  (setq grid-implementation (make-instance 'grid-implementation))
  (format t "grid-server: Writing the IOR of ~s to the file ~s~%" grid-implementation filename)
  (orblink:write-ior-to-file grid-implementation filename)
  grid-implementation)


;;The function grid-client retrieves the IOR named "grid" from the simple-nameserver;
;;it then sets and retrieves an element at index 2 3

(defun grid-client(filename)
  (format t "Setting the variable:~%   ~s~% to the IOR in the file named ~s~%" 'grid-proxy filename)
  (setq grid-proxy (orblink:read-ior-from-file filename))
  (format t "Setting the grid element at 2 3 by invoking OP:SET of the object pointed to by the proxy:~%   ~s~%  to the string: ~s~%" grid-proxy "foo")
  (op:set grid-proxy 2 3 "foo")
  (format t "Invoking OP:GET on~%   ~s~%   got back the value: ~s~%" grid-proxy (op:get grid-proxy 2 3)))



