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
;; $Id: naming-administration.cl,v 2.7 2007/04/17 21:54:39 layer Exp $

(in-package :orblink/naming)

(defparameter *default-naming-port* 12404)
(defparameter *naming-marker* "ORBLINK NAME SERVICE")

(defun start-nameserver (&key port)
  (setq port (or port *default-naming-port*))
  (setf (op:port corba:orb) port)
  (let ((nameservice
	 (make-instance 'orblink/naming:NamingContext :_marker *naming-marker*)))
    (op:object_to_string corba:orb nameservice)
    nameservice))

#+ignore
(defun nameservice-ior (&key host port)
  (setq port (or port *default-naming-port*))
  (setq host (or host (op:host corba:orb)))
  (orblink:stringified-ior :host host :port port :_marker *naming-marker* :repository_id "IDL:omg.org/CosNaming/NamingContext:1.0"))

