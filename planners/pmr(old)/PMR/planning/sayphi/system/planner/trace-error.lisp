;; =========================================================================    
;;  (C) Copyright 2006, 2008 
;;      Universidad Carlos III de Madrid
;;      Planning & Learning Group (PLG)
;; 
;; =========================================================================
;; 
;; This file is part of SAYPHI
;; 
;; 
;; SAYPHI is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; SAYPHI is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with SAYPHI.  If not, see <http://www.gnu.org/licenses/>.
;; ========================================================================

;; Author: Tomas de la Rosa 
;; Description: Controlling error messaging
;; Date: 2005.12.04
;; 

(defvar *error-info* t)

(defun msg-error (type-error function &optional (eparameter nil))
  (let ((msg
	 (case type-error 
	       (not_pddl_problem "This file is not a PDDL problem")
	       (no_domain_dir "Need to load first a domain file")
	       (no-loaded-domain "There is no loaded domain!")
;; 	       (not_domain_predicate "State predicate not in domain description ")
;; 	       (not_domain_function "State function not in domain description ")
	       (t "Generic SAYPHI error"))))
    (if eparameter
	(format t "~%~a [~a]" msg eparameter)
	(format t "~%~a" msg))
    (when *error-info*
      (format t "~%Function: ~a" function))))