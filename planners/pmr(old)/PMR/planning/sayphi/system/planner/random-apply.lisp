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
;; Description: Random Apply of Actions (Random walks)
;; Date: 2008.10.04
;; 
;; ========================================================================


;; The state need to be loaded in the *current-problem*
;; variable. Use directly parse-problem for loading
;; a symbolic initial state

(defun state-random-apply (&key (apply-depth 50))
  (let ((random-state (make-random-state t))
	(node nil)
	(num-of-children 0))
    (set-duplicate-hashing)
    (setf node (initialize-current-problem))

    (dotimes (i-time apply-depth (snode-state node))
;;       (format t "~% Executing ~a" (snode-plan-action node))
      (expand-state node :helpful nil)
      (setf num-of-children (length (snode-children node)))
      (setf node (nth (random (length (snode-children node)) random-state)
		      (snode-children node))))))

