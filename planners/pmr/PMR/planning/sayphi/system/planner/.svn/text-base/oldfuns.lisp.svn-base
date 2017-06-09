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
;; Description: Old Functions. They may be useful in other moments.
;; ========================================================================



;; I just want to keep some functions that are not in sayphi current version
;; but they were coded for some reason

;; Creates the preconds-formulas for all actions
(defun buildall-posformula ()
  (dolist (iaction (dom-actions *pspace*))
    (setf (action-precond-formulas iaction) (actionprec-posformula iaction))))


;; Creates the tests-formulas to eval if the bitmap-position is true for the precondition
;; instance
(defun actionprec-posformula (action)
  (let ((objects (problem-inheritobjects *current-problem*))
	(params (action-parameters action))
	(formulas-thash (make-hash-table :test #'equal)))
    (dolist (iprec (action-preconditions action) formulas-thash)
      (let ((p-formula nil))
	(dolist (iarg (cdr iprec))
	  (let ((arg-mod (length (gethash (cdr iarg) objects)))
		(arg-permutfactor (get-permut-factor iarg params objects))
		(arg-var (intern (format nil "~a-POS" (symbol-name (car iarg)))))
		(v-formula nil))
	    (if (= 1 arg-permutfactor)
		(setf v-formula (list 'mod 'this-pos  arg-mod))
	      (setf v-formula (list 'mod (list 'floor 'this-pos arg-permutfactor) arg-mod)))
	    
	    (push (list 'eq arg-var v-formula) p-formula)))
	(if (= 1 (length p-formula))
	    (setf (gethash iprec formulas-thash) (car p-formula))
	  (setf (gethash iprec formulas-thash) (push 'and p-formula)))))))



;; It's like h-relaxplan. It also counts the applicable-action that modifies the satate 
;; through steps propagation. Then this is use to differenciate the nodes with the same
;; h value
(defun h-relaxplan-ext (node &optional (problem *current-problem*))
  (let ((state (copy-state (snode-state node))))
    (do* ((h-distance 1 (+ 1 h-distance))
	  (h-applied-factor (expand-relax-ext state problem h-distance)
			    (expand-relax-ext state problem h-distance))
	  (total-applied-factor h-applied-factor (+ total-applied-factor h-applied-factor)))
	 ((or (goals-reached state problem)
	      (> h-distance *h-max-distance*))
	  (+ h-distance (- 1 total-applied-factor))))))
   

;; It's like expand-state but it generetes only one child with all instance applied
(defun expand-relax-ext (state problem h-distance)
  (let ((current-state (copy-state state))
	(actions (dom-actions *pspace*))
	(predicates (dom-predicates *pspace*))
	(objects (problem-inheritobjects problem))
	(totalnum-applicable (get (problem-plist problem) :num-applicable-instances)) 
	(app-instances nil) (applicable-count 0) (applicable-changes 0)
	(h-applied-factor 0.0))
    (declare (type h-applied-factor single-float))
    (dolist (iaction actions)
      (let ((applicable-map (action-applicable-bitmap iaction current-state problem))
	    (params (action-parameters iaction)))
	(when (car applicable-map)
;; 	  (setf applicable-count (+ applicable-count (count 1 (cadr applicable-map))))
	  (setf app-instances (varinstance-from-bitmap (cadr applicable-map) params objects))
	  (dolist (inst app-instances)
	    (when (apply-instance-relaxadd state (action-adds iaction) inst objects predicates)
	      (incf applicable-changes))))))
;;     (format t "total:~d  app-count:~d  app-changes: ~d" 
;; 	      totalnum-applicable applicable-count applicable-changes)
    (when (< h-distance 3)
      (setf h-applied-factor 
	    (/ (/ applicable-changes totalnum-applicable)
	       (expt 10 (- h-distance 1)))))
    (coerce h-applied-factor 'short-float)
    ))


;; Old function to calculate applicable map.  I changed this by the direct preconditions 
(defun action-applicable-bitmap (action state problem)
  (let* ((objects (problem-inheritobjects problem))
	 (applicable-map (copy-seq (action-instances-pattern action)))
	 (preconds-map (calculate-action-precond action state))
	 (args-patterns (action-params-pattern action))
	 (rev-args (reverse (action-parameters action)))
	 (prec-instances nil) (prec-args nil))
    (do ((applicable-p t)
	 (imap (pop preconds-map) (pop preconds-map)))
	((or (not applicable-p) (null imap))
 	 (list applicable-p applicable-map))
	(setf prec-args (cdar imap))
	(setf prec-instances (extract-prec-instances (caar imap) prec-args 
						     (cadr imap) objects))
	(cond ((null prec-instances)
	       (setf applicable-p nil))

	      (t
	       (let ((prec-bitmap nil))
		 (dolist (inst prec-instances)
		   (let ((inst-map (typebitmap-from-poslist inst objects))
			 (bitmap-list nil) (pos-arg nil))
		     (dolist (iarg rev-args)
		       (if (setf pos-arg (position iarg prec-args :test #'equal))
			   (push (cdr (nth pos-arg inst-map)) bitmap-list)
			 (push (gethash iarg args-patterns) bitmap-list)))
		     (if (simple-bit-vector-p prec-bitmap)
			 (bit-ior prec-bitmap (permut-bitmap bitmap-list) t)
		       (setf prec-bitmap (permut-bitmap bitmap-list)))))
		 (bit-and applicable-map prec-bitmap t))
	       )
))))


(defun extract-prec-instances (pred args bitmap objects)
  (let ((predicates (dom-predicates *pspace*))
	(prec-instances nil))
    (dotimes (ibit (length bitmap) prec-instances)
      (when (= 1 (sbit bitmap ibit))
	(let ((objpos (objpos-from-mappos ibit (gethash pred predicates) objects)))
	  (push (get-type-instances objpos args objects) prec-instances))))))



(defun get-real-instances (poslist args objects)
  (mapcar #'(lambda (ipos iarg)
	      (if (eq (cdr ipos) (cdr iarg))
		  ipos
		(realobject-pos ipos objects)))
	  poslist args))


;; Gets the bitmaps that satisfy preconditions from the state
(defun calculate-action-precond (xaction state)
  (let ((precs (action-preconditions xaction))
	(preconds-maps nil))
    (dolist (iprec precs preconds-maps)
      (let* ((pattern (get-precond-pattern iprec *current-problem*))
	     (state-map (gethash (car iprec) state))
	     (precond-map (bit-and state-map pattern)))
	(push (list iprec precond-map) preconds-maps)))))
     

;; It get the precondition pattern. If it doesn't exist, is created in the hash table
;; When the predicate in precondition is the same in predicates definition 
;; a full 1-bit map is returned
(defun get-precond-pattern (precond problem)
  (let* ((precond-htable (problem-precond-patterns problem))
	 (precond-pattern (gethash precond precond-htable)))

    (when (null precond-pattern)
      (let* ((predicates (dom-predicates *pspace*))
	     (objects (problem-inheritobjects problem)) 
	     (args (mapcar #'cdr (cdr precond)))
	     (abs-args (mapcar #'cdr (gethash (car precond) predicates)))
	     (prec-objects (map-gethash args objects))
	     (abs-prec-objects (map-gethash abs-args objects))
	     (args-bitmaps (mapcar #'list-bitmap prec-objects abs-prec-objects)))
	(setf (gethash precond precond-htable) (permut-bitmap args-bitmaps))))

    (gethash precond precond-htable)))


;; It's like expand-state but it generetes only one child with all instance applied
(defun expand-relax (state problem)
  (let ((actions (dom-actions *pspace*))
	(predicates (dom-predicates *pspace*))
	(objects (problem-inheritobjects problem))
	(relax-child (copy-state state)) 
	(app-instances nil)) 
    (dolist (iaction actions relax-child)
      (let ((applicable-map (action-applicable-bitmap iaction state problem))
	    (params (action-parameters iaction)))
	(when (car applicable-map)
	  (setf app-instances (varinstance-from-bitmap (cadr applicable-map) params objects))
	  (dolist (inst app-instances)
	    (apply-instance-relaxadd relax-child (action-adds iaction) inst objects predicates)))
	  ))))


;; It's like apply-instance-eff. In this case i check if the application changes something
;; in the state, becase in relaxed plans the applied-action instance may be true yet
(defun apply-instance-relaxadd (state effs instance objects predicates)
  (let ((relax-state-changed nil))
    (dolist (i-eff effs relax-state-changed)
      (let* ((pred-args (gethash (car i-eff) predicates))
	     (eff-args (cdr i-eff))
	     (eff-inst (sublis instance eff-args))
	     (typed-inst (get-type-instances eff-inst pred-args objects))
	     (inst-poslist (mapcar #'(lambda (x-inst)
				       (list (car x-inst)
					     (length (gethash (cdr x-inst) objects))))
				   typed-inst))
	     (pos nil))

	(setf pos (map-position inst-poslist))
	(when (= 0 (sbit (gethash (car i-eff) state) pos))
	  (setf relax-state-changed t))
	(setf (sbit (gethash (car i-eff) state) pos) 1)))))

;; It changes the state by shifting bitval in the instance-position
;; Is the same function for adds and dels -> bitval 1 or 0 
(defun apply-instance-eff (state effs instance objects bitval)
  (let ((predicates (dom-predicates *pspace*)))
    (dolist (i-eff effs state)
      (let* ((pred-args (gethash (car i-eff) predicates))
	     (eff-args (cdr i-eff))
	     (eff-inst (sublis instance eff-args))
	     (typed-inst (get-type-instances eff-inst pred-args objects))
	     (inst-poslist (mapcar #'(lambda (x-inst)
				       (list (car x-inst)
					     (length (gethash (cdr x-inst) objects))))
				   typed-inst))
	     (pos nil))
      (setf pos (map-position inst-poslist))
      (setf (sbit (gethash (car i-eff) state) pos) bitval)))))


;; Creates the parameters pattern.  The are used to permut the bitmaps with the instances
;; bitmap from each precondition
(defun build-actions-params-pattern ()
  (let ((objects (problem-inheritobjects *current-problem*))
	(actions (dom-actions *pspace*)))
    (dolist (iaction actions)
      (let ((params (action-parameters iaction)))
	(setf (action-params-pattern iaction) 
	      (args-hash-pattern params objects 1))))))


;; Builds a bitval-bitmap pattern for each argument's variable
(defun args-hash-pattern (arguments objects bitval)
  (let ((thash (make-hash-table :test #'equal)))
    (dolist (iarg arguments thash)
      (let ((len (length (gethash (cdr iarg) objects))))
	(setf (gethash iarg thash)
	      (make-array len :element-type 'bit :initial-element bitval))))))



(defun h-leveldiff (node &optional (problem *current-problem*))
  (let ((parent-gplan (snode-relax-state (snode-parent node))))
    (cond ((> (length parent-gplan) 1)
	   (let ((node-relax-level (get-expand-relax (snode-state node))))
	     (diff-states (nth 1 parent-gplan) node-relax-level)))
	  (t
	   (step-to-goals (snode-state node))))))
	   


(defun diff-states (state1 state2)
  (let ((num-diff 0) (pred-diff 0))
    (maphash #'(lambda (pred1 pred-bitmap1)
		 (when (simple-bit-vector-p pred-bitmap1)
		   (let ((pred-bitmap2 (gethash pred1 state2)))
		     (setf pred-diff (count 1 (bit-xor pred-bitmap1 pred-bitmap2)))
		     (setf num-diff (+ num-diff pred-diff)))))
	     state1)
    num-diff))
		 
    
(defun step-to-goals (state &optional (problem *current-problem*))
  (let ((goals-to-reach 0)
	(goals (problem-goals problem)))
    (maphash #'(lambda (pred goal-bitmap)
		 (let ((state-bitmap (gethash pred state)))
		   (setf pred-goals-to-reach (count 1 (bit-andc1 state-bitmap goal-bitmap)))
		   (setf goals-to-reach (+ goals-to-reach pred-goals-to-reach))))
	     goals)
    goals-to-reach))


(defun action-applicable-byte2 (action state &optional (memory-byte nil))
  (apply #'logand (mapcar #'(lambda (iprec jbyte)
			      (apply #'logior (map 'list #'(lambda (kbit pbyte)
							     (if (= 1 kbit) pbyte 0))
						   (gethash (car iprec) state) jbyte)))
			  (action-preconditions action) (action-preconds-bytes action))))



(defun all-applicable-map (state)
  (let ((all-maps nil))
    (dolist (iaction (dom-actions *pspace*) all-maps)
      (let ((iaction-map (action-applicable-map iaction state)))
	(when (simple-bit-vector-p iaction-map)
 	  (push (cons (action-name iaction) iaction-map)
		all-maps))))))


;; This calculate the applicable instances of an operator and returns the bitmap with bit to
;; 1 if the instance is applicable
(defun action-applicable-map (action state &optional (memory-pattern nil))
  (let* ((inst-patterns (if (null memory-pattern)
			    (action-instances-pattern action)
			  (bit-andc2 (action-instances-pattern action) memory-pattern)))
	 (inst-preconds (action-inst-preconds action))
	 (applicable-map (make-array (length inst-patterns) 
				     :element-type 'bit 
				     :initial-element 0)))
    (dotimes (ibit (length inst-patterns))
      (when (= 1 (sbit inst-patterns ibit))
	(let ((applicable-inst t))
	  (dolist (iprec (aref inst-preconds ibit))
	    (unless (= 1 (sbit (gethash (car iprec) state) (cdr iprec)))
	      (setf applicable-inst nil)))

	  (when applicable-inst
	    (setf (sbit applicable-map ibit) 1)))))
    (when (find 1 applicable-map)
      applicable-map))) 


(defun action-app-map (action state &optional (memory-pattern nil))
  (let* ((inst-patterns (if (null memory-pattern)
			    (action-instances-pattern action)
			  (bit-andc2 (action-instances-pattern action) memory-pattern)))
	 (inst-prec (action-inst-prec action))
	 (preconds (action-preconditions action))
	 (applicable-map (make-array (length inst-patterns) 
				     :element-type 'bit 
				     :initial-element 0)))
    (dotimes (ibit (length inst-patterns))
      (when (= 1 (sbit inst-patterns ibit))
	(let ((applicable-inst t))
	  (mapc #'(lambda (iprec jinst)
		    (unless (= 1 (sbit (gethash (car iprec) state) jinst))
		      (setf applicable-inst nil)))
		preconds (aref inst-prec ibit))
	  (when applicable-inst
	    (setf (sbit applicable-map ibit) 1)))))
    (when (find 1 applicable-map)
      applicable-map))) 

;;I count this to avoid counting it many times during planning 
;;The value is located in problem plist :num-applicable-instances 
(defun count-applicable-instances ()
  (let ((num-applicable-instances 0))
    (dolist (iaction (dom-actions *pspace*) num-applicable-instances)
      (setf num-applicable-instances (+ num-applicable-instances
					(count 1 (action-instances-pattern iaction)))))))



;; This creates the instantiated preconditions for each action.  They are allocated in an array
;; of list with the same dimension of instance-pattern
;; Each position contains ie. ((pred1 . 12) (pred2 . 2))
;; This indicates that the bitmap's position in the state must be 1
(defun build-direct-preconds ()
  (let ((predicates (dom-predicates *pspace*))
	(objects (problem-inheritobjects *current-problem*)))
    (dolist (iaction (dom-actions *pspace*))
      (let ((inst-preconds (action-inst-preconds iaction))
	    (inst-prec (action-inst-prec iaction))
	    (inst-pattern (action-instance-byte iaction))
	    (params (action-parameters iaction)))
	(dotimes (ibit (integer-length inst-pattern))
	  (when (logbitp ibit inst-pattern)
	    (let ((varpos (varpos-from-mappos ibit params objects))
		  (direct-preconds nil) (direct-prec nil))
	    (dolist (iprec (action-preconditions iaction))
	       (let* ((pred-args (gethash (car iprec) predicates))
		      (precond-args (cdr iprec))
		      (precond-inst (say-sublis varpos precond-args))
		      (typed-inst (get-type-instances precond-inst pred-args objects))
		      (inst-poslist (poslist-from-typedinst typed-inst objects))
		      (precond-pos (map-position inst-poslist)))
	       (push (cons (car iprec) precond-pos) direct-preconds)
	       (push precond-pos direct-prec)))
	    (setf (aref inst-preconds ibit) direct-preconds)
;; 	    En allegro no funciona bien el nreverse??
;; 	    (nreverse direct-prec)
 	    (setf direct-prec (reverse direct-prec))
	    (setf (aref inst-prec ibit) direct-prec))))))))


(defun build-preconds-bytes (&optional (problem *current-problem*))
  (let ((istate (problem-init-state problem)))
    (dolist (action (dom-actions *pspace*))
      (let ((inst-prec (action-inst-prec action))
	    (preconditions (action-preconditions action)))
	(setf (action-preconds-bytes action) nil)
	(dolist (iprec preconditions)
	  (let ((pred-precbyte (make-array (length (gethash (car iprec) istate)) 
					   :element-type 'integer
					   :initial-element 0)))
	    (setf (action-preconds-bytes action)
		  (append (action-preconds-bytes action) (list pred-precbyte)))))
	(dotimes (ipos (length inst-prec))
	  (when (not (null (aref inst-prec ipos)))
	    (mapc #'(lambda (i-instprec j-precbyte)
		      (set-instance-map (aref j-precbyte i-instprec) ipos 1))
		  (aref inst-prec ipos)
		  (action-preconds-bytes action))))
	))))



;; This create the instantiated effects for each action. They are allocated in an array
;; of list with the same dimension of instance-pattern 
(defun build-direct-apply ()
  (let ((predicates (dom-predicates *pspace*))
	(objects (problem-inheritobjects *current-problem*)))
    (dolist (iaction (dom-actions *pspace*))
      (let ((inst-pattern (action-instance-byte iaction))
	    (inst-adds (action-inst-adds iaction))
	    (inst-dels (action-inst-dels iaction))
	    (params (action-parameters iaction))
	    (rev-adds (reverse (action-adds iaction)))
	    (rev-dels (reverse (action-dels iaction))))
	(dotimes (ibit (integer-length inst-pattern))
	  (when (logbitp ibit inst-pattern) 
	    (let* ((var-inst (varpos-from-mappos ibit params objects))
		   (direct-adds (calculate-direct-apply var-inst rev-adds predicates objects))
		   (direct-dels (calculate-direct-apply var-inst rev-dels predicates objects)))
	      (setf (aref inst-adds ibit) direct-adds)
	      (setf (aref inst-dels ibit) direct-dels))))))))

	      
(defun calculate-direct-apply (varinst eff-list predicates objects)
  (let ((direct-apply nil))
    (dolist (i-eff eff-list direct-apply)
      (let* ((pred-args (gethash (car i-eff) predicates))
	     (eff-args (cdr i-eff))
	     (eff-inst (say-sublis varinst eff-args))
	     (typed-inst (get-type-instances eff-inst pred-args objects))
	     (inst-poslist (poslist-from-typedinst typed-inst objects))
	     (eff-pos (map-position inst-poslist)))
	(push eff-pos direct-apply)
	))))
    

(defun build-effects-bytes (&optional (problem *current-problem*))
  (let ((istate (problem-init-state problem)))
    (dolist (action (dom-actions *pspace*))
      (create-effects-bytes (action-adds action) (action-adds-bytes action)
			    (action-inst-adds action) istate)
      (create-effects-bytes (action-dels action) (action-dels-bytes action)
			    (action-inst-dels action) istate))))


(defun create-effects-bytes (effects effs-bytes inst-effs state-pattern)
  (dolist (ieff effects)
    (let* ((predicate (car ieff))
	   (pred-effbyte (make-array (length (state-pred-bitmap predicate state-pattern))
				    :element-type 'integer
				    :initial-element 0)))
      (setf (gethash predicate effs-bytes) pred-effbyte)))
  (dotimes (ipos (length inst-effs))
    (when (not (null (aref inst-effs ipos)))
      (let ((list-pos 0))
	(dolist (ieff effects)
	  (let ((j-bytes (gethash (car ieff) effs-bytes))
		(pred-pos (nth list-pos (aref inst-effs ipos))))
	    (set-instance-map (aref j-bytes pred-pos) ipos 1)
	    (incf list-pos)))))))


;;======================================================================
;; 2007.03.27
;;Remove from the pattern bindings that have duplicate objets in parameters
;; ie. (x . type-a) (y . type-a) avoids binding -> (a a)
(defun reject-duplicates-patterns ()
  (let ((objects (problem-inheritobjects *current-problem*)))
     (dolist (iaction (dom-actions *pspace*))
       (let ((params (action-parameters iaction))) 
	(dotimes (ibit (integer-length (action-instance-byte iaction)))
	  (let* ((objpos (objpos-from-mappos ibit params objects))
		 (inst (instances-from-poslist objpos objects)))
	    (when (has-duplicates inst)
	      (set-instance-map (action-instance-byte iaction) ibit 0))))))))


(defun newlits-in-state (state1 state2)
  (let ((diff-state (make-hash-table :test #'eq)))
    (maphash #'(lambda (pred1 pred-bitmap1)
		 (when (simple-bit-vector-p pred-bitmap1)
		   (let* ((pred-bitmap2 (gethash pred1 state2))
			  (res-bitmap (bit-andc1 pred-bitmap1 pred-bitmap2)))
		     (when (find 1 res-bitmap)
		       (setf (gethash pred1 diff-state) res-bitmap)))))
	     state1)
    diff-state))


;;========================================================================
;; 2007.11.09
;; Cleaning up ASAYPHI.  The ground actions are represented as a separated
;; structures.  Many changes were done one the instantiating part

;; Creates the list (0 1 ... n-1)
(defun numeral-list (n)
  (let ((nlist nil))
    (dotimes (i n nlist)
      (push (- (1- n) i) nlist))))



(defun action-applicable-byte (action state)
  (declare (hash-table state)
	   (action action))
  (let ((app-byte (action-instance-byte action))
	(prec-bytes (action-prec-bytes action))
	(prec-pos 0))
    (dolist (iprec (action-preconditions action) app-byte)
      (let ((state-pred (state-pred-bitmap (car iprec) state))
	    (pred-appbyte 0)
	    (pred-precbytes (nth prec-pos prec-bytes)))
	(declare (simple-bit-vector state-pred)
		 (integer pred-appbyte))
	(dotimes (ibit (length state-pred))
	  (when (= 1 (sbit state-pred ibit))
	    (setf pred-appbyte (logior pred-appbyte (aref pred-precbytes ibit)))))
	(setf app-byte (logand app-byte  pred-appbyte))
	(incf prec-pos)))))


(defun all-applicable-byte (state)
  (let ((all-bytes nil))
    (dolist (iaction (dom-actions *pspace*) all-bytes)
      (let ((iaction-byte (action-applicable-byte iaction state)))
	(when (> iaction-byte 0)
 	  (push (cons (action-name iaction) iaction-byte)
		all-bytes))))))



(defun create-pred-byte (pos pred-args params abspred-args objects)
  (let* ((objs-pos (objpos-from-mappos pos abspred-args objects))
	 (instances-pos (get-type-instances objs-pos pred-args objects))
	 (pred-byte 0) (byte-poslist nil) (pos-arg nil))
    (dolist (iparam params)
      (let ((num-inst (length (gethash (cdr iparam) objects))))
	(if (setf pos-arg (position iparam pred-args :test #'equal))
	    (push (cons (expt 2 (car (nth pos-arg instances-pos))) num-inst) byte-poslist)
	  (push (cons (- (expt 2 num-inst) 1) num-inst) byte-poslist))))
    (setf pred-byte (permut-bytes byte-poslist))))
	

(defun build-pred-bytes (action-preds action-args state-pattern predicates objects)
  (let ((pred-bytes-list nil))
    (dolist (ipred (reverse action-preds) pred-bytes-list)
      (let* ((state-map (state-pred-bitmap (car ipred) state-pattern))
	     (pred-schema (get-type-schema ipred))
	     (predicate-args (gethash (car ipred) predicates))		 
	     (pred-bytes (make-array (length state-map)
				     :element-type 'integer
				     :initial-element 0)))
	(dotimes (ipos (integer-length pred-schema))
	  (when (logbitp ipos pred-schema)
	    (setf (aref pred-bytes ipos)
		  (create-pred-byte ipos (cdr ipred) action-args 
				    predicate-args objects))))
      (push pred-bytes pred-bytes-list)))))



(defun create-inst-predicates (pred-bytes pred-inst)
  (dolist (i-predbytes (reverse pred-bytes))
    (dotimes (pred-pos (length i-predbytes))
      (let ((predpos-byte (aref i-predbytes pred-pos)))
	(dotimes (ibit (integer-length predpos-byte))
	  (when (logbitp ibit predpos-byte)
	    (push pred-pos (aref pred-inst ibit))))))))
		       
  


;; Note that the achivers (pred (addpos . action)) can look like the applicable action
;; Here the cdr is not a position, but a relative position of the add predicate 
(defun build-pred-achievers ()
  (let ((htable-achievers (make-hash-table :test #'eq)))
    (dolist (iaction (dom-actions *pspace*) htable-achievers)
      (let ((add-pos 0))
	(dolist (i-add (action-adds iaction))
	  (push (cons add-pos (action-name iaction)) 
		(gethash (car i-add) htable-achievers))
	  (incf add-pos)
	  )))))


;; (defun is-action-pred-irrelevant (action
(defun is-action-cost-irrelevant (action i-inst)
  (let ((irrelevant t)
	(inst-costfn (aref (action-inst-costs action) i-inst)))
    (dolist (i-costfn inst-costfn irrelevant)
      (when irrelevant
	(cond ((and (numberp (second i-costfn))
		    (> (second i-costfn) 0))
	       (setf irrelevant nil)))))))


;; It takes the instantiated actions and looks for redundancy, when applying an action
;; does not change the state
(defun remove-redundant-inst-action ()
  (dolist (iaction (dom-actions *pspace*))
    (dotimes (i-inst (action-num-instances iaction))
        (let ((matching-effects (every #'(lambda (x y) (eq (car x) (car y))) 
				       (action-adds iaction) (action-dels iaction)))
	      (irrelevant-for-predicate nil)
	      (irrelevant-for-numvars nil))
	  (when matching-effects 
	    (cond ((equalp (aref (action-inst-adds iaction) i-inst)
			   (aref (action-inst-dels iaction) i-inst))
		   (setf irrelevant-for-predicate t)
		   (when (is-action-cost-irrelevant iaction i-inst)
		     (setf irrelevant-for-numvars t))))
	    (when (and irrelevant-for-predicate irrelevant-for-numvars)
	      (set-instance-map (action-instance-byte iaction) i-inst 0)))))))


(defun build-direct-predinstance()
  (dolist (action (dom-actions *pspace*))
    (create-inst-predicates (action-prec-bytes action) (action-inst-prec action))
    (create-inst-predicates (action-adds-bytes action) (action-inst-adds action))
    (create-inst-predicates (action-dels-bytes action) (action-inst-dels action))))



(defun build-actions-bytes ()
  (let ((istate (problem-init-state *current-problem*))
	(predicates (dom-predicates *pspace*))
	(objects (problem-inheritobjects *current-problem*)))
    (dolist (action (dom-actions *pspace*))
      (let ((rev-action-params (reverse (action-parameters action))))
	(setf (action-prec-bytes action) 
	      (build-pred-bytes (action-preconditions action) 
				rev-action-params istate predicates objects))
	(setf (action-adds-bytes action)
	      (build-pred-bytes (action-adds action) 
				rev-action-params istate predicates objects))
	(setf (action-dels-bytes action) 
	      (build-pred-bytes (action-dels action) 
				rev-action-params istate predicates objects))))))




;; Creates the pattern to see which instances are true from a precondition
;; This should be called only once after loading a problem.
;; Some instances are never true in the state, but we have to calculate it after some semantic
;; analysis
(defun build-actions-instpattern ()
  (let ((objects (problem-inheritobjects *current-problem*)))
    (dolist (iaction (dom-actions *pspace*))
      (let* ((params (action-parameters iaction))
	     (count-ob-list (mapcar #'(lambda (iparam) 
					(length (gethash (cdr iparam) objects)))
					params))
	     (num-instances (apply #'* count-ob-list)))
	(setf (action-instance-byte iaction)
	      (- (expt 2 num-instances) 1))   
	(setf (action-num-instances iaction) num-instances)

	(setf (action-inst-prec iaction)
	      (make-array num-instances :element-type 'list :initial-element nil))
	(setf (action-inst-adds iaction)
	      (make-array num-instances :element-type 'list :initial-element nil))
	(setf (action-inst-dels iaction)
	      (make-array num-instances :element-type 'list :initial-element nil))     

	;;for metric optimization
	(setf (action-inst-precfuns iaction)
	      (make-array num-instances :element-type 'list :initial-element nil))     
	(setf (action-precfuns-clousures iaction)
	      (make-array num-instances :element-type 'list :initial-element nil))     
	(setf (action-inst-costs iaction)
	      (make-array num-instances :element-type 'list :initial-element nil))     
	

	))))


(defun create-actions-direct-access ()
  (let ((direct-actions (make-hash-table :test #'eq)))
    (dolist (iaction (dom-actions *pspace*) direct-actions)
      (setf (gethash (action-name iaction) direct-actions) iaction))))


(defun instobjects-schema (args objects)
  (let ((instob-schemas nil))
    (dolist (iarg args (reverse instob-schemas))
      (cond ((hkey-present iarg objects)
	     (push (gethash iarg objects) instob-schemas))
	    (t (push (list iarg) instob-schemas))))))


(defun get-type-schema (predicate)
  (let* ((htable-schemas (problem-predtype-schemas *current-problem*))
	 (predtype-schema (gethash predicate htable-schemas)))
    (when (null predtype-schema)
      (let* ((predicates (dom-predicates *pspace*))
	     (objects (problem-inheritobjects *current-problem*)) 
	     (args (mapcar (lambda (arg) (if (consp arg) (cdr arg) arg)) (cdr predicate)))
	     (abs-args (mapcar #'cdr (gethash (car predicate) predicates)))
	     (inst-objects (instobjects-schema args objects))
	     (abs-inst-objects (map-gethash abs-args objects))
	     (schema-bytes (mapcar #'typematch-integer inst-objects abs-inst-objects)))
	(if (not (null schema-bytes))
	    (setf (gethash predicate htable-schemas) 
		  (permut-bytes schema-bytes))
	  (setf (gethash predicate htable-schemas) 1))))
    (gethash predicate htable-schemas)))
