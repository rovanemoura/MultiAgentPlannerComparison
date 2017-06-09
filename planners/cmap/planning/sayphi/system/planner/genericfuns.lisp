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
;; Description: Generic Functions & Macos, not associated to any part of the planner
;; Date: 2005.12.06
;; 
;; ========================================================================


;; Generic Macros
;; for Access to STATES
;; ===================================================

(defmacro change-state (state predicate pos bitval)
  `(setf (sbit (gethash ,predicate ,state) ,pos) ,bitval))

(defmacro true-in-state (state predicate pos)
  `(= 1 (sbit (gethash ,predicate ,state) ,pos)))

(defmacro false-in-state (state predicate pos)
  `(zerop (sbit (gethash ,predicate ,state) ,pos)))

(defmacro set-instance-map (map pos bitval)
  `(setf (ldb (byte 1 ,pos) ,map) ,bitval))

;; Return the bitmap of the given predicate
(defmacro state-pred-bitmap (predicate state)
  `(gethash ,predicate ,state))
;;=================================================================  

(defmacro linearexpression-p (expression)
  `(when (listp ,expression)
    (or (eq (car ,expression) '+)
     (eq (car ,expression) '-))))


(defmacro numvar-predpos-state-value (pred-pos state)
   `(aref (gethash (car ,pred-pos) ,state) (cdr ,pred-pos)))


(defmacro numvar-state-value (functor state pos)
   `(aref (gethash ,functor ,state) ,pos))


(defmacro functor-state-value (functor arg state varpos)
 `(aref (gethash ,functor ,state) (fun-state-pos ,arg ,varpos)))

(defmacro functor-pos-state-value (functor pos state)
 `(aref (gethash ,functor ,state) ,pos))



(defmacro change-fun-state (state pred-fun pos value)
  `(setf (aref (gethash ,pred-fun ,state) ,pos) ,value))

(defmacro set-sayp (parameter value)
  `(setf (getf (problem-plist *current-problem*) ,parameter) ,value))

(defmacro get-sayp (parameter)
  `(getf (problem-plist *current-problem*) ,parameter))



;;======================================================
;; Generic Functions 
;;======================================================

(defun predpos-p (proposition state)
  (and (consp proposition)
       (hkey-present (car proposition) state)
       (numberp (cdr proposition))))


(defun get-vals (element assoc-list)
  (cadr (assoc element assoc-list))) 


;;It's a function to simulate a mapcar whith a gethash function 
(defun map-gethash (keys htable)
  (mapcar #'(lambda (ikey)
	      (gethash ikey htable)) 
	  keys))

(defun hashkeys-to-list (hash-table)
  (let ((hash-list nil))
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (push key hash-list))
	     hash-table)
    (reverse hash-list)))


(defun has-duplicates (xlist)
  (let ((duplicate nil))
    (mapl #'(lambda (rest-list)
	      (when (find (car rest-list) (cdr rest-list))
		(setf duplicate t)))
	  xlist)
    duplicate))


;;Like sublis. It only substitute in the cdr's of the trees
(defun say-sublis (inst params) 
  (mapcar #'(lambda (i-param)
	      (cons (cdr (assoc (car i-param) inst)) (cdr i-param)))
	  params))



;; Testing if the key of a hash-table is present
(defun hkey-present (key htable)
  (multiple-value-bind (value is-present) (gethash key htable) 
    (declare (ignore value))
    is-present))



(defun poslist-from-typedinst (typedinst objects)
  (let ((poslist nil))
     (dolist (x-inst typedinst (reverse poslist))
       (push (list (car x-inst) (length (gethash (cdr x-inst) objects)))
	     poslist))))


;;Returns the real-object position of the abs-type
;;ie. (2 . abs-type) -> (0 . type-b)
(defun realobject-pos (pos objects)
  (let ((types (get-vals (cdr pos) (dom-inheritypes *pspace*)))
	(inst-ob (nth (car pos) (gethash (cdr pos) objects)))
	(real-pos nil))
    (dolist (itype types real-pos)
      (let* ((inherit-obs (gethash itype objects))
	     (xpos (position inst-ob inherit-obs)))
	(unless (null xpos) 
	  (setf real-pos (cons xpos itype)))))))
	

;;Returns the type-object position of the abs-type
;;ie. (2 . abs-type), b-type  -> (0 . b-type)
(defun typeobject-pos (pos type objects)
  (let* ((inst-ob (nth (car pos) (gethash (cdr pos) objects)))
	(typeob-pos nil)
	(inherit-obs (gethash type objects))
	(xpos (position inst-ob inherit-obs)))
    (unless (null xpos) 
      (setf typeob-pos (cons xpos type)))))


(defun get-type-instances (poslist args objects)
  (mapcar #'(lambda (ipos iarg)
	      (if (or (atom iarg)
		      (eq (cdr ipos) (cdr iarg)))
		  ipos
	       	(typeobject-pos ipos (cdr iarg) objects)))
	  poslist args))


;; Builds the pairs lists that use as input map-position (see map-position)
(defun literal-args-pos (instances arguments objects)
  (cond ((= (length instances) (length arguments))
	 (mapcar #'(lambda (inst arg)
		     (let ((thistype-objects (gethash (cdr arg) objects)))
		       (cond ((find inst thistype-objects)
			      (list (position inst thistype-objects)
				    (length thistype-objects)))
			     (t (error "Object [~S] is not of type ~S" inst (cdr arg)))
			     ))) 
		 instances arguments))
	(t (error "Wrong number of objects for predicate arguments [~S][~S]" instances arguments))))


;; Calculates the map's position of a literal 
;; Input: A list containing pairs of (positions num-objects)) 
;; ie. ((0 3) (1 2)) -> (a0 b1) in ((type-a (a0 a1 a2)) 
;;                                  (type-b (b0 b1)))
(defun map-position (pos-list)
  (let ((pos 0) 
	(permuting-factor 1))
    (dolist (this-arg pos-list pos)
      (setf pos (+ pos (* permuting-factor (car this-arg))))
      (setf permuting-factor (* permuting-factor (cadr this-arg))))))


;; Inverse function of map-position. Given a map positions it returns the object position
;; output is like ((0 . type-a) (1 . type-b))
(defun objpos-from-mappos (pos arguments objects)
  (let ((object-pos nil)
	(num pos) (objs-len 1))
    (dolist (iarg arguments object-pos)
      (setf objs-len (length (gethash (cdr iarg) objects)))
      (setf object-pos (append object-pos (list (cons (mod num objs-len) (cdr iarg)))))
      (setf num (floor num objs-len)))))


;; Like objpos-from-mappos. Given a map positions it returns the real object instances
(defun realinst-from-mappos (pos arguments objects)
  (let ((realinst nil)
	(num pos) (objs-len 1))
    (dolist (iarg arguments realinst)
      (setf objs-len (length (gethash (cdr iarg) objects)))
      (setf realinst (append realinst (list (nth (mod num objs-len) 
						 (gethash (cdr iarg) objects)))))
      (setf num (floor num objs-len)))))


;; The same as objpos-from-mappos but returns the argument var and the object position
(defun varpos-from-mappos (pos arguments objects)
  (let ((var-pos nil)
	(num pos) (objs-len 1))
    (dolist (iarg arguments var-pos)
      (setf objs-len (length (gethash (cdr iarg) objects)))
      (setf var-pos (append var-pos (list (cons (car iarg) (mod num objs-len)))))
      (setf num (floor num objs-len)))))


;; Return the real instances of objects from a positons' list;; ie. ((0 . type-a) (2 . type-b)) -> (a0 b2)
(defun instances-from-poslist (poslist objects)
  (mapcar #'(lambda (this-pos) 
	      (nth (car this-pos) (gethash (cdr this-pos) objects)))
	  poslist))


;; Returns the instances of a bitmap in a poslist format
;; ie. #*010001 ((a . a-type) (b. b-type)) -> (((1 . a-type) (0 . b-type))
;;                                             ((1 . a-type) (2 . b-type)))
(defun instancepos-from-bitmap (bitmap arguments objects)
  (let ((instancepos nil))
    (dotimes (ibit (length bitmap) instancepos)
      (when (= 1 (sbit bitmap ibit))
	(push (objpos-from-mappos ibit arguments objects)
	      instancepos)))))


;; Returns the real instances of a bitmap 
(defun realinstances-from-bitmap (bitmap arguments objects)
  (let ((realinst nil))
    (dotimes (ibit (length bitmap) realinst)
      (when (= 1 (sbit bitmap ibit))
	(push (realinst-from-mappos ibit arguments objects)
	      realinst)))))


;; Returns the instanciated vars of parameters from a bitmap
;; ie. #*010001 ((a . a-type) (b. b-type)) -> (((a . 1) (b . 0))
;;                                             ((a . 1) (b . 2))
(defun varinstance-from-bitmap (bitmap arguments objects)
  (let ((varinstance nil))
    (dotimes (ibit (length bitmap) varinstance)
      (when (= 1 (sbit bitmap ibit))
	(push (varpos-from-mappos ibit arguments objects)
	      varinstance)))))


;; Return the bitmaps of objects from a positons' list
;; ie. ((0 . type-a) (2 . type-b)) -> ((type-a #*1000) (type-b #*0010))
(defun typebitmap-from-poslist (poslist objects)
  (mapcar #'(lambda (this-pos) 
	      (let* ((len (length (gethash (cdr this-pos) objects)))
		     (bitmap (make-array len :element-type 'bit)))
		(setf (sbit bitmap (car this-pos)) 1)
		(cons (cdr this-pos) bitmap)))
	  poslist))


;; Returns the real instance of a given plan action
;; ie (op1 . 15) -> (op1 a1 b0 c3)
(defun instance-plan-action (plan-action &optional (objects nil))
  (let* ((action (find (car plan-action) (dom-actions *pspace*) :key #'action-name))
	(params (action-parameters action))
	(xobjects (if (null objects)
		      (problem-inheritobjects *current-problem*)
		    objects))
 	(poslist (objpos-from-mappos (cdr plan-action) params xobjects)))
     (instances-from-poslist poslist xobjects)))



;; Creates a simple-bit-vector whith the bitmap of the elements in the search list
;; ie. el= (b c e)  listmap = (a b c d e) -> #*01101  

(defun list-bitmap (elements maplist)
  (let ((bitmap (make-array (length maplist) :element-type 'bit))
	(pos nil))
    (dolist (ielem elements bitmap)
      (when (setf pos (position ielem maplist))
	(setf (sbit bitmap pos) 1)))))


;;Uff! It's hard to explain. Ask me, and i'll try to say something
(defun permut-bitmap (bitmap-list)
  (let ((x-bitmap (car bitmap-list))
	(p-bitmap nil))
    (dolist (imap (cdr bitmap-list) x-bitmap)
      (setf p-bitmap (make-array (* (length x-bitmap) (length imap)) :element-type 'bit))
      (dotimes (ibit (length imap))
	(when (= 1 (sbit imap ibit))
	  (replace p-bitmap x-bitmap :start1 (* ibit (length x-bitmap)))))
      (setf x-bitmap (copy-seq p-bitmap)))
))


;; Creates an integer whith the map of the elements in the search list
(defun typematch-integer (elements maplist)
  (let ((map-byte 0)
	(pos nil))
    (dolist (ielem elements (cons map-byte (length maplist)))
      (when (setf pos (position ielem maplist))
	(set-instance-map map-byte pos 1)))))


(defun permut-bytes (bytes-poslist)
  (let* ((x-posbyte (car bytes-poslist))
	 (t-byte (car x-posbyte))
	 (x-byte (car x-posbyte))
	 (x-bytesize (cdr x-posbyte)))
    (dolist (i-posbyte (cdr bytes-poslist) t-byte)
      (let ((i-byte (car i-posbyte))
	    (i-bytesize (cdr i-posbyte)))
	(setf t-byte 0)
	(dotimes (ibit i-bytesize)
	  (when (logbitp ibit i-byte)
	    (setf (ldb (byte x-bytesize (* x-bytesize ibit)) t-byte) x-byte)))
	(setf x-byte t-byte)
	(setf x-bytesize (* i-bytesize x-bytesize))))))



;; Stuff about bitmaps position.  It calculates the rigth divisor for the argument to get the
;; map position.
(defun get-permut-factor (arg arguments objects)
  (let ((permut-factor 1)
	(permut-args (cdr (member arg (reverse arguments)))))
    (dolist (iarg permut-args permut-factor)
      (setf permut-factor (* permut-factor (length (gethash (cdr iarg) objects)))))))


(defun get-object-type (object)
  (let ((x-type nil) (found nil))
    (maphash #'(lambda (type objects)
         (unless found
             (when (find object objects)
             (setf x-type type)
               (setf found t))))
         (problem-objects *current-problem*))
    x-type))



(defun bitmap-to-integer (bitmap)
  (let ((varint 0))
    (dotimes (ibit (length bitmap) varint)
      (when (= 1 (sbit bitmap ibit))
	(set-instance-map varint ibit 1)))))

;;===============================================================
;; Generic functions for symbolic procedures	
;;===============================================================
(defun symbolic-predicate-state (pred pred-bitmap args objects)
  (let ((predicate-list nil)
	(pred-inst (realinstances-from-bitmap pred-bitmap args objects)))
    (mapcar #'(lambda (inst)
		(push (cons pred inst) predicate-list))
	    pred-inst)
    predicate-list
))

  
(defun symbolic-functor-state (pred varsmap args objects)
  (let ((functor-list nil))
    (dotimes (i (length varsmap) functor-list)
      (when (numberp (aref varsmap i))
	(let ((var-inst (realinst-from-mappos i args objects)))
	  (push (list '= (cons pred var-inst) (aref varsmap i))
		functor-list)
	  )))))


(defun symbolic-state (state)
  (let ((symbolic-list nil)
	(predicates (dom-predicates *pspace*))
	(functors (dom-functors *pspace*))
	(objects (problem-inheritobjects *current-problem*)))
    (maphash #'(lambda (pred bitmap)
		 (if (simple-bit-vector-p bitmap)
		     (setf symbolic-list (append symbolic-list (symbolic-predicate-state pred bitmap 
											 (gethash pred predicates) objects)))
		     (setf symbolic-list (append symbolic-list (symbolic-functor-state pred bitmap 
										       (gethash pred functors) objects)))
))
	     state)
    symbolic-list
    ))

;; It returns the state literal from a pred-pos cons
;; ie. (at . 2) -> (at hoist0 depot0)
(defun literal-from-predpos (pred-pos)
  (cons (car pred-pos)
	(realinst-from-mappos (cdr pred-pos) 
			      (if (hkey-present (car pred-pos) (dom-predicates *pspace*))
				  (gethash (car pred-pos) (dom-predicates *pspace*))
				  (gethash (car pred-pos) (dom-functors *pspace*)))
			      (problem-inheritobjects *current-problem*))))




(defun get-all-types (domtypes)
  (let ((all-types nil))
    (dolist (itype domtypes all-types)
      (cond ((listp itype)
	     (unless (find (car itype) all-types)
	       (push (car itype) all-types))
	     (dolist (i-child (cadr itype))
	       (unless (find i-child all-types)
		 (push i-child all-types))))
	    (t
	     (unless (find itype all-types)
	       (push itype all-types)))))))


(defun is-object-type (ob-inst type)
  (let ((objects (problem-inheritobjects *current-problem*)))
    (when (find ob-inst (gethash type objects)) t)))



(defun get-all-objects ()
  (let  ((all-objects nil))
    (maphash #'(lambda (type objects)
		 (declare (ignore type))
		 (dolist (i-ob objects)
		   (push i-ob all-objects)))
	     (problem-objects *current-problem*))
    all-objects
    ))


(defun get-ground-types ()
  (remove-if (lambda (x) 
	       (assoc x (remove-if #'atom (dom-inheritypes *pspace*)))) 
	     (get-all-types (dom-domtypes *pspace*))))





(defun hash-keys-to-list (hash-table)
  (let ((list nil))
    (maphash #'(lambda (key item)
		 (declare (ignore item))
		 (push key list))
	     hash-table)
    (reverse list)))


;; It selects the max of a structure taking a number returned by the key
;; It is a useful implementation of the ARGMAX
(defun element-max (sequence &key (key nil))
  (cond ((functionp key)
	 (let ((max-element (car sequence))) 
	   (dolist (i (cdr sequence) max-element)
	     (when (> (funcall key i) 
		      (funcall key max-element))
	       (setf max-element i)))))
	(t (apply #'max sequence))))

;;ARGMIN
(defun element-min (sequence &key (key nil))
  (cond ((functionp key)
	 (let ((max-element (car sequence))) 
	   (dolist (i (cdr sequence) max-element)
	     (when (< (funcall key i) 
		      (funcall key max-element))
	       (setf max-element i)))))
	(t (apply #'min sequence))))



;;; string replace function
(defun string-replace (str1 sub1 sub2)
  (let ((str1 (string str1))
        (str2 "")
        (sub1 (string sub1))
        (sub2 (string sub2))
        (index1 0))
    (loop
        (if (string-equal str1 sub1
                          :start1 index1
                          :end1 (min (length str1)
                                     (+ index1 (length sub1))))
            (progn
              (setq str2 (concatenate 'string str2 sub2))
              (incf index1 (length sub1)))
            (progn
              (setq str2 (concatenate 'string
                                      str2
                                      (subseq str1 index1 (1+ index1))))
              (incf index1)))
        (unless (< index1 (length str1))
          (return str2)))))


