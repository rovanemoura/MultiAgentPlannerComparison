#|
========================
File: dmap.lisp
Author: Vladimir Kulyukin
Description: Direct Memory Access Parsing
Copyright (c) 1993-94, 1997-98

Acknowledgments:

Charles Martin introduced me to Direct Memory 
Access Parsing and explained to me how it works.

Chris Riesbeck's code for DMAP-LITE served
as an excellent starting point and a guide
for further development.

Comments and bugs to vkulyukin@cs.depaul.edu
========================
|#

(eval-when (load eval compile)
  (unless (find-package :ir)
    (make-package :ir)))

(in-package  :ir)
(use-package :cl-user)

(export '(process-tokens make-dmap add-callback clear-expectations))
(import '(cl-user::deftable cl-user::to-symbols))

(defvar *dmap* nil)

(deftable static-expectations)
(deftable dynamic-expectations)
(deftable callbacks)
(deftable answers)

(defclass expectation ()
  ((frame :initarg :frame :initform nil :accessor frame)
   (tok-seq :initarg :tok-seq :initform nil :accessor tok-seq)
   (slots :initarg :slots :initform nil :accessor slots)))

(defclass static-expectation  (expectation) ())

(defclass dynamic-expectation (expectation) 
  ((init-frame :initarg :init-frame :initform nil :accessor init-frame)
   (active-p   :initarg :active-p :initform t :accessor active-p)))

(defmethod print-object ((exp static-expectation) stream)
  (with-slots (frame tok-seq slots) exp
    (print-unreadable-object
     (exp stream :type t :identity t)
     (format stream "~S ~S ~S" frame tok-seq slots))))

(defmethod print-object ((exp dynamic-expectation) stream)
  (with-slots (frame tok-seq slots init-frame) exp
    (print-unreadable-object
     (exp stream :type t :identity t)
     (format stream "~S ~S ~S ~S" frame tok-seq slots init-frame))))

(defun make-static-expectation (&key frame tok-seq slots)
  (make-instance 'static-expectation
                 :frame frame :tok-seq tok-seq :slots slots))

(defun make-dynamic-expectation (&key frame tok-seq slots init-frame)
  (make-instance 'dynamic-expectation
                 :frame frame :tok-seq tok-seq :slots slots
                 :init-frame init-frame
		 :active-p t))

(defclass dmap ()
  ((frame-manager :initarg :frame-manager 
                  :accessor frame-manager)
   (static-expectations :initarg :static-expectations 
                        :initform *static-expectations-table*
                        :accessor static-expectations)
   (dynamic-expectations :initarg :dynamic-expectations 
                         :initform *dynamic-expectations-table*
                         :accessor dynamic-expectations)
   (callbacks :initarg :callbacks :initform *callbacks-table*
              :accessor callbacks)
   (answers :initarg :answers :initform *answers-table*
            :accessor answers)
   (referenced-frames :initarg :referenced-frames
                      :accessor referenced-frames)))

(defun make-dmap (&key frame-manager)
  (make-instance 'dmap :frame-manager frame-manager))

(defmethod current-token ((exp expectation))
  (first (tok-seq exp)))

(defmethod add-expectation ((dm dmap) (exp dynamic-expectation))
  (with-slots (dynamic-expectations) dm
    (push exp 
          (get-dynamic-expectations (expected-token dm exp)
                                    dynamic-expectations))))

(defmethod add-expectation ((dm dmap) (exp static-expectation))
  (with-slots (static-expectations) dm
    (push exp
          (get-static-expectations (expected-token dm exp)
                                   static-expectations))))

(defmacro deftokseq (frame &rest tok-seq)
  `(define-token-seq ,*dmap* ',frame ',tok-seq))

(defmethod define-token-seq ((dm dmap) (frame t) (tok-seq list))
  (unless (and (eql frame (first tok-seq))
               (null (rest toke-seq)))
    (let ((exp (make-static-expectation :frame frame
                                        :tok-seq tok-seq)))
      (add-expectation dm exp))))

(defmethod expected-token ((dm dmap) (exp expectation))
  (with-slots (frame-manager) dm
    (with-slots (frame tok-seq) exp
      (let ((tok (first tok-seq)))
        (if (attribute-specifier-p tok)
            (let ((val (frame-and-attribute-to-inherited-value
                        frame-manager frame tok)))
              (if val
                  val
                (error "~S is not an attribute in ~S" tok frame)))
          tok)))))

(defun attribute-specifier-p (x)
  (and (keywordp x)
       (let ((sn (symbol-name x)))
         (and (char= (char sn 0) #\=)
              (char= (char sn 1) #\=)))))

(defmethod expectations-on-token ((dm dmap) (tok t))
  (with-slots (static-expectations dynamic-expectations) dm
    `(,@(get-static-expectations tok static-expectations)
      ,@(filter-dynamic-expectations
	 (get-dynamic-expectations tok dynamic-expectations)))))

(defun filter-dynamic-expectations (dyn-exps)
  (remove-if-not #'(lambda (exp) (active-p exp)) dyn-exps))

(defmethod process-tokens ((dm dmap) (tokens list) &optional package)
  (declare (ignore package))
  (dolist (tok tokens) (reference-token dm tok)))

(defmethod reference-token ((dm dmap) (tok t))
  (with-slots (frame-manager) dm
    (dolist (abst (get-all-absts frame-manager tok))
      (dolist (exp (expectations-on-token dm abst))
        (advance-expectation dm exp tok)))))

(defmethod advance-expectation ((dm dmap) 
                                (exp static-expectation) 
                                (tok t))
  (with-slots (frame-manager) dm
    (with-slots (frame tok-seq slots) exp
        (let ((cur-tok (first tok-seq))
              (advanced-tok-seq (rest tok-seq))
              (new-slots (extend-slots dm exp tok)))
          (if (null advanced-tok-seq)
              (let ((spec-frame (abst-and-slots-to-frame
                                 frame-manager frame new-slots)))
                (run-callbacks dm spec-frame)
                (reference-token dm spec-frame))
            (unless (dynamic-expectation-initiated-p dm cur-tok
                                                     advanced-tok-seq)
              (let ((dexp (make-dynamic-expectation :frame frame
                                                    :tok-seq advanced-tok-seq
                                                    :slots new-slots
                                                    :init-frame tok)))
                (add-expectation dm dexp))))))))

(defmethod advance-expectation ((dm dmap) 
                                (exp dynamic-expectation) 
                                (tok t))
  (with-slots (frame-manager) dm
    (with-slots (frame tok-seq slots init-frame) exp
        (unless (same-frame-p frame-manager frame tok)
          (let ((advanced-tok-seq (rest tok-seq))
                (new-slots (extend-slots dm exp tok)))
            (cond
             ((null advanced-tok-seq)
              (let ((spec-frame (abst-and-slots-to-frame
                                 frame-manager frame new-slots)))
                (unless (eql spec-frame frame)
                  (run-callbacks dm frame))
                (run-callbacks dm spec-frame)
                (remove-expectation dm exp tok)
                (reference-token dm spec-frame)))
             (t 
              (let ((dexp (make-dynamic-expectation :frame frame
                                                    :tok-seq advanced-tok-seq
                                                    :slots new-slots
                                                    :init-frame init-frame)))
                (add-expectation dm dexp)))))))))

(defmethod dynamic-expectation-initiated-p ((dm dmap) (tok t) (tokens null))
  (declare (ignore dm) (ignore tok) (ignore tok-seq))
  nil)

(defmethod dynamic-expectation-initiated-p ((dm dmap) 
                                            (possible-initiator-tok t)
                                            (tokens cons))
  (let ((next-tok (first tokens)))
    (with-slots (dynamic-expectations frame-manager) dm
      (some #'(lambda (dexp)
                (with-slots (tok-seq init-frame) dexp
                  (and (equal tok-seq tokens)
                       (abstp frame-manager init-frame
                              possible-initiator-tok))))
            (get-dynamic-expectations next-tok dynamic-expectations)))))
  
(defmethod run-callbacks ((dm dmap) (frame t))
  (with-slots (callbacks) dm
    (dolist (clb (get-callbacks frame callbacks))
      (funcall clb frame))))

(defmethod remove-expectation ((dm dmap) 
                               (exp dynamic-expectation)
                               (tok t))
  (with-slots (dynamic-expectations) dm
    (multiple-value-bind (dexps bool)
       (get-dynamic-expectations tok dynamic-expectations)
       (declare (ignore bool))
       (setf (get-dynamic-expectations tok dynamic-expectations)
             (delete exp dexps))
       (multiple-value-bind (dexps bool)
         (get-dynamic-expectations tok dynamic-expectations)
         (declare (ignore bool))
         (when (null dexps)
               (rem-dynamic-expectations tok dynamic-expectations))))))

(defmethod remove-expectation :after ((dm dmap)
                                      (exp dynamic-expectation)
                                      (tok t))
  (map-dynamic-expectations 
   #'(lambda (key exp-list)
       (loop for e in exp-list
	       when (eql (frame e) (frame exp))
	       do (setf (active-p e) nil)))
   (dynamic-expectations dm)))

(defmethod extend-slots ((dm dmap) (exp expectation) (token t))
  (with-slots (frame-manager) dm
    (with-slots (token-seq slots) exp
      (let ((curr-tok (current-token exp)))
        (if (attribute-specifier-p curr-tok)
            (if (abstp frame-manager 
                       token 
                       (expected-token dm exp))
                slots
              (cons (make-slot curr-tok token)
                    slots))
          slots)))))

(defun init-dmap (&key frames tokseqs)
  (setf *dmap* (make-dmap :frame-manager
                          (init-frame-manager :frames frames)))
  (load tokseqs)
  *dmap*)

(defmethod add-answer-callbacks ((dm dmap) (file-path string))
  (toggle-answers dm file-path)
  (toggle-answer-callbacks dm))

(defmethod toggle-answers ((dm dmap) (file-path string))
  (with-open-file (in file-path :direction :input)
    (with-slots (answers) dm
      (let ((frame-to-str-sexps (read in)))
        (dolist (frame-to-str-sexp frame-to-str-sexps)
          (put-answers (first frame-to-str-sexp)
                       (second frame-to-str-sexp)
                       answers))))))

(defmethod toggle-answer-callbacks ((dm dmap))
  (with-slots (callbacks answers) dm
    (map-answers #'(lambda (frame answer-string)
                       (declare (ignore answer-string))
                       (push #'(lambda (tok)
                                 (add-referenced-frame dm tok))
                             (get-callbacks frame callbacks)))
                 answers)))

(defmethod add-referenced-frame ((dm dmap) (frame t))
  (with-slots (referenced-frames) dm
    (pushnew frame referenced-frames)))

(defmethod collect-answer-strings ((dm dmap))
  (with-slots (referenced-frames frame-manager answers) dm
    (labels ((frame-compare (f1 f2)
               (if (abstp frame-manager f1 f2)
                   nil
                 t)))
       (loop for frame in (stable-sort referenced-frames #'frame-compare)
             collect (list frame 
                           (get-answers frame answers))))))

(defmethod answer-query ((dm dmap) (query string))
  (let ((symbols (to-symbols query)))
    (answer-query dm symbols)))

(defmethod answer-query ((dm dmap) (query list))
  (process-tokens dm query)
  (when (null (referenced-frames dm))
        (format t "No frames referenced...~%"))
  (let ((answers (collect-answer-strings dm)))
    (clear-referenced-frames dm)
    answers))

(defmethod clear-referenced-frames ((dm dmap))
  (setf (referenced-frames dm) nil))

(defun answer-remote-query (query-string)
  (let* ((syms (to-symbols query-string :dmap))
         (answer (answer-query *dmap* syms)))
    (clear-expectations *dmap* :dynamic)
    (if answer 
        (second (first answer))
      "No answer found.")))

;;; ============= Debugging Tools ===================

(defmethod clear-frame-memory ((dm dmap))
  (with-slots (frame-manager) dm
    (clear-frame-memory frame-manager)))

(defmethod clear-expectations ((dm dmap) &optional (type :dynamic))
  (ecase type
    (:dynamic (clear-dynamic-expectations (dynamic-expectations dm)))
    (:static  (clear-static-expectations  (static-expectations  dm)))
    (:all     (clear-dynamic-expectations (dynamic-expectations dm))
              (clear-static-expectations  (static-expectations  dm)))))

(defmethod remove-callbacks ((dm dmap))
  (with-slots (callbacks) dm
    (clear-callbacks callbacks)))

(defmethod clear-dmap ((dm dmap))
  (clear-frame-memory dm)
  (clear-expectations dm :all)
  (remove-callbacks dm)
  t)

(defmethod add-default-callbacks ((dm dmap))
  (with-slots (callbacks frame-manager) dm
    (with-slots (frame-to-slots) frame-manager
      (map-callbacks #'(lambda (key val)
                         (declare (ignore val))
                         (push #'(lambda (tok)
                                   (declare (ignore tok))
                                   (format t "~A referenced...~%" key))
                               (get-callbacks key callbacks)))
                     frame-to-slots)
      t)))

(defmethod add-callback ((dm dmap) callback)
  (with-slots (callbacks frame-manager) dm
    (with-slots (frame-to-slots) frame-manager
      (map-callbacks #'(lambda (key val)
                         (declare (ignore val))
                         (push callback (get-callbacks key callbacks)))
                     frame-to-slots))))

(defmethod print-expectations ((dm dmap) 
                               &optional (type :dynamic)
                                         (stream t))
  (labels ((displayer (key val)
             (format stream "~S --> ~S~%" key val)))
    (ecase type
      (:dynamic (map-dynamic-expectations #'displayer
                                          (dynamic-expectations dm)))
      (:static (map-static-expectations #'displayer
                                        (static-expectations dm))))))

(defmethod check-dynamic-frame-count ((dm dmap) (tok t) n)
  (let ((count 0))
    (with-slots (dynamic-expectations) dm
      (maphash #'(lambda (key exps)
                   (declare (ignore key))
                   (when (>= (count tok exps :key #'frame)
                             n)
                         (break)))
               dynamic-expectations))))

(defmethod print-frames ((dm dmap))
  (with-slots (frame-manager) dm
    (with-slots (frame-to-slots) frame-manager
       (maphash #'(lambda (frame slots)
                    (format t "[ ~S <--> ~A ]~%" frame slots))
                frame-to-slots))))

;;; end-of-file
