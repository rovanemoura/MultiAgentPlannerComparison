(in-package :user)
(defclass test-implementation (idltest:test-servant)
  ((message :accessor get-message)))
(defclass a-implementation (idltest:a-servant)())
(defclass b-implementation (idltest:b-servant)())
(defclass c-implementation (idltest:c-servant)())
(defclass d-implementation (idltest:d-servant)())

(defclass anytest-implementation (idltest:anytest-servant)())
(corba:define-method testany((this anytest-implementation) value)
  (format t "anytest::testany: got value of: ~s~%" value)
  (force-output)
  value)

(defclass locfor-implementation (idltest:locfor-servant)())
(corba:define-method test_location_forward ((this locfor-implementation) location)
  (format t "test_location_forward called on id of: ~d to id of: ~d~%" (op:id this) (op:id location))
  (force-output)
  (cond
   ((eql (op:id this) (op:id location)) (op:id this))
   (t (op:_forward this location))))

(corba:define-method forward_to_id ((this locfor-implementation) id)
  (cond
   ((eql (op:id this) id) id)
   (t (op:test_location_forward this (make-instance 'locfor-implementation :id id)))))

(corba:define-method testreturningobject ((this test-implementation) arg)
  this)

(corba:define-method testmarshallingobject((this test-implementation) bar)
  (declare (ignore bar))
  )

(corba:define-method create_locfor ((this test-implementation) arg)
  (make-instance 'locfor-implementation :id arg))
(corba:define-method op_for_a ((this a-implementation) arg) arg)
(corba:define-method op_for_b ((this b-implementation)) 30000)
(corba:define-method op_for_c ((this c-implementation)) #\c)
(corba:define-method op_for_d ((this c-implementation)) t)
(corba:define-method op_for_a ((this d-implementation) arg) (concatenate 'string "d-implementation of: " arg))
(corba:define-method op_for_a :around
  ((this d-implementation) arg)
  (format t "d-implementation , around method, called with arg: ~s~%" arg)
  (force-output)
  (if (equal arg "stop") "foo"
    (call-next-method)))



(defmacro testmethod (implementationclass methodname &rest args)
  `(corba:define-method ,methodname ((this ,implementationclass) ,@args)
     (format t "~A: got arguments of: ~A~%" ',methodname (list ,@args) )
     (force-output)
     ,(when args (car args))))

(corba:define-method testobject ((this test-implementation) argument)
  (format t "testobject: got argument of: ~s~%" argument)
  (force-output)
  argument
)

(corba:define-method testobjectshort ((this test-implementation) argument)
  (format t "testobject: got argument of: ~s~%" argument)
  (force-output))

(defmacro testimpl (&rest args) `(testmethod test-implementation ,@args))

(testimpl testunsignedshort a)
(testimpl testshort a)
(testimpl testlong a)
(testimpl testlonglong a)
(testimpl testunsignedlong a)
(testimpl testunsignedlonglong a)
(testimpl teststructexample a)
(testimpl testenum a)
(testimpl testrecursivestruct a)
(testimpl testoctet a)
(testimpl testlongarray a)
(testimpl testfloat a)
(testimpl testdouble a)
(testimpl testchar a)
(testimpl testboolean a)
(testimpl testsequence a)
(testimpl testunionexample a)
(testimpl testunionenumexample a)
(testimpl testarray2 a)
(testimpl testarray3 a)
(testimpl testarray4 a)


(corba:define-method teststring ((this test-implementation) string)
  (unless (typep string 'string) (error "unexpected argument to teststring"))
  (format t "teststring: got string of length ~d first 50 characters: ~s~%"
	  (length string)
	  (subseq string 0 (min 50 (length string))))
  string)

(corba:define-method testout1 ((this test-implementation))
  456)

(corba:define-method testout2 ((this test-implementation))
  (values 456 457))

(corba:define-method testout3 ((this test-implementation) arg)
  (format t "testout3: got arg of: ~s~%" arg) (force-output)
  #\b)

(corba:define-method testout4 ((this test-implementation) arg0)
  (format t "testout4: got arg0 of: ~s~%" arg0)
  (force-output)
  (values nil #\c))

(corba:define-method testout6 ((this test-implementation) a c d e f )
  (format t "testout 6 got args of: ~s ~s ~s ~s ~s: ~%" a c d e f)
  (force-output)
  (values "result" "a" "b" "d" "f" "g"))
(corba:define-method testveryvoid((this test-implementation))
  (values)
  )
(corba:define-method testout5 ((this test-implementation) arg0)
  (format t "testout5: got arg0 of: ~s~%" arg0)
  (force-output)
  (values 900000 arg0 "Final arg"))


(corba:define-method onewaytestvoid ((this test-implementation) delay)
  (format t "onewaytestvoid: delaying for: ~s seconds~%" delay)
  (force-output)
  (sleep delay)
  (format t "onewaytestvoid: delayed for: ~s seconds~%" delay)
  (force-output)
  )

(corba:define-method testexception ((this test-implementation) a)
  (cond
   ((plusp a) a)
   ((zerop a) (/ 1 0))
   (t
    (signal (idltest:exceptionexample :member1 a)))))

(corba:define-method testvoid ((this test-implementation) n m)
  (format t "testvoid: got n: ~S and m: ~S~%" n m)
  (force-output)
  )

(corba:define-method testmessage ((this test-implementation) delay)
  (sleep delay)
  (format t "testmessage: got message of: ~s~%" orblink:*message* this)
  (force-output)
  (setf (get-message this) orblink:*message*)
  )

