(in-package :user)

(defclass anytest-implementation (anytest:test-servant)())


(corba:define-method testany ((this anytest-implementation) arg)
  arg
  )
