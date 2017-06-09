(in-package :user)

(corba:define-method handlejunctionclose ((orb orblink:orb) junction)
  (format t "HandleJunctionClose: closed junction: ~s ~%" junction)
  (force-output)
)

(setf (op:handlejunctionclosepolicy corba:orb) t)
