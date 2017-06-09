(in-package :user)

(defvar *anytest-root* *load-pathname*)

(excl:defsystem :anytest
    (:default-pathname #.*load-pathname*)
  (:serial
   "anytest-implementation.cl"
   "anytest-invoke.cl"
   )
  )

(defvar anytest-repository)

(defun load-anytest ()
  (setq anytest-repository (corba:idl (merge-pathnames "../idl/anytest.idl" *anytest-root*)))
  (excl:compile-system :anytest)
  (excl:load-system :anytest)
  )

(load-anytest)

