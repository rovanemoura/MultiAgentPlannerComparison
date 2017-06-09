
;; $Id: bean1gen.cl,v 5.0 2004/01/14 18:31:35 layer Exp $

(in-package :user)

;; This file generates the Java definition of the classes
;; LispButton, LispBean, and LispBeanBeanInfo

(def-java-to-lisp-wrapper
  (
   ;; the Lisp class name of the remote reference to 
   ;;  the Java class instance
   lisp-button 

   ;; the name of the Java class
   "LispButton" 

   ;; the Java super-class
   ("LispButtonBase") 

   ;; the attributes of the Java class
   :public

   )
  (
   
   ;; this is where the Java class definition will be written
   :java-file "LispButton.java"
	      :if-exists :supersede
	      
	      ;; this is how the Java class methods will connect
	      ;;  to Lisp
	      :connector ("MyConnector.go" |false| |null|)
	      )

  ;; The method definitions

  (
   (
    ;; the name of the Lisp method that will be called
    fire-action 

    ;; the name of the Java method
    "fireAction"
    ) 

   ;; the argument types of the Java method
   () 
   
   ;; the Java type of the value returned by the Java method
   "void" 

   ;; the Java attributes of the Java method
   :public :oneway
   )

  ;; more methods
  ((set-debug "setDebug") ("boolean") "void" :public)
  ((get-debug "getDebug") () "boolean" :public)
  ((set-large-font "setLargeFont") ("boolean") "void" :public)
  ((is-large-font "isLargeFont") () "boolean" :public)
  ((set-font-size "setFontSize") ("int") "void" :public)
  ((get-font-size "getFontSize") () "int" :public)
  ((set-label "setLabel") ("String") "void" :public)

  )



(def-java-to-lisp-wrapper
  (lisp-bean "LispBean" () :public)
  (:java-file "LispBean.java"
	      :if-exists :supersede
	      
	      ;; this is how the Java class methods will connect
	      ;;  to Lisp
	      :connector ("MyConnector.go" |false| |null|)
	      )
  
  ;; The method definitions

  ((start-action "startAction") 
   ("java.awt.event.ActionEvent") "void"  :public )
  ((stop-action "stopAction") ("java.awt.event.ActionEvent") "void" :public)
  ((get-debug "getDebug") () "boolean" :public)
  ((set-debug "setDebug") ("boolean") "void" :public)
  ((get-rate  "getRate")  () "int" :public)
  ((get-rate  "setRate")  ("int") "void" :public)
  ((get-prop  "fetchProp") () "String" :public)
  ((get-prop  "storeProp") ("String") "void" :public)
  )


(def-java-to-lisp-wrapper
  (lisp-bean-info "LispBeanBeanInfo" ("java.beans.SimpleBeanInfo") :public)
  (:java-file "LispBeanBeanInfo.java"
	      :if-exists :supersede
	      :connector ("MyConnector.go" |false| |null|)
	      :java-slots
	      ( 
	       ;; We add this slot to avoid ref by name to classes
	       ;; in the BeanBox since these dont appear in CLASSPATH
	       ;; and thus appear undefined.
	       ((bean-class beanClass :type "Class" 
			    :initform (ref "LispBean.class")
			    :flags (:public))
		:reader bean-class)
		)
	      )

  ((get-prop-descs "getPropertyDescriptors")
   () (|array| "java.beans.PropertyDescriptor" *) :public)
  ((get-prop-index "getDefaultPropertyIndex")
   () "int" :public)
  )
