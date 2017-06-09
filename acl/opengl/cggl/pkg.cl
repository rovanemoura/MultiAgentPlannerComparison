(in-package :cg-user)

(defpackage :cggl
  (:use :cl :cg :wgl :ff)
  (:export #:cggl-mixin
           #:current-cggl-stream
           #:exit-cggl
           #:cggl-double-buffering
           #:pixel-format-descriptor-custom-values))

(defpackage :cg-user
  (:use :cggl))

