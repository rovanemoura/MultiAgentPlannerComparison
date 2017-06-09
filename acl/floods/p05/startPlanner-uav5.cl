#! /home/rovane/acl/alisp -#!
(require :asdf)
(push "/home/rovane/shop2/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :shop2)
(load "/home/rovane/floods/p05/uav5")
#(define-floods-uav-domain)
#(load "/home/rovane/floods/p01/uav1")
#(find-plans 'uav1 :verbose :plans)
# (find-plans '$PROBLEM_NAME :which :shallowest :time-limit 10)
