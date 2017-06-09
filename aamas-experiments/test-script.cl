#! /home/rovane/acl100express/alisp -#!
(require :asdf)
(push "/home/rovane/shop2/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :shop2)
(load "/home/rovane/shop2/examples/toy/basic-example.lisp")
