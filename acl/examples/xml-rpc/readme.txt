
;;
;; copyright (c) 2002 Franz Inc, Berkeley, CA
;;

;; $Id: readme.txt,v 2.2 2005/09/22 20:28:46 layer Exp $


WHAT IS THIS?

This directory contains files that define and exercise the XML-RPC
protocol that allows remote procedure invocation over an http
connection.

More extensive documentation is in the file xml-rpc.txt.



PREREQUISITES:

ACL 6.0 or higher

Allegro Serve - there is a version distributed with ACL

XmlUtils - This is an Open Source component that may be downloaded
           from the Franz web site:

           1. download sources to ./xmlutils
           2. compile in ACL with the command:   :ld xmlutils/build.cl
          


HOW TO RUN EXAMPLES:

  (require :xml-rpc)
  ;; Compile and load examples:
  :cl examples/xml-rpc/xml-rpc-examples

With an open web connection, try some public servers with the
forms:  (user::try1a)  (user::try1b)   (user::try2)  ...

Look in the file xml-rpc-examples.cl for more things to try.
