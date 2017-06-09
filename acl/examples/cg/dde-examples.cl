;; Example use of the CLOSsiffied DDE facility

;; All rights reserved.
;; copyright (c) 2002-2013 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: dde-examples.cl,v 1.5 2007/04/17 21:32:40 layer Exp $

;;
;; Permission is granted only to any individual or institution which has
;; current Allegro CL license(s) to use, copy, or modify this software,
;; provided any reproduction or distribution of binary versions of this
;; software are compiled with a licensed Allegro CL, and provided
;; that this complete copyright and permission notice is maintained, intact,
;; in all copies and supporting documentation. 
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252 52.227-7013 (c) (1) (ii), as
;; applicable.

(in-package :user)

(defpackage :user
  (:use :dde))

;; ---------------------------------------------------------------------
;; Test this lisp running as a DDE client.

(defun client-test (&optional (application :progman)
                              (topic :progman)
                              (item :groups)
                              (timeout 1000))
  
  ;; Open a client port into a topic of the application.
  ;; The default talks to the program manager.
  (let* ((port (make-instance 'client-port
                 :application application
                 :topic topic)))
    ;; Connect to the DDE server
    (when (open-port port)
      ;; Add a program manager icon labelled "Test" that
      ;; starts up the notepad applet
      
      ;; NOTE: With the Windows 95 shell, this adds a "Test" shortcut
      ;; under the Start menu.  Windows 95 and Windows NT 4.0 behavior
      ;; seem to be a little different on this point, though.  Windows 95
      ;; may add the shortcut under "Programs".  Windows NT 4.0 may add the
      ;; shortcut under "Programs -> Allegro CL for Windows".
      ;; cac 10oct96.
      
      (when (eq application :progman)
        
        ;; This would cause the item to be added under
        ;; Programs -> Allegro 6.0, also showing an explorer window
        ;; with the new icon in it
        ;; (send-command port "[ShowGroup(Allegro CL 6.0,5)]")
        
        (send-command
         port
         "[AddItem(notepad.exe,Added by the Allegro DDE Example)]"
         :timeout timeout))
      (prog1
          ;; Request an item from the server.
          ;; The default is :groups, which asks progman what the
          ;; names of all of its group windows are
          (send-request port item :timeout timeout)
        (close-port port)))))

;; ---------------------------------------------------------------------
;; Test another lisp instance which is running as a DDE server.
;; First call (open-server) in that lisp, and then call (server-test)
;; in this lisp

(defun server-test ()
  (let ((port (make-instance 'client-port
                :application :allegro
                :topic :eval)))
    ;; Connect to the lisp server
    (open-port port)
    (terpri t)
    
    ;; Send some commands to another lisp
    (dolist (form '(
                    (+ 10 12)
                    (defvar *foo*)
                    (setq *foo* 10)
                    (incf *foo*)
                    ))
      (format t "Remote-eval: ~s~%" (format nil "~s" form))
      (send-command port 
                    (format nil "~s" form))
      (format t "   => ~s~2%"
        (send-request port :command-result))
      )
    
    ;; Test a poke
    (format t "Poke 37 into *foo*, returning ~s~%"
      (send-value port :*foo* "37"))
    (format t "*foo* => ~s~%"
      (send-request port :*foo*))
    
    (close-port port)
    t))

;; ---------------------------------------------------------------------
;; DDE receive-advice example

;; Start running a blank Microsoft Excel.
;; Compile this code and say (setq port (excel-test))
;; When done testing, close the port with (close-port port)

;; This example sets up a "hot" advice link with the Excel DDE server.
;; The link applies only to the top-left cell in the spreadsheet
;; plus the cell just to the right of that.  Whenever you change the
;; value in one of those cells, the Excel DDE server sends us a piece
;; of "advice" which results in a call to the receive-advice method
;; that we supply here for this port.  This simple example
;; simply prints whatever Excel passed to us.

;; The example also shows how to read and write individual cells.

(defclass excel-client (client-port)())

(defun excel-test ()
  
  ;; Make a system port to find what all of Excel's topics are.
  (let* ((system-port (open-port (make-instance 'excel-client
                                   :application "Excel"
                                   :topic "System")))
         
         ;; The topics for Excel may include spreadsheet names
         ;; and other unknown stuff.
         (topics (send-request system-port "Topics"))
         
         ;; Find the first topic name that has the string "Sheet"
         ;; in it, and assume that it is the name of an open sheet.
         (sheet-name (find "Sheet" topics :test #'search))
         
         ;; Open a DDE port for that particular sheet.
         (port (open-port (make-instance 'excel-client
                            :application "Excel"
                            :topic sheet-name))))
    
    ;; Ask for advice whenever row1 column1 or row1 column2 changes.
    (send-request port "R1C1" :link :hot)
    (send-request port "R1C2" :link :hot)
    port))

(defmethod receive-advice ((port excel-client) topic item string)
  
  ;; This is our advice-receiving method, which simply prints the
  ;; information that Excel passed to us.
  ;; A real application would add code here to process the data.
  (format t
      "~&Advice ~s received for port ~a topic ~a item ~a~%"
    string (port-name port) topic item)
  (force-output))

#| Other Excel DDE commands.

(setq port (excel-test))

;; This form returns a list of the string that's in
;; row 2 column 1 right now.
(send-request port "R2C1")

;; This form writes a string into a particular cell.
(send-value port "R2C2" "37")

(defmethod convert-returned-dde-string ((port excel-client) string)
  (setq string (string-right-trim '(#\tab #\newline) string))
  (if* (find #\tab string)
     then (if* (find #\newline string)
             then (mapcar (lambda (s)
                            (delimited-string-to-list s #\tab))
                    (delimited-string-to-list string #\newline))
             else (delimited-string-to-list string #\tab))
     else (delimited-string-to-list string #\newline)))

;; This form returns a tree of four cells.
(send-request port "r1c1:r2c2")

;; This creates a new worksheet.
(send-command port "[New(1)]")

;; This (somehow) creates a chart from the first ten values
;; in the first row.
(send-command port "[Select(\"R1C1:R1C10\")][New(2,2)]")

;; This makes the font bold in the third cell of the first column.
(send-command port "[SELECT(\"R3C1\")][FONT.PROPERTIES(,\"Bold\")]")

;; This changes the port's topic to "System" and returns a list
;; of all topics that may be passed.
(close-port port)
(setf (port-topic port) "System")
(open-port port)
(send-request port "Topics")

;; This returns a list of all item strings that may be passed
;; to send-request when the port's topic is "System".  This and
;; "Topics" are a standard "System" topic requests that DDE servers
;; generally support.
(send-request port "SysItems")

;; The above query informs us that we can pass "Selection"
;; to the system topic.  Doing so returns a string indicating
;; the range of selected cells.
(send-request port "Selection")

|#

;; ---------------------------------------------------------------------
