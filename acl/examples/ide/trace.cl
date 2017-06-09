;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2012 Franz Inc, Oakland, CA  All rights reserved.
;;
;; All rights reserved.
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
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;; -=End Copyright Notice=-


;;;; Trace Dialog

(in-package :cg-user)

(defun run-trace-example ()
  
  (when (standalone-application (app *system*))
    (pop-up-message-dialog 
     (screen *system*) "Requires the IDE"
     #.(format nil "This example uses the Trace Dialog and other facilities ~
                    that exist only in the IDE, and so it cannot run ~
                    without the IDE running.")
     error-icon :~ok)
    (exit -1))
  
  ;; Expose the special trace dialog so that trace data will be added
  ;; to the trace dialog as the traced functions are called, rather than
  ;; being printed to *standard-output* (the Debug window).
  (let* ((trace-dialog (ide.base:trace-dialog-command (screen *system*))))
    
    ;; "Click" the Clear button on the trace dialog to start a fresh trace.
    (toggle-button (find-component :clear trace-dialog))
    
    ;; Trace several functions involved in the creation and
    ;; resizing of a window, which we will test below.
    ;; Instead of calling TRACE explicitly, you normally can
    ;; just place the text cursor over a symbol and then invoke the
    ;; trace or profile menu commands (which have the keyboard shortcuts
    ;; of F8 and F5 respectively).
    ;; EVAL is used rather than calling the TRACE macro directly so
    ;; that the tracing is turned on at runtime rather than at compile time.
    (eval '(trace
            trace-test-go
            make-window close device-close
            select-window expand-window bring-window-to-front
            set-focus redisplay-window
            resize-window ((setf exterior))))
    
    ;; Call the test function while the desired functions are being traced.
    (trace-test-go)
    
    ;; Turn all tracing back off.  We turned this on and off
    ;; programatically to avoid the nuisance of tracing frequent events
    ;; outside of our specific test.
    (eval '(untrace))))
   
(defparameter *message-names*
  (let* ((items nil))
    #+mswindows
    (do-symbols (sym (find-package :windows))
      (when (and (>= (length (symbol-name sym)) 3)
                 (string-equal (symbol-name sym) "WM_" :end1 3)
                 (boundp sym))
        (unless (member sym '(win:WM_MOUSEMOVE win:WM_MOUSELAST)
                        :test #'eq)
          (push (list (symbol-value sym) sym 0)
                items))))
    items))

(defun trace-test-go ()
  
  ;; Create and resize a sample test window while tracing or
  ;; profiling of various functions is turned on.
  (let* ((window (make-window :trace-test
                   :class 'frame-window 
                   :owner (development-main-window *system*)
                   :exterior (make-box 20 420 400 450)
                   :title "My creation and resizing were traced!"))
         
         ;; Add a method to our test window on the fly to take note
         ;; of every message that the OS sends to this window.
         ;; Note that it would be unfeasible to do this for ALL windows
         ;; due to the large number of events coming in.
         #+mswindows
         (tracer-method
          (defmethod cg.base:window-procedure
              :before ((window (eql window)) 
                       message wparam lparam)
            (let* ((message-name (second (assoc message *message-names*
                                                :test #'eq))))
              ;; Insert a line into the trace output for each
              ;; message that comes in for this window.
              (when message-name
                (ide:trace-format "Msg ~a ~a ~a"
                  message-name wparam lparam))))))
    
    ;; Insert a message between the traced calls as a milestone of where we
    ;; are.
    (ide:trace-format "After CREATION of ~a" window)
    
    ;; Resize the window thrice.      
    (dotimes (j 3)
      (ide:with-trace-color ((case j
                               (0 (make-rgb :red 255 :green 255 :blue 202))
                               (1 (make-rgb :red 236 :green 255 :blue 216))
                               (2 (make-rgb :red 255 :green 240 :blue 220))))
        (resize-window window (make-position (+ 300 (* j 50))(+ 30 (* j 30))))
        (ide:trace-format "After RESIZE #~a of ~a" (1+ j) window)))
    
    ;; Get rid of the method dynamically defined above just to
    ;; keep methods from proliferating.
    #+mswindows
    (remove-method #'cg.base:window-procedure tracer-method)
    
    #+maybe
    ;; If we do this we won't be able to inspect the window
    ;; from the trace dialog, since it would then be a CLOSED-STREAM.
    (close window)))

;; Since this example is tracing messages passed to pc:window-procedure,
;; find all of the Windows API message names, which are all
;; in the WINDOWS package and begin with "WM_" so that
;; we can report them by name rather than by number

#+run-example (run-trace-example)
