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

;; Using the tree grapher for arbitrary (non-class) objects

;; The grapher is not productized as a general facility
;; for incorporating into user applications, and is not documented
;; other than by this example code.  It may nevertheless be
;; useful to model after this example in order to use the
;; grapher for custom tools within the IDE.  The grapher code
;; is not present in a standalone CG application.

(in-package :cg-user)

;; ----------------------------------------------------------------
;; Sample hierarchical domain data

;; Each entry here is a list of a parent and its children.
;; The actual data could be arranged in any way, as long as
;; you can pass a function below to map from a parent to its children.
;; You would also more likely use CLOS instances rather than symbols
;; in a real application.
(defparameter *things* '((:organism :microorganism :plant :animal)
                         (:microorganism :virus :bacterium :fungus)
                         (:animal :insect :fish :reptile :bird :mammal)
                         (:feline :garfield :sylvester :bill)
                         (:mammal :feline :rodent :primate)
                         (:primate :monkey :ape :human)
                         (:human :politician :lawyer :actor :musician
                           :educator :physician :astronaut :software-developer)
                         (:rodent :nutria :hedgehog :easter-bunny)
                         (:reptile :dinosaur :snake :lizard)
                         (:plant :grass :cabbage :tree)
                         (:cabbage :green :purple :pickled)
                         (:tree :elm :larch :ginkgo)))

;; ----------------------------------------------------------------
;; Make our own subclasses for node, graph frame window, and graph pane.
;; Sorry about the internal symbols; this grapher has not been
;; officially productized.

(defclass thing-gnode (ide.grapher:generational-gnode)
  ((object :initarg :object :accessor ide.grapher:gnode-object)))

(defclass thing-graph-frame (ide.grapher:graph-frame)
  ())

(defclass thing-graph-pane (ide.grapher:cg-grapher-window-display)
  ())

;; ----------------------------------------------------------------
;; This function to return a list of the children for a given item
;; displayed in the graph will be passed below to the grapher function.
(defun thing-children (thing)
  (cdr (assoc thing *things*)))

;; Specify how to print the string in each node.
(defmethod ide.grapher:gnode-to-string  ((gnode thing-gnode))
  (substitute #\space #\-
    (capitalize-object (ide.grapher:gnode-object gnode))))

;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
;;
;; ----------------------------------------------------------------
;; Our main user function for graphing our custom things.
;;
(defun graph-subthings (thing)
  (with-hourglass
    #-runtime-system
    (ide:lisp-message "Graphing subthings of ~(~s~) ..." thing)
    
    ;; Create our graph window.
    (let* ((frame (make-window :thing-graph-frame
                    :class 'thing-graph-frame
                    :owner (development-main-window *system*)
                    :state :shrunk
                    :scrollbars nil
                    :title (format nil "Graph for ~:(~a~)" thing)))
           (pane (make-window :thing-graph-pane
                   :class 'thing-graph-pane
                   :owner frame
                   :font (make-font-ex nil "Arial" 14)
                   :right-attachment :right
                   :bottom-attachment :bottom
                   :exterior
                   (make-box	; make it fill the frame initially
                    0 0 
                    (interior-width frame)
                    (interior-height frame)))))
      
       ;; Plot the graph into the pane.
       (funcall #'ide.grapher:graph-descendants
                thing
                :direction :kid     ;; other choices are :parent and :both
                :alignment :bottom  ;; other choices are :top and :center
                :children-fn 'thing-children
                :gnode-class 'thing-gnode
                :title "foo"
                :display pane
                ;; Number of generations to show; NIL means all
                :generations nil)
       
       ;; Perhaps we should resize the window
       (ide.grapher:resize-graph-to-fit frame)
       
       ;; Initially focus on the root node         
       (let ((gnode (ide.grapher:locate-gnode-by-data pane thing)))
         (when gnode (ide.grapher:set-current-gnode pane gnode)))
       
       ;; Expose the window now that it is all ready.
       (move-window frame (make-position 300 200))
       (select-window frame)
       
       #-runtime-system
       (ide:lisp-message "~
GRAPHED the subthings of ~(~s~)       ~
Navigate with the arrow keys ~
and home/end/pageup/pagedown, and with the CONTROL key for alternate ~
behavior       Left-click the background and drag to scroll"
         thing)
       frame)))
  
;; ----------------------------------------------------------------
;; Some event handlers

(defmethod ide.grapher:gnode-data-status-message ((display thing-graph-pane)
                                                  (data t))
  ;; This is called when the user selects a node with the mouse or arrow keys.
  (ide:lisp-message "Thing ~a with children ~a"
    data (thing-children data)))

(defmethod virtual-key-down ((display thing-graph-pane) buttons data)
  (declare (ignore buttons))
  (case data
    (#.vk-space
     (ide:lisp-message "Overriding the SPACE handler for class graphs"))
    (t
     (call-next-method))))

(defmethod ide.grapher::show-children-command ((window thing-graph-frame))
  (error "show-children-command not implemented in this example."))

(defmethod ide.grapher::show-parents-command ((window thing-graph-frame))
  (error "show-parents-command not implemented in this example."))

(defun run-grapher-example ()
  (if* (standalone-application (app *system*))
     then (pop-up-message-dialog 
           (screen *system*) "Requires the IDE"
           #.(format nil "This example uses functionality ~
                    that exists only in the IDE, and so it cannot run ~
                    without the IDE running.")
           error-icon :~ok)
     else (graph-subthings :organism)))

;; ----------------------------------------------------------------

#+run-example
(run-grapher-example)
