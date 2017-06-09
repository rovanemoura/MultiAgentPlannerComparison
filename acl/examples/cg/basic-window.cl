;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2015 Franz Inc, Oakland, CA  All rights reserved.
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

;; Basic Drawing and Mouse-Handling

;; This example defines a window that draws node circles
;; with link lines connecting them.
;; Clicking the window adds another circle
;; plus a link line that links it with the previous circle.

(in-package :cg-user)

(defclass basic-example-window (frame-window)
  
  ;; Define a window subclass so that we can store values
  ;; in custom slots, and so that we can define methods
  ;; for our window below.
  
  ((nodes :accessor nodes :initarg :nodes :initform nil)
   (links :accessor links :initarg :links :initform nil))
  (:default-initargs
      
      ;; This is important for eliminating annoying flashing effects
      ;; when updating the contents of windows.
      :double-buffered t
    
    :scrollbars nil
    :font (make-font-ex nil "Arial" 14)))

(defmethod redisplay-window ((window basic-example-window) &optional clipping-box)
  (declare (ignore clipping-box))
  
  ;; Our redisplay-window method will get called automatically
  ;; whenever the window gets uncovered, to redraw its contents.
  
  ;; The default method fills the background with the background color.
  (call-next-method)
  
  (let* ((text-height (line-height window))
         (nodes (nodes window))
         node1 node2 x y radius)
    
    ;; Use a thicker line width than the default of 1 pixel.
    (with-line-width (window 2)
      
      ;; Draw the links.  Do these first so that nodes can cover them.
      (dolist (link (links window))
        (setq node1 (first link))
        (setq node2 (second link))
        
        ;; Draw each link line between the centers of the
        ;; two nodes that it links.
        (draw-line-x-y window (first node1)(second node1)
                       (first node2)(second node2)))
      
      ;; Draw the node circles.
      (dolist (node nodes)
        (setq x (first node))
        (setq y (second node))
        (setq radius (third node))
        
        ;; This macro is just for efficiency, to avoid consing
        ;; position objects in a loop.
        (with-positions (center)
          
          ;; This fills in the temporary position that we
          ;; allocated above with node's coordinates.
          (nmake-position center x y)
          
          ;; Erase link lines where they intesect this node by
          ;; filling the circle's interior with the background color.
          (erase-contents-circle window center radius)
          
          ;; Draw the node circle itself.
          (draw-circle window center radius)
          
          ;; Label the node with its center coordinates.
          (with-boxes (string-box)
            (nmake-box-relative string-box
              (- x radius)(- y (floor text-height 2))
              (* 2 radius) text-height)
            (draw-string-in-box
             window (format nil "~a, ~a" (first node)(second node))
             nil nil string-box :center :top)))))))

(defmethod add-node ((window basic-example-window) &key x y)
  
  ;; This adds a new new node and links it with the previous node.
  
  (let* ((width (interior-width window))
         (height (interior-height window))
         (nodes (nodes window))
         (previous-node (first nodes))
         (radius (+ 40 (random 20)))
         (diameter (* 2 radius))
         (new-node (list (or x (+ radius (random (- width diameter))))
                         (or y (+ radius (random (- height diameter))))
                         radius))
         (new-link (and previous-node
                        (list previous-node new-node))))
  
    ;; Add the specified node to the set of nodes that the
    ;; window is now drawing.
    (push new-node (nodes window))
    
    (when new-link
      (push new-link (links window)))
    
    ;; Cause the redisplay-window method to get called to redraw
    ;; the window to show the new node.  This will be done lazily
    ;; after all events are handled, to reduce the number of redraws.
    (invalidate window)))

(defmethod mouse-left-down ((window basic-example-window) buttons position)
  (declare (ignore buttons))
  
  ;; Clicking the left mouse button in the interior of the window
  ;; will cause mouse-left-down to be called.  Our method adds
  ;; another node to the window.
  
  (add-node window :x (position-x position) :y (position-y position)))

(defun run-basic-window-example ()
  (let* ((window (make-window :basic-example-window
                   :class 'basic-example-window
                   :title "Click to Add More Nodes"
                   :interior (make-box-relative 100 100 600 500))))
    (dotimes (j 3)(add-node window))
    window))

#+run-example
(run-basic-window-example)

