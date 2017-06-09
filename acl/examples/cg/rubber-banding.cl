;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2013 Franz Inc, Oakland, CA  All rights reserved.
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

;;; Rubber-Banding Example

;;; This example displays a pentagon in a window.
;;; The user can click down on a vertex of the
;;; polygon and drag it to a new location.
;;; get-shape-line is called to "rubber band" the
;;; two lines that connect to the vertex while it
;;; is being dragged.

(in-package :cg-user)

(defclass polygon-window (frame-window)
  ((vertices :accessor vertices
             :initarg :vertices
             :initform nil)
   (mousing-margin :accessor mousing-margin
                   :initarg :mousing-margin
                   :initform nil))
  (:default-initargs
      :vertices (list (make-position 20 280)
                      (make-position 20 100)
                      (make-position 150 20)
                      (make-position 280 100)
                      (make-position 280 280))
    :mousing-margin 6))

(defmethod redisplay-window ((window polygon-window) &optional box)
  (declare (ignore box))
  (call-next-method) ;; Clear the window background
  (draw-polygon window (vertices window)))

(defmethod mouse-left-down ((window polygon-window)
                            mouse-buttons cursor-position)
  (declare (ignore mouse-buttons))
  
  ;; When the user clicks in the window, find the vertex
  ;; that is within mousing-margin pixels of the click
  ;; position, if any.
  (do* ((mousing-margin (mousing-margin window))
        (vertices (vertices window))
        (previous-position nil this-position)
        (positions vertices (rest positions))
        this-position next-position)
       ((null positions))
    (setq this-position (first positions))
    
    ;; If a moused vertex is found ...
    (when (and (<= (abs (- (position-x this-position)
                           (position-x cursor-position)))
                   mousing-margin)
               (<= (abs (- (position-y this-position)
                           (position-y cursor-position)))
                   mousing-margin))
      
      ;; ... then find the previous and next vertices in order
      ;; to rubber-band lines from them to the moving vertex.
      
      ;; If we are moving the first vertex in the list,
      ;; then the previous vertex will be the last one
      ;; in the list.
      (unless previous-position
        (setq previous-position (first (last vertices))))
      
      ;; If we are moving the last vertex in the list, then
      ;; the next vertex will be the first one in the list.
      (setq next-position (or (second positions)
                              (first vertices)))
      
      ;; The rubber-banding functions draw in "xor" mode, and
      ;; so we can use the same function to erase the lines
      ;; as we use to draw them.  To pass in a single function
      ;; as both the draw-fn and erase-fn arguments to get-shape-line,
      ;; we use an flet here to specify it a single time only.
      (flet ((draw-it (stream start-pos drag-pos)
              (declare (ignore start-pos))
              (draw-line stream previous-position drag-pos)
              (draw-line stream drag-pos next-position)))
        
        ;; This call to get-shape-line will do the custom
        ;; rubber-banding, using our draw-it function to draw
        ;; lines from the surrounding vertices to the moving one.
        ;; Get-shape-line will return when the user releases the
        ;; mouse button.
        (let* ((new-position (get-shape-line window this-position
                                             #'draw-it #'draw-it)))
          
          ;; If the user did not cancel from the rubber-banding
          ;; (by pressing the ESCAPE key),
          ;; then write the final position into the vertex list
          ;; for this window, and invalidate the window so that
          ;; the polygon will be redrawn.
          (when new-position
            (setf (first positions) new-position)
            (invalidate window))))
      (return))))
                      
(defmethod mouse-moved ((window polygon-window)
                        mouse-buttons cursor-position)
  (declare (ignore mouse-buttons))
  
  ;; This mouse-moved method changes the mouse cursor to
  ;; show when it is near enough to a vertex to drag it.
  (do* ((mousing-margin (mousing-margin window))
        (vertices (vertices window)(rest vertices))
        vertex)
       ((null vertices))
    (setq vertex (first vertices))
    
    ;; If a mousable vertex is under the mouse ...
    (when (and (<= (abs (- (position-x vertex)
                           (position-x cursor-position)))
                   mousing-margin)
               (<= (abs (- (position-y vertex)
                           (position-y cursor-position)))
                   mousing-margin))
      
      ;; then set the window's mouse cursor to indicate this.
      (setf (cursor window) sizing-cursor)
      (return-from mouse-moved)))
  
  ;; If no vertex is under the mouse, use the regular mouse cursor.
  (setf (cursor window) :default))
  
(defun run-rubber-banding-example ()
  (make-window :rubber-banding-example
    :class 'polygon-window
    :title "Drag Vertices"
    :scrollbars nil
    :interior (make-box-relative 400 200 300 300)))

#+run-example
(run-rubber-banding-example)

