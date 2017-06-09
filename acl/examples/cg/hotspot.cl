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

;; Using hotspots

(in-package :cg-user)

;; Make a custom frame class.
(defclass my-hotspot-frame (non-refreshing-window)
  ())

;; Make a custom pane class that will contain the hotspots.
;; Hotspot-mixin should come BEFORE the window class in the parent list.
(defclass my-hotspot-pane (hotspot-mixin non-refreshing-pane)
  ((use-custom-opaque-highlighting
    :accessor use-custom-opaque-highlighting
    :initform nil)
   
   ;; For this example, hard-wire a box hotspot.
   (text-hotspot-box
    :accessor text-hotspot-box
    :initform (make-box 5 5 110 65))
   
   ;; And a line hotspot.
   (line-hotspot-endpoints
    :accessor line-hotspot-endpoints
    :initform (list (make-position 110 90)
                    (make-position 138 47)))
   
   ;; Hard-wire a polygon hotspot.
   (polygon-hotspot-vertices
    :accessor polygon-hotspot-vertices
    :initform
    (vector (make-position 90 70)
            (make-position 110 90)
            (make-position 160 110)
            (make-position 90 140)
            (make-position 90 100)
            (make-position 140 180)
            (make-position 150 150)
            (make-position 70 180)
            (make-position 20 180)
            (make-position 50 140)
            (make-position 40 120)
            (make-position 40 90)
            (make-position 60 70)))))

;; Tell the frame class which pane class to auto-instantiate
;; (since non-refreshing-window is a frame-with-single-child class).
(defmethod default-pane-class ((window my-hotspot-frame))
  'my-hotspot-pane)

;; Define a custom hotspot subclass so that we can define methods on it.
(defclass my-hotspot (hotspot)
  ())

;; Give our window class a redisplay-window method that draws some
;; "real" content where some of the hotspots are defined.
(defmethod redisplay-window ((window my-hotspot-pane) &optional box)
  (declare (ignore box))
  
  ;; Call the default method that clears the window.
  (call-next-method)
  
  ;; Draw some text in the rectangular hotspot.
  (draw-string-in-box
   window "There is a hotspot here."
   nil nil (text-hotspot-box window) :center :top nil t)
  
  ;; Draw the fancy polygon, which we will also make into a hotspot.
  (with-foreground-color (window dark-green)
    (draw-polygon window (polygon-hotspot-vertices window)))
  
  ;; Draw an arrow, which we will also make into a hotspot.
  (with-foreground-color (window dark-red)
    (let* ((endpoints (line-hotspot-endpoints window)))
      (apply 'draw-line window endpoints)
      (apply 'draw-arrowhead window endpoints))))

;; No mouse-click event handlers are defined for hotspots by default,
;; so let's make a mouse-click method for our own hotspot class.
(defmethod mouse-left-down ((hotspot my-hotspot) buttons data)
  (declare (ignore buttons data))
  (window-message (parent-or-owner ;; pane to frame
                   (parent hotspot))
      "You clicked ~s." (name hotspot))
  
  ;; Return non-NIL to prevent the click from being further handled
  ;; by the parent window of the hotspot.
  t)

;; This method for the pane itself will be called when it is
;; clicked not over any hotspot.
(defmethod mouse-left-down ((pane my-hotspot-pane) buttons data)
  (declare (ignore buttons data))
  (window-message (parent-or-owner pane) ;; pane to frame
      "You clicked NO hotspot.")
  t)

;;; Make a window instance that has hotspots.
;;; This is the on-initialization function of this example's project.
;;; As such, it returns a window such that when the window is closed,
;;; the example terminated.
(defun run-hotspots-example ()
  (let* ((width 215)
         (height 290)
         (frame (make-window :hotspot-test-frame
                  :class 'my-hotspot-frame 
                  :owner (development-main-window *system*)
                  :title "Hotspots"
                  :exterior (make-box-relative 500 180 width height)
                  :resizable nil
                  :scrollbars nil
                  :state :shrunk))
         (pane (frame-child frame))
         
         ;; A standard box hotspot.
         (text-box (make-instance 'my-hotspot
                     :name :text-box
                     :highlight-style :invert
                     :color (make-rgb :red 200 :green 200 :blue 255)
                     :region (text-hotspot-box pane)))
         
         ;; A fancy polygon hotspot.
         (polygon (make-instance 'my-hotspot
                    :name :polygon
                    
                    ;; We don't need to highlight the polygon hotspot
                    ;; when the mouse is over it, because the window
                    ;; draws that polygon all the time.
                    :highlight-style nil
                    
                    :cursor (find-cursor :hand-cursor)
                    :region (polygon-hotspot-vertices pane)))
         
         ;; A circle hotspot.
         (circle (make-instance 'my-hotspot
                   :name :circle
                   :highlight-style :outline
                   :color red
                   :cursor cross-cursor
                   :region (list (make-position 150 30) 20)))
         
         ;; A single-line hotspot.
         (arrow (make-instance 'my-hotspot
                  :name :arrow
                  :highlight-style nil
                  :cursor cross-cursor
                  :region (line-hotspot-endpoints pane)))
         (underlapper (make-instance 'my-hotspot
                        :name :underlapper
                        :highlight-style :outline
                        :cursor cross-cursor
                        :color blue
                        :region (make-box 140 90 185 140))))
    (add-common-status-bar frame)
    (add-component (make-instance 'check-box
                     :name :custom
                     :title "Custom Highlighting"
                     :wrapping t
                     :left 10 :top 200 :width (- width 20) :height 34
                     :on-change (lambda (wij new old)
                                  (declare (ignore old))
                                  (setf (use-custom-opaque-highlighting
                                         (parent wij))
                                    new)
                                  t))
                   pane)
    
    ;; Add the hotspots to the window.  Hotspots added earlier
    ;; will be under any overlapping hotspots that are added later.
    (dolist (hotspot (list underlapper arrow text-box polygon circle))
      (add-hotspot pane hotspot))
    
    ;; Everything is now all ready, so show the window.
    (select-window frame)
    (window-message frame "Click on hotspots!")
    frame))

;;; An OPTIONAL method for custom highlighting.
(defmethod highlight-hotspot ((hotspot my-hotspot) &key off)
  (unless (use-custom-opaque-highlighting (parent hotspot))
    (return-from highlight-hotspot
      (call-next-method)))
  
  ;; This is an example of a custom highlight-hotspot method
  ;; that an application might define instead of using one of
  ;; the two built-in highlighting styles.
  ;; This draws opaque highlighting rather than xoring
  ;; screen colors as the built-in styles do.
  
  ;; If we are turning the highlight off, then just invalidate
  ;; the hotspot so that the window's redisplay-window method
  ;; will be called to redraw the hotspot's region without the
  ;; hotspot highlighting.
  (if* off
     then (invalidate hotspot)
          
          ;; Otherwise draw highlighting over the top of whatever
          ;; is usually drawn in the window.
     else (let* ((window (parent hotspot))
                 (region (hotspot-region hotspot)))
            (with-background-color (window yellow)
              (with-foreground-color (window black)
                
                ;; This interprets all four kinds of hotspot-region.
                
                ;; A simple orthogonal rectangle hotspot.
                (cond ((boxp region)
                       
                       ;; Defer to the built-in highlighting for
                       ;; box hotspots.
                       (call-next-method))
                      
                      ;; A polygon hotspot.
                      ((or (vectorp region)
                           (cddr region))
                       
                       ;; These "erase" functions really draw,
                       ;; but in the current background color.
                       (erase-contents-polygon window region)
                       (draw-polygon window region))
                      
                      ;; A circle hotspot.
                      ((integerp (second region))
                       (erase-contents-circle window (first region)
                                              (second region))
                       (draw-circle window (first region)
                                    (second region)))
                      
                      ;; A line hotspot.
                      ;; Use with-clipping-box here to prevent this
                      ;; thick line from extending outside the
                      ;; region-box that will be erased by a simple
                      ;; call to invalidate on the hotspot.
                      (t (with-clipping-box (window (region-box region))
                           (with-line-width (window 10)
                             (with-foreground-color (window blue)
                               (draw-line window (first region)
                                          (second region))))))))))))

#+run-example
(run-hotspots-example)

