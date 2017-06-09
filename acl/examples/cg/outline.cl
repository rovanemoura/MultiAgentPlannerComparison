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

;; Basic Outline Control Example
;; Call (run-outline-example) to run

;; This example makes two outline controls, each of which
;; displays the subclass hierarchy of a class.  The lefthand
;; outline demonstrates the basic approach of creating all of
;; the outline-items when the outline is created, while the
;; righthand outline demonstrates the alternate approach of
;; creating outline-items only as they are needed ("lazily")
;; when the user opens their parent items.

(in-package :cg-user)

;; This function is used by the outline in the left half of the dialog
;; to recursively create all of the outline-items for a whole class
;; hierarchy when the outline control is first created.
(defun make-subclass-item-recursively (class)
  (make-instance 'outline-item
    :value class
    :state :open
    :range (mapcar #'make-subclass-item-recursively
             (mop:class-direct-subclasses class))))

;; This function is used by the outline in the right half of the dialog
;; to create outline-items "lazily" for the direct subclasses of a class
;; when that class' outline-item is opened by the user.
(defun make-subclass-item-lazily (class)
  (make-instance 'outline-item
    :value class
    :state :closed
    
    ;; Since this outline is opening items lazily, CG doesn't yet
    ;; know if the outline-item is REALLY a "leaf" with no sub-items.
    ;; So check ahead for subclasses of this subclass, and if there
    ;; are any then tell the outline to draw this item as an
    ;; openable item rather than as a leaf.
    :has-range-on-open (and (mop:class-direct-subclasses class) t)))

;; Define an outline subclass in order to add the custom
;; range-on-open method below.
(defclass my-class-outline (outline)())

;; The generic function range-on-open is called whenever an
;; outline-item is opened.  We add this method to lazily open the
;; outline-items in the outline in the right half of the dialog.
(defmethod range-on-open ((outline my-class-outline) item-value range)
  
  ;; If this item has already been opened, then the range we gave
  ;; it last time is passed back in on this call.  Assume that the
  ;; set of subclasses we found previously is still current, and
  ;; just return them again to show the same subclasses this time.
  (or range
      
      ;; If no child items were passed in, then either this
      ;; class has no subclasses or this outline-item has not
      ;; yet been opened.  So find the subclasses for the latter
      ;; case and create new outline-items for them the first
      ;; time the user opens this item.
      (mapcar #'make-subclass-item-lazily
        (mop:class-direct-subclasses item-value))))

;; Pass a class or class name to this function in order to
;; display its subclass hierarchy in an outline control.
(defun show-subclasses-in-outline (class)
  (when (symbolp class)
    (setq class (find-class class)))
  
  ;; The lefthand outline creates its whole hierarchy initially.
  (let* ((outline1 (make-instance 'outline
                     :value nil
                     :range (list (make-subclass-item-recursively class))
                     :on-print #'(lambda (class)
                                   (princ-to-string
                                    (class-name class)))
                     :close-subtrees-on-close nil
                     :left 20 :top 20 :width 280 :height 280
                     :right-attachment :scale
                     :bottom-attachment :bottom))
         
         ;; The righthand outline creates its items lazily as
         ;; the user opens each item.
         (outline2 (make-instance 'my-class-outline
                     :value nil
                     :range (list (make-subclass-item-lazily class))
                     :on-print #'(lambda (class)
                                   (princ-to-string
                                    (class-name class)))
                     :opened-pixmap-name :opened
                     :closed-pixmap-name :closed
                     :leaf-pixmap-name :leaf
                     :left 320 :top 20 :width 280 :height 280
                     :left-attachment :scale
                     :right-attachment :right
                     :bottom-attachment :bottom))
         
         ;; Make a dialog and place the two outlines in it.
         (dialog (make-window 'my-dialog
                   :class 'dialog
                   :title (format nil "Subclasses of ~s"
                            (class-name class))
                   :resizable t
                   :interior (make-box-relative 200 200 620 320)
                   :dialog-items (list outline1 outline2))))
    dialog))

(defun run-outline-example ()
  (show-subclasses-in-outline 'dialog-item))




;;; The rest of this file defines the pixmaps for the blue
;;; arrows used in the righthand outline.  If these outlines
;;; had been layed out interactively on a form, then these
;;; pixmaps would be added to the project automatically in
;;; the .bml file for the form.  But since this example
;;; was created programmatically, and these pixmaps do not
;;; exist outside the IDE, we have to create them explicitly.
;;; One way to generate this code from an existing pixmap is
;;; to call recreation-code on the pixmap object, as in:
;;; (recreation-code (find-pixmap :opened))

(cache-pixmap
 (make-instance 'pixmap
   :name :opened
   :contents
   '((15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 04 04 04 04 04 04 04 04 15 15 15 15 15)
     (15 15 15 04 04 04 04 04 04 04 04 15 15 15 15 15)
     (15 15 15 15 04 04 04 04 04 04 15 15 15 15 15 15)
     (15 15 15 15 04 04 04 04 04 04 15 15 15 15 15 15)
     (15 15 15 15 15 04 04 04 04 15 15 15 15 15 15 15)
     (15 15 15 15 15 04 04 04 04 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 04 04 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)))
 )
(cache-pixmap
 (make-instance 'pixmap
   :name :closed
   :contents
   '((15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 04 04 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 04 04 04 04 15 15 15 15 15 15 15 15 15)
     (15 15 15 04 04 04 04 04 04 15 15 15 15 15 15 15)
     (15 15 15 04 04 04 04 04 04 04 15 15 15 15 15 15)
     (15 15 15 04 04 04 04 04 04 04 15 15 15 15 15 15)
     (15 15 15 04 04 04 04 04 04 15 15 15 15 15 15 15)
     (15 15 15 04 04 04 04 15 15 15 15 15 15 15 15 15)
     (15 15 15 04 04 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)))
 )
(cache-pixmap
 (make-instance 'pixmap
   :name :leaf
   :contents
   '((15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 08 08 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 08 08 08 08 15 15 15 15 15 15 15 15 15)
     (15 15 15 08 15 15 15 08 08 15 15 15 15 15 15 15)
     (15 15 15 08 15 15 15 15 15 08 15 15 15 15 15 15)
     (15 15 15 08 15 15 15 15 15 08 15 15 15 15 15 15)
     (15 15 15 08 15 15 15 08 08 15 15 15 15 15 15 15)
     (15 15 15 08 15 08 08 15 15 15 15 15 15 15 15 15)
     (15 15 15 08 08 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)
     (15 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15)))
 )
