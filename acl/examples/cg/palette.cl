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


#|
A palette holds an arbitrary set of colors for drawing in one
or more windows.  An application needs to use a palette only
if (1) the end user might be running the Windows operating
system in 256-color mode or (perhaps) 65,536-color mode
AND (2) colors are needed other than the 20 colors in the
end user's Control Panel color scheme
(which normally includes the 16 standard VGA colors
black, white, gray, light-gray, red, green, blue, yellow,
cyan, magenta, dark-red, dark-green, dark-blue, dark-yellow, dark-cyan,
and dark-magenta, plus four special colors for button background and
3d edges and so on).

By default, calling the function "palette" on a Common Graphics
window returns the symbol :RGB, which indicates that the window
has not been given a custom palette.  To draw in color on a window that has
no custom palette, you simply set the foreground-color or background-color
to be an RGB color object (as created by make-rgb), and then call
various drawing functions.  But if Windows is not in true color mode
(that is, less than 24-bit color) and the requested RGB color is not one of
the 20 system colors (either a VGA color or one of the other four colors
in the current Control Panel color scheme), then this actual color will
not be used --- for a line-drawing operation, one of the 20 system colors
that is nearest to the requested color will be substituted; and for a
space-filling operation, each pixel that is drawn will be one of two
system colors in such a proportion as to approximate the requested color
(this approximation technique is called dithering, and does not look as
nice as every pixel being the actual requested color).

The alternative to the above is to define and use a custom color palette.
The basic procedure to set up a custom palette is to (1) create a vector
of RGB color objects, (2) create a palette by calling open-palette on
that RGB vector, and then (3) assign the "palette handle" returned by
open-palette to a window by calling (setf palette).  To then draw in
the window, (1) setf the current foreground-color and/or background-color
of the window to be the index of the desired color in the color vector
that you defined earlier, and then (2) call drawing functions on the
window.

A palette should be no larger than the number of colors in which 
the end user is running the Windows operating system.  If Windows
is running in 256-color mode (8-bit color), for example, then
no more than 256 different colors may appear on the screen at any
time, and a palette may contain no more than 256 colors.

If multiple windows (including those in other applications) are
using palettes, and Windows is running in 256-color mode,
then any given window that is using a palette will
typically not display the correct colors except when it is the
most recently selected window that uses a palette, since the
combined palettes will typically contain more colors than the
whole system can display.

The set of colors currently being displayed system-wide is sometimes
referred to as the system palette.  Common Graphics knows to force
a window's full palette into the system palette whenever it is selected.

When Windows is running in 256-color mode, it is recommended that a
palette have somewhat less than 256 colors, so that when the window
is selected, the basic colors used in other windows will still
have room in the system palette and so other windows will not
switch to odd colors borrowed from your custom palette.  A palette
of 236 colors or less would still leave room for the user's 20 colors
from their Control Panel color scheme.

The Windows OS may be run in 1-bit (monochrome) mode, 4-bit
(16-color / VGA) mode, 8-bit (256-color) mode, 16-bit (65,536-color)
mode, or 24- or 32-bit (true color) mode, depending on the end user's
settings in Control Panel.  True color never uses a palette because
the 24 bits or more for each pixel is large enough to contain an
accurate RGB (red-green-blue) specification directly rather than serving
as in index into a smaller table of colors.  Though
most any color computer that is sold nowadays is capable of true color,
the end user may still elect to
run in 256-color mode either to achieve a greater screen resolution
(width by height) or to achieve greater speed of graphical output.
So in general it should not be assumed that an end user will run your
application in true color unless that is an explicit requirement of
the application.

A special case is 16-bit color mode, where the OS appears to use
a default palette with millions of colors that approximate true color.
In this mode, arbitrary RGB colors may be used for foreground and
background colors of Common Graphics windows without using custom palettes,
but the colors actually used will still only be approximate (though a much
better approximation than dithering).  A custom palette
may still be needed even in this mode for better color accuracy.
|#
 
(in-package :cg-user)

(defparameter number-of-colors 100)

(defclass palette-test-window (frame-window) nil)

(defun run-palettes-example (&optional window use-old-palette)
  
  #+gtk (error "Palettes are not yet implemented on GTK.")
  
  (unless window
    (setq window (make-window
                     :palette-test-window
                   :class 'palette-test-window
                   :owner (development-main-window *system*)
                   :title "Palette Test"
                   :child-p t
                   :scrollbars nil
                   :double-buffered t
                   :exterior (make-box 500 240 750 440))))
  (let* ((array (make-array number-of-colors))
         (color-increment (/ 256.0 number-of-colors))
         (red-value (case (random 4)
                      (0 0)
                      (1 255)
                      (2 :up)
                      (3 :down)))
         (blue-value (case (random 4)
                       (0 0)
                       (1 255)
                       (2 :up)
                       (3 :down)))
         (green-value (case (random 4)
                        (0 0)
                        (1 255)
                        (2 :up)
                        (3 :down)))
         ;; (red-value 0)(green-value 0)(blue-value 0)
         palette)
    
    ;; Set random values or movement directions for each color component
    ;; until at least one of them will vary
    (loop (when (or (symbolp red-value)
                    (symbolp green-value)
                    (symbolp blue-value))
            (return))
          (setq red-value (case (random 4)
                            (0 0)
                            (1 255)
                            (2 :up)
                            (3 :down)))
          (setq green-value (case (random 4)
                              (0 0)
                              (1 255)
                              (2 :up)
                              (3 :down)))
          (setq blue-value (case (random 4)
                             (0 0)
                             (1 255)
                             (2 :up)
                             (3 :down))))
    (unless use-old-palette
      (dotimes (j number-of-colors)
        (setf (aref array j)
          (make-rgb
           :red (case red-value
                  (:up (floor (* j color-increment)))
                  (:down (floor (* (i- number-of-colors j 1)
                                   color-increment)))
                  (t red-value))
           :green (case green-value
                    (:up (floor (* j color-increment)))
                    (:down (floor (* (i- number-of-colors j 1)
                                     color-increment)))
                    (t green-value))
           :blue (case blue-value
                   (:up (floor (* j color-increment)))
                   (:down (floor (* (i- number-of-colors j 1)
                                    color-increment)))
                   (t blue-value)))))
      (setq palette (open-palette window array nil))
      (setf (palette window) palette))
    (invalidate window)
    
    ;; This function is the on-initialization function of the project for
    ;; this example.  As such, it returns a window so that when the project
    ;; is made into a generated standalone application, the application
    ;; executable will exit whenever this window is closed by the end user.
    window))

(defmethod redisplay-window ((window palette-test-window)
                             &optional box)
  (declare (ignore box))
  (call-next-method)
  (when (eq (palette window) :rgb) ;; while creating the window
    (return-from redisplay-window))
  (let* ((bar-width (/ (interior-width window)
                       number-of-colors))
         (bar-height (interior-height window)))
    (dotimes (j number-of-colors)
      
      ;; When setting the foreground (or background) color of a window
      ;; that uses a palette, always specify the color by its index
      ;; in the color vector from which the palette was made.  If you
      ;; instead specify the color as an RGB object, and the Windows
      ;; operating system is not running in true color mode, then
      ;; the requested RGB will be converted by the OS to the nearest
      ;; of the 20 system colors defined in the current Control Panel
      ;; color scheme (though the color might be approximated for fills
      ;; by dithering two system colors in some proportion).
      (setf (foreground-color window) j)
      
      (fill-box window (make-box (floor (* j bar-width))
                                 0 (floor (* (1+ j) bar-width)) bar-height)))))

;; Force a full redisplay of our window after resizing, since the
;; contents stretch to fit the new interior.
(defmethod resize-window ((window palette-test-window) position)
  (declare (ignore position))
  (invalidate window))

#+run-example (run-palettes-example)
