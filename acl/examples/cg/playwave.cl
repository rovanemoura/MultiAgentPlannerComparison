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

;; Play back a .wav digital sound file using the Windows
;; Multimedia Control Interface (MCI) facility

(in-package :cg-user)

;; This is the on-initialization function.  As such, it returns a window
;; such that when the window is closed, the example is considered to
;; be terminated.
;;
(defun run-play-wave-example ()
  
  #+gtk (error "There is no support on GTK for playing sound files.")
  
  ;; Call this to nil out the pressed-since-previous-call state of the
  ;; escape key.  (See the exit test below.)
  (key-is-down-p vk-escape)
  
  ;; Make a CLOS object for playing wave-audio files.
  (let* ((wave (make-instance 'mci-wave-audio))
         
         ;; Find all of the wave-audio files in the the Media
         ;; subdirectory where the Windows OS is installed
         (files (sort (copy-list
                       (directory (merge-pathnames
                                   "media\\*.wav"
                                   (sys::windows-directory))))
                      #'(lambda (path1 path2)
                          (string-lessp (namestring path1)
                                        (namestring path2)))))
         (number (length files))
         (count 0)
         (aborted nil))
    
    ;; Play each file in the Windows "Media" subdirectory
    (dolist (path files)
      (format t "~&Now Playing --- ~a     (~a of ~a)~%"
        path (incf count) number)
      
      ;; Open the wave-audio device on some .wav file, and play it.
      (mci-open wave :file (namestring path) :wait-p t)
      (mci-play wave :wait-p t)
      
      ;; Back up and play each sound a second time.
      #+maybe (mci-seek wave :seek-to-start-p t :wait-p t)
      #+maybe (mci-play wave :wait-p t)
      
      ;; Close the wave-audio device so that it can be re-opened
      ;; to play a different sound file.
      (mci-close wave :wait-p t)
      
      ;; Exit if the user has pressed the ESCAPE key
      (multiple-value-bind (is-down pressed-since-previous-call)
          (key-is-down-p vk-escape)
        (when (or is-down pressed-since-previous-call)
          (setq aborted t)
          (return))))
    (format t (if aborted
                  "~&Oh, you're no fun anymore.~%"
                "~&That's All Folks!~%"))))

#+run-example (run-play-wave-example)
