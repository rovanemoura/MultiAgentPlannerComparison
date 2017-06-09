;/* example-start buttonbox buttonbox.c */
;
;#include <gtk/gtk.h>
;
;/* Create a Button Box with the specified parameters */
;GtkWidget *create_bbox( gint  horizontal,
;			 char *title,
;			 gint  spacing,
;			 gint  child_w,
;			 gint  child_h,
;			 gint  layout )
;{
;  GtkWidget *frame;
;  GtkWidget *bbox;
;  GtkWidget *button;
;
;  frame = gtk_frame_new (title);
;
;  if (horizontal)
;    bbox = gtk_hbutton_box_new ();
;  else
;    bbox = gtk_vbutton_box_new ();
;
;  gtk_container_set_border_width (GTK_CONTAINER (bbox), 5);
;  gtk_container_add (GTK_CONTAINER (frame), bbox);
;
;  /* Set the appearance of the Button Box */
;  gtk_button_box_set_layout (GTK_BUTTON_BOX (bbox), layout);
;  gtk_button_box_set_spacing (GTK_BUTTON_BOX (bbox), spacing);
;  gtk_button_box_set_child_size (GTK_BUTTON_BOX (bbox), child_w, child_h);
;
;  button = gtk_button_new_with_label ("OK");
;  gtk_container_add (GTK_CONTAINER (bbox), button);
;
;  button = gtk_button_new_with_label ("Cancel");
;  gtk_container_add (GTK_CONTAINER (bbox), button);
;
;  button = gtk_button_new_with_label ("Help");
;  gtk_container_add (GTK_CONTAINER (bbox), button);
;
;  return(frame);
;}
;
;int main( int   argc,
;	   char *argv[] )
;{
;  static GtkWidget* window = NULL;
;  GtkWidget *main_vbox;
;  GtkWidget *vbox;
;  GtkWidget *hbox;
;  GtkWidget *frame_horz;
;  GtkWidget *frame_vert;
;
;  /* Initialize GTK */
;  gtk_init( &argc, &argv );
;
;  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;  gtk_window_set_title (GTK_WINDOW (window), "Button Boxes");
;
;  gtk_signal_connect (GTK_OBJECT (window), "destroy",
;		       GTK_SIGNAL_FUNC(gtk_main_quit),
;		       NULL);
;
;  gtk_container_set_border_width (GTK_CONTAINER (window), 10);
;
;  main_vbox = gtk_vbox_new (FALSE, 0);
;  gtk_container_add (GTK_CONTAINER (window), main_vbox);
;
;  frame_horz = gtk_frame_new ("Horizontal Button Boxes");
;  gtk_box_pack_start (GTK_BOX (main_vbox), frame_horz, TRUE, TRUE, 10);
;
;  vbox = gtk_vbox_new (FALSE, 0);
;  gtk_container_set_border_width (GTK_CONTAINER (vbox), 10);
;  gtk_container_add (GTK_CONTAINER (frame_horz), vbox);
;
;  gtk_box_pack_start (GTK_BOX (vbox),
;	    create_bbox (TRUE, "Spread (spacing 40)", 40, 85, 20, GTK_BUTTONBOX_SPREAD),
;		       TRUE, TRUE, 0);
;
;  gtk_box_pack_start (GTK_BOX (vbox),
;	    create_bbox (TRUE, "Edge (spacing 30)", 30, 85, 20, GTK_BUTTONBOX_EDGE),
;		       TRUE, TRUE, 5);
;
;  gtk_box_pack_start (GTK_BOX (vbox),
;	    create_bbox (TRUE, "Start (spacing 20)", 20, 85, 20, GTK_BUTTONBOX_START),
;		       TRUE, TRUE, 5);
;
;  gtk_box_pack_start (GTK_BOX (vbox),
;	    create_bbox (TRUE, "End (spacing 10)", 10, 85, 20, GTK_BUTTONBOX_END),
;		       TRUE, TRUE, 5);
;
;  frame_vert = gtk_frame_new ("Vertical Button Boxes");
;  gtk_box_pack_start (GTK_BOX (main_vbox), frame_vert, TRUE, TRUE, 10);
;
;  hbox = gtk_hbox_new (FALSE, 0);
;  gtk_container_set_border_width (GTK_CONTAINER (hbox), 10);
;  gtk_container_add (GTK_CONTAINER (frame_vert), hbox);
;
;  gtk_box_pack_start (GTK_BOX (hbox),
;	    create_bbox (FALSE, "Spread (spacing 5)", 5, 85, 20, GTK_BUTTONBOX_SPREAD),
;		       TRUE, TRUE, 0);
;
;  gtk_box_pack_start (GTK_BOX (hbox),
;	    create_bbox (FALSE, "Edge (spacing 30)", 30, 85, 20, GTK_BUTTONBOX_EDGE),
;		       TRUE, TRUE, 5);
;
;  gtk_box_pack_start (GTK_BOX (hbox),
;	    create_bbox (FALSE, "Start (spacing 20)", 20, 85, 20, GTK_BUTTONBOX_START),
;		       TRUE, TRUE, 5);
;
;  gtk_box_pack_start (GTK_BOX (hbox),
;	    create_bbox (FALSE, "End (spacing 20)", 20, 85, 20, GTK_BUTTONBOX_END),
;		       TRUE, TRUE, 5);
;
;  gtk_widget_show_all (window);
;
;  /* Enter the event loop */
;  gtk_main ();
;    
;  return(0);
;}
;/* example-end */

(defpackage "10.10-buttonbox" (:use :excl :common-lisp))
(in-package "10.10-buttonbox")

(defun create-bbox (horizontal title spacing child-w child-h layout)
  (let ((frame nil)
	(bbox nil)
	(button nil))

    (setq frame (gtk:gtk_frame_new title))

    (if* (eql horizontal gtk:TRUE)
       then (setq bbox (gtk:gtk_hbutton_box_new))
       else (setq bbox (gtk:gtk_vbutton_box_new)))
    
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER bbox) 5)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) bbox)

    (gtk:gtk_button_box_set_layout (gtk:GTK_BUTTON_BOX bbox) layout)
    (gtk:gtk_button_box_set_spacing (gtk:GTK_BUTTON_BOX bbox) spacing)
    (gtk:gtk_button_box_set_child_size (gtk:GTK_BUTTON_BOX bbox)
				       child-w child-h)

    (setq button (gtk:gtk_button_new_with_label "OK"))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER bbox) button)

    (setq button (gtk:gtk_button_new_with_label "Cancel"))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER bbox) button)

    (setq button (gtk:gtk_button_new_with_label "Help"))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER bbox) button)
    frame))

(ff:defun-foreign-callable cb-gtk-main-quit ()
  (gtk:gtk-main-quit))

(let ((window nil))
  (defun buttonbox ()
    (let ((main-vbox nil)
	  (vbox nil)
	  (hbox nil)
	  (frame-horz nil)
	  (frame-vert nil))

      (gtk:gtk_init 0 0)

      (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
      (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "Button Boxes")

      (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "destroy"
			      (gtk:GTK_SIGNAL_FUNC
			       #+original (ff:get-entry-point "gtk_main_quit")
			       #-original (ff:register-foreign-callable
					   'cb-gtk-main-quit))
			      gtk:NULL)

      (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 10)

      (setq main-vbox (gtk:gtk_vbox_new gtk:FALSE 0))
      (gtk:gtk_container_add (gtk:GTK_CONTAINER window) main-vbox)

      (setq frame-horz (gtk:gtk_frame_new "Horizontal Button Boxes"))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX main-vbox)
			      frame-horz gtk:TRUE gtk:TRUE 10)

      (setq vbox (gtk:gtk_vbox_new gtk:FALSE 0))
      (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER vbox) 10)
      (gtk:gtk_container_add (gtk:GTK_CONTAINER frame-horz) vbox)

      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox)
			      (create-bbox gtk:TRUE "Spread (spacing 40)" 40 85
					   20 gtk:GTK_BUTTONBOX_SPREAD)
			      gtk:TRUE gtk:TRUE 0)

      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox)
			      (create-bbox gtk:TRUE "Edge (spacing 30)" 30 85
					   20 gtk:GTK_BUTTONBOX_EDGE)
			      gtk:TRUE gtk:TRUE 5)

      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox)
			      (create-bbox gtk:TRUE "Start (spacing 20)" 20 85
					   20 gtk:GTK_BUTTONBOX_START)
			      gtk:TRUE gtk:TRUE 5)

      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox)
			      (create-bbox gtk:TRUE "End (spacing 10)" 10 85 20
					   gtk:GTK_BUTTONBOX_END)
			      gtk:TRUE gtk:TRUE 5)

      (setq frame-vert (gtk:gtk_frame_new "Vertical Button Boxes"))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX main-vbox) frame-vert gtk:TRUE
			      gtk:TRUE 10)

      (setq hbox (gtk:gtk_hbox_new gtk:FALSE 0))
      (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER hbox) 10)
      (gtk:gtk_container_add (gtk:GTK_CONTAINER frame-vert) hbox)

      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox)
			      (create-bbox gtk:FALSE "Spread (spacing 5)" 5 85
					   20 gtk:GTK_BUTTONBOX_SPREAD)
			      gtk:TRUE gtk:TRUE 0)

      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox)
			      (create-bbox gtk:FALSE "Edge (spacing 30)" 30 85
					   20 gtk:GTK_BUTTONBOX_EDGE)
			      gtk:TRUE gtk:TRUE 5)

      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox)
			      (create-bbox gtk:FALSE "Start (spacing 20)" 20 85
					   20 gtk:GTK_BUTTONBOX_START)
			      gtk:TRUE gtk:TRUE 5)

      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox)
			      (create-bbox gtk:FALSE "End (spacing 20)" 20 85
					   20 gtk:GTK_BUTTONBOX_END)
			      gtk:TRUE gtk:TRUE 5)

      (gtk:gtk_widget_show_all window)

      #+original (gtk:gtk_main)
      #-original (gtk:gtk-main))))


(flet ((run-example (name function)
	 ;; workaround for bogus (imo) redef. warnings generated by defvar
	 (declare (special gtk::*run-example*))
	 (unless (boundp 'gtk::*run-example*)
	   (setq gtk::*run-example* t))
	 (when gtk::*run-example*
	   (mp:process-run-function
	    (format nil "GTK+ Example: ~a" name)
	    function))))
  (run-example "10.10-buttonbox" #'buttonbox))
