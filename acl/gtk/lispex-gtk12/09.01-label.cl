;/* example-start label label.c */
;
;#include <gtk/gtk.h>
;
;int main( int   argc,
;          char *argv[] )
;{
;  static GtkWidget *window = NULL;
;  GtkWidget *hbox;
;  GtkWidget *vbox;
;  GtkWidget *frame;
;  GtkWidget *label;
;
;  /* Initialise GTK */
;  gtk_init(&argc, &argv);
;
;  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;  gtk_signal_connect (GTK_OBJECT (window), "destroy",
;		      GTK_SIGNAL_FUNC(gtk_main_quit),
;		      NULL);
;
;  gtk_window_set_title (GTK_WINDOW (window), "Label");
;  vbox = gtk_vbox_new (FALSE, 5);
;  hbox = gtk_hbox_new (FALSE, 5);
;  gtk_container_add (GTK_CONTAINER (window), hbox);
;  gtk_box_pack_start (GTK_BOX (hbox), vbox, FALSE, FALSE, 0);
;  gtk_container_set_border_width (GTK_CONTAINER (window), 5);
;  
;  frame = gtk_frame_new ("Normal Label");
;  label = gtk_label_new ("This is a Normal label");
;  gtk_container_add (GTK_CONTAINER (frame), label);
;  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 0);
;  
;  frame = gtk_frame_new ("Multi-line Label");
;  label = gtk_label_new ("This is a Multi-line label.\nSecond line\n" \
;			 "Third line");
;  gtk_container_add (GTK_CONTAINER (frame), label);
;  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 0);
;  
;  frame = gtk_frame_new ("Left Justified Label");
;  label = gtk_label_new ("This is a Left-Justified\n" \
;			 "Multi-line label.\nThird      line");
;  gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_LEFT);
;  gtk_container_add (GTK_CONTAINER (frame), label);
;  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 0);
;  
;  frame = gtk_frame_new ("Right Justified Label");
;  label = gtk_label_new ("This is a Right-Justified\nMulti-line label.\n" \
;			 "Fourth line, (j/k)");
;  gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_RIGHT);
;  gtk_container_add (GTK_CONTAINER (frame), label);
;  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 0);
;
;  vbox = gtk_vbox_new (FALSE, 5);
;  gtk_box_pack_start (GTK_BOX (hbox), vbox, FALSE, FALSE, 0);
;  frame = gtk_frame_new ("Line wrapped label");
;  label = gtk_label_new ("This is an example of a line-wrapped label.  It " \
;			 "should not be taking up the entire             " /* big space to test spacing */\
;			 "width allocated to it, but automatically " \
;			 "wraps the words to fit.  " \
;			 "The time has come, for all good men, to come to " \
;			 "the aid of their party.  " \
;			 "The sixth sheik's six sheep's sick.\n" \
;			 "     It supports multiple paragraphs correctly, " \
;			 "and  correctly   adds "\
;			 "many          extra  spaces. ");
;  gtk_label_set_line_wrap (GTK_LABEL (label), TRUE);
;  gtk_container_add (GTK_CONTAINER (frame), label);
;  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 0);
;  
;  frame = gtk_frame_new ("Filled, wrapped label");
;  label = gtk_label_new ("This is an example of a line-wrapped, filled label.  " \
;			 "It should be taking "\
;			 "up the entire              width allocated to it.  " \
;			 "Here is a sentence to prove "\
;			 "my point.  Here is another sentence. "\
;			 "Here comes the sun, do de do de do.\n"\
;			 "    This is a new paragraph.\n"\
;			 "    This is another newer, longer, better " \
;			 "paragraph.  It is coming to an end, "\
;			 "unfortunately.");
;  gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_FILL);
;  gtk_label_set_line_wrap (GTK_LABEL (label), TRUE);
;  gtk_container_add (GTK_CONTAINER (frame), label);
;  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 0);
;  
;  frame = gtk_frame_new ("Underlined label");
;  label = gtk_label_new ("This label is underlined!\n"
;			 "This one is underlined in quite a funky fashion");
;  gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_LEFT);
;  gtk_label_set_pattern (GTK_LABEL (label),
;			 "_________________________ _ _________ _ ______     __ _______ ___");
;  gtk_container_add (GTK_CONTAINER (frame), label);
;  gtk_box_pack_start (GTK_BOX (vbox), frame, FALSE, FALSE, 0);
;  
;  gtk_widget_show_all (window);
;
;  gtk_main ();
;  
;  return(0);
;}
;/* example-end */

(defpackage "09.01-label" (:use :excl :common-lisp))
(in-package "09.01-label")

(ff:defun-foreign-callable cb-gtk-main-quit ()
  (gtk:gtk-main-quit))

(defun label ()
  (let ((window nil)
	(hbox nil)
	(vbox nil)
	(frame nil)
	(label nil))
    (gtk:gtk_init 0 0)
    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "destroy"
			    (gtk:GTK_SIGNAL_FUNC
			     #+original (ff:get-entry-point "gtk_main_quit")
			     #-original (ff:register-foreign-callable
					 'cb-gtk-main-quit))
			    gtk:NULL)
    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "Label")
    (setq vbox (gtk:gtk_vbox_new gtk:FALSE 5))
    (setq hbox (gtk:gtk_hbox_new gtk:FALSE 5))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) hbox)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) vbox gtk:FALSE gtk:FALSE 0)
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 5)
    
    (setq frame (gtk:gtk_frame_new "Normal Label"))
    (setq label (gtk:gtk_label_new "This is a Normal label"))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) label)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) frame gtk:FALSE gtk:FALSE 0)

    (setq frame (gtk:gtk_frame_new "Multi-line Label"))
    (setq label (gtk:gtk_label_new #.(format nil "~
This is a Multi-line label.~%Second line~%~
Third line")))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) label)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) frame gtk:FALSE gtk:FALSE 0)

    (setq frame (gtk:gtk_frame_new "Left Justified Label"))
    (setq label (gtk:gtk_label_new #.(format nil "~
This is a Left-Justified~%~
Multi-line label.~%Third      line")))
    (gtk:gtk_label_set_justify (gtk:GTK_LABEL label) gtk:GTK_JUSTIFY_LEFT)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) label)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) frame gtk:FALSE gtk:FALSE 0)

    (setq frame (gtk:gtk_frame_new "Right Justified Label"))
    (setq label (gtk:gtk_label_new #.(format nil "~
This is a Right-Justified~%Multi-line label.~%~
Fourth line, (j/k)")))
    (gtk:gtk_label_set_justify (gtk:GTK_LABEL label) gtk:GTK_JUSTIFY_RIGHT)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) label)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) frame gtk:FALSE gtk:FALSE 0)

    (setq vbox (gtk:gtk_vbox_new gtk:FALSE 5))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) vbox gtk:FALSE gtk:FALSE 0)
    (setq frame (gtk:gtk_frame_new "Line wrapped label"))
    (setq label (gtk:gtk_label_new #.(format nil "~
This is an example of a line-wrapped label.  It ~
should not be taking up the entire             ~
width allocated to it, but automatically ~
wraps the words to fit.  ~
The time has come, for all good men, to come to ~
the aid of their party.  ~
The sixth sheik's six sheep's sick.
     It supports multiple paragraphs correctly, ~
and  correctly   adds ~
many          extra  spaces. ")))
    (gtk:gtk_label_set_line_wrap (gtk:GTK_LABEL label) gtk:TRUE)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) label)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) frame gtk:FALSE gtk:FALSE 0)

    (setq frame (gtk:gtk_frame_new "Filled, wrapped label"))
    (setq label (gtk:gtk_label_new #.(format nil "~
This is an example of a line-wrapped, filled label.  ~
It should be taking ~
up the entire              width allocated to it.  ~
Here is a sentence to prove ~
my point.  Here is another sentence. ~
Here comes the sun, do de do de do.~%~
    This is a new paragraph.~%~
    This is another newer, longer, better ~
paragraph.  It is coming to an end, ~
unfortunately.")))
    (gtk:gtk_label_set_justify (gtk:GTK_LABEL label) gtk:GTK_JUSTIFY_FILL)
    (gtk:gtk_label_set_line_wrap (gtk:GTK_LABEL label) gtk:TRUE)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) label)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) frame gtk:FALSE gtk:FALSE 0)

    (setq frame (gtk:gtk_frame_new "Underlined label"))
    (setq label (gtk:gtk_label_new #.(format nil "~
This label is underlined!~%~
This one is underlined in quite a funky fashion")))
    (gtk:gtk_label_set_justify (gtk:GTK_LABEL label) gtk:GTK_JUSTIFY_LEFT)
    (gtk:gtk_label_set_pattern (gtk:GTK_LABEL label) #.(format nil "~
_________________________ _ _________ _ ______     __ _______ ___"))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) label)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) frame gtk:FALSE gtk:FALSE 0)

    (gtk:gtk_widget_show_all window)

    #+original (gtk:gtk_main)
    #-original (gtk:gtk-main)))

(flet ((run-example (name function)
	 ;; workaround for bogus (imo) redef. warnings generated by defvar
	 (declare (special gtk::*run-example*))
	 (unless (boundp 'gtk::*run-example*)
	   (setq gtk::*run-example* t))
	 (when gtk::*run-example*
	   (mp:process-run-function
	    (format nil "GTK+ Example: ~a" name)
	    function))))
  (run-example "09.01-label" #'label))
