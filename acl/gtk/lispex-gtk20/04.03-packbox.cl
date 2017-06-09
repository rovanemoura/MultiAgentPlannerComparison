;
;#include <stdio.h>
;#include <stdlib.h>
;#include "gtk/gtk.h"
;
;gint delete_event( GtkWidget *widget,
;		    GdkEvent  *event,
;		    gpointer   data )
;{
;    gtk_main_quit ();
;    return FALSE;
;}
;
;/* Make a new hbox filled with button-labels. Arguments for the 
; * variables we're interested are passed in to this function. 
; * We do not show the box, but do show everything inside. */
;GtkWidget *make_box( gboolean homogeneous,
;		      gint     spacing,
;		      gboolean expand,
;		      gboolean fill,
;		      guint    padding ) 
;{
;    GtkWidget *box;
;    GtkWidget *button;
;    char padstr[80];
;    
;    /* Create a new hbox with the appropriate homogeneous
;     * and spacing settings */
;    box = gtk_hbox_new (homogeneous, spacing);
;    
;    /* Create a series of buttons with the appropriate settings */
;    button = gtk_button_new_with_label ("gtk_box_pack");
;    gtk_box_pack_start (GTK_BOX (box), button, expand, fill, padding);
;    gtk_widget_show (button);
;    
;    button = gtk_button_new_with_label ("(box,");
;    gtk_box_pack_start (GTK_BOX (box), button, expand, fill, padding);
;    gtk_widget_show (button);
;    
;    button = gtk_button_new_with_label ("button,");
;    gtk_box_pack_start (GTK_BOX (box), button, expand, fill, padding);
;    gtk_widget_show (button);
;    
;    /* Create a button with the label depending on the value of
;     * expand. */
;    if (expand == TRUE)
;	     button = gtk_button_new_with_label ("TRUE,");
;    else
;	     button = gtk_button_new_with_label ("FALSE,");
;    
;    gtk_box_pack_start (GTK_BOX (box), button, expand, fill, padding);
;    gtk_widget_show (button);
;    
;    /* This is the same as the button creation for "expand"
;     * above, but uses the shorthand form. */
;    button = gtk_button_new_with_label (fill ? "TRUE," : "FALSE,");
;    gtk_box_pack_start (GTK_BOX (box), button, expand, fill, padding);
;    gtk_widget_show (button);
;    
;    sprintf (padstr, "%d);", padding);
;    
;    button = gtk_button_new_with_label (padstr);
;    gtk_box_pack_start (GTK_BOX (box), button, expand, fill, padding);
;    gtk_widget_show (button);
;    
;    return box;
;}
;
;int main( int   argc,
;	   char *argv[]) 
;{
;    GtkWidget *window;
;    GtkWidget *button;
;    GtkWidget *box1;
;    GtkWidget *box2;
;    GtkWidget *separator;
;    GtkWidget *label;
;    GtkWidget *quitbox;
;    int which;
;    
;    /* Our init, don't forget this! :) */
;    gtk_init (&argc, &argv);
;    
;    if (argc != 2) {
;	 fprintf (stderr, "usage: packbox num, where num is 1, 2, or 3.\n");
;	 /* This just does cleanup in GTK and exits with an exit status of 1. */
;	 exit (1);
;    }
;    
;    which = atoi (argv[1]);
;
;    /* Create our window */
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;
;    /* You should always remember to connect the delete_event signal
;     * to the main window. This is very important for proper intuitive
;     * behavior */
;    g_signal_connect (G_OBJECT (window), "delete_event",
;		       G_CALLBACK (delete_event), NULL);
;    gtk_container_set_border_width (GTK_CONTAINER (window), 10);
;    
;    /* We create a vertical box (vbox) to pack the horizontal boxes into.
;     * This allows us to stack the horizontal boxes filled with buttons one
;     * on top of the other in this vbox. */
;    box1 = gtk_vbox_new (FALSE, 0);
;    
;    /* which example to show. These correspond to the pictures above. */
;    switch (which) {
;    case 1:
;	 /* create a new label. */
;	 label = gtk_label_new ("gtk_hbox_new (FALSE, 0);");
;	 
;	 /* Align the label to the left side.  We'll discuss this function and 
;	  * others in the section on Widget Attributes. */
;	 gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
;
;	 /* Pack the label into the vertical box (vbox box1).  Remember that 
;	  * widgets added to a vbox will be packed one on top of the other in
;	  * order. */
;	 gtk_box_pack_start (GTK_BOX (box1), label, FALSE, FALSE, 0);
;	 
;	 /* Show the label */
;	 gtk_widget_show (label);
;	 
;	 /* Call our make box function - homogeneous = FALSE, spacing = 0,
;	  * expand = FALSE, fill = FALSE, padding = 0 */
;	 box2 = make_box (FALSE, 0, FALSE, FALSE, 0);
;	 gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
;	 gtk_widget_show (box2);
;
;	 /* Call our make box function - homogeneous = FALSE, spacing = 0,
;	  * expand = TRUE, fill = FALSE, padding = 0 */
;	 box2 = make_box (FALSE, 0, TRUE, FALSE, 0);
;	 gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
;	 gtk_widget_show (box2);
;	 
;	 /* Args are: homogeneous, spacing, expand, fill, padding */
;	 box2 = make_box (FALSE, 0, TRUE, TRUE, 0);
;	 gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
;	 gtk_widget_show (box2);
;	 
;	 /* Creates a separator, we'll learn more about these later, 
;	  * but they are quite simple. */
;	 separator = gtk_hseparator_new ();
;	 
;	 /* Pack the separator into the vbox. Remember each of these
;	  * widgets is being packed into a vbox, so they'll be stacked
;	  * vertically. */
;	 gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 5);
;	 gtk_widget_show (separator);
;	 
;	 /* Create another new label, and show it. */
;	 label = gtk_label_new ("gtk_hbox_new (TRUE, 0);");
;	 gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
;	 gtk_box_pack_start (GTK_BOX (box1), label, FALSE, FALSE, 0);
;	 gtk_widget_show (label);
;	 
;	 /* Args are: homogeneous, spacing, expand, fill, padding */
;	 box2 = make_box (TRUE, 0, TRUE, FALSE, 0);
;	 gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
;	 gtk_widget_show (box2);
;	 
;	 /* Args are: homogeneous, spacing, expand, fill, padding */
;	 box2 = make_box (TRUE, 0, TRUE, TRUE, 0);
;	 gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
;	 gtk_widget_show (box2);
;	 
;	 /* Another new separator. */
;	 separator = gtk_hseparator_new ();
;	 /* The last 3 arguments to gtk_box_pack_start are:
;	  * expand, fill, padding. */
;	 gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 5);
;	 gtk_widget_show (separator);
;	 
;	 break;
;
;    case 2:
;
;	 /* Create a new label, remember box1 is a vbox as created 
;	  * near the beginning of main() */
;	 label = gtk_label_new ("gtk_hbox_new (FALSE, 10);");
;	 gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
;	 gtk_box_pack_start (GTK_BOX (box1), label, FALSE, FALSE, 0);
;	 gtk_widget_show (label);
;	 
;	 /* Args are: homogeneous, spacing, expand, fill, padding */
;	 box2 = make_box (FALSE, 10, TRUE, FALSE, 0);
;	 gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
;	 gtk_widget_show (box2);
;	 
;	 /* Args are: homogeneous, spacing, expand, fill, padding */
;	 box2 = make_box (FALSE, 10, TRUE, TRUE, 0);
;	 gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
;	 gtk_widget_show (box2);
;	 
;	 separator = gtk_hseparator_new ();
;	 /* The last 3 arguments to gtk_box_pack_start are:
;	  * expand, fill, padding. */
;	 gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 5);
;	 gtk_widget_show (separator);
;	 
;	 label = gtk_label_new ("gtk_hbox_new (FALSE, 0);");
;	 gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
;	 gtk_box_pack_start (GTK_BOX (box1), label, FALSE, FALSE, 0);
;	 gtk_widget_show (label);
;	 
;	 /* Args are: homogeneous, spacing, expand, fill, padding */
;	 box2 = make_box (FALSE, 0, TRUE, FALSE, 10);
;	 gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
;	 gtk_widget_show (box2);
;	 
;	 /* Args are: homogeneous, spacing, expand, fill, padding */
;	 box2 = make_box (FALSE, 0, TRUE, TRUE, 10);
;	 gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
;	 gtk_widget_show (box2);
;	 
;	 separator = gtk_hseparator_new ();
;	 /* The last 3 arguments to gtk_box_pack_start are: expand, fill, padding. */
;	 gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 5);
;	 gtk_widget_show (separator);
;	 break;
;    
;    case 3:
;
;	 /* This demonstrates the ability to use gtk_box_pack_end() to
;	  * right justify widgets. First, we create a new box as before. */
;	 box2 = make_box (FALSE, 0, FALSE, FALSE, 0);
;
;	 /* Create the label that will be put at the end. */
;	 label = gtk_label_new ("end");
;	 /* Pack it using gtk_box_pack_end(), so it is put on the right
;	  * side of the hbox created in the make_box() call. */
;	 gtk_box_pack_end (GTK_BOX (box2), label, FALSE, FALSE, 0);
;	 /* Show the label. */
;	 gtk_widget_show (label);
;	 
;	 /* Pack box2 into box1 (the vbox remember ? :) */
;	 gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, FALSE, 0);
;	 gtk_widget_show (box2);
;	 
;	 /* A separator for the bottom. */
;	 separator = gtk_hseparator_new ();
;	 /* This explicitly sets the separator to 400 pixels wide by 5 pixels
;	  * high. This is so the hbox we created will also be 400 pixels wide,
;	  * and the "end" label will be separated from the other labels in the
;	  * hbox. Otherwise, all the widgets in the hbox would be packed as
;	  * close together as possible. */
;	 gtk_widget_set_size_request (separator, 400, 5);
;	 /* pack the separator into the vbox (box1) created near the start 
;	  * of main() */
;	 gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 5);
;	 gtk_widget_show (separator);    
;    }
;    
;    /* Create another new hbox.. remember we can use as many as we need! */
;    quitbox = gtk_hbox_new (FALSE, 0);
;    
;    /* Our quit button. */
;    button = gtk_button_new_with_label ("Quit");
;    
;    /* Setup the signal to terminate the program when the button is clicked */
;    g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			       G_CALLBACK (gtk_main_quit),
;			       window);
;    /* Pack the button into the quitbox.
;     * The last 3 arguments to gtk_box_pack_start are:
;     * expand, fill, padding. */
;    gtk_box_pack_start (GTK_BOX (quitbox), button, TRUE, FALSE, 0);
;    /* pack the quitbox into the vbox (box1) */
;    gtk_box_pack_start (GTK_BOX (box1), quitbox, FALSE, FALSE, 0);
;    
;    /* Pack the vbox (box1) which now contains all our widgets, into the
;     * main window. */
;    gtk_container_add (GTK_CONTAINER (window), box1);
;    
;    /* And show everything left */
;    gtk_widget_show (button);
;    gtk_widget_show (quitbox);
;    
;    gtk_widget_show (box1);
;    /* Showing the window last so everything pops up at once. */
;    gtk_widget_show (window);
;    
;    /* And of course, our main function. */
;    gtk_main ();
;
;    /* Control returns here when gtk_main_quit() is called, but not when 
;     * exit() is used. */
;    
;    return 0;
;}

(defpackage "04.03-packbox" (:use :excl :common-lisp))
(in-package "04.03-packbox")

(ff:defun-foreign-callable delete-event ((widget (* gtk:GtkWidget))
					 (event (* gtk:GdkEvent))
					 (data gtk:gpointer))
  (declare (ignore widget event data))
  (format t "~&Delete Event Called~%")
  #+original (gtk:gtk_main_quit)
  #-original (gtk:gtk-main-quit)
  gtk:FALSE)

(defun make-box (homogeneous spacing expand fill padding)
  (let ((box nil)
	(button nil)
	(padstr nil))

    (setq box (gtk:gtk_hbox_new homogeneous spacing))

    (setq button (gtk:gtk_button_new_with_label "gtk_box_pack"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box) button expand fill padding)
    (gtk:gtk_widget_show button)
    
    (setq button (gtk:gtk_button_new_with_label "(box,"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box) button expand fill padding)
    (gtk:gtk_widget_show button)
    
    (setq button (gtk:gtk_button_new_with_label "button,"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box) button expand fill padding)
    (gtk:gtk_widget_show button)

    (if* (eq gtk:TRUE expand)
       then (setq button (gtk:gtk_button_new_with_label "TRUE,"))
       else (setq button (gtk:gtk_button_new_with_label "FALSE,")))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box) button expand fill padding)
    (gtk:gtk_widget_show button)
    
    (setq button (gtk:gtk_button_new_with_label
		  (if* (eq gtk:TRUE fill)
		     then "TRUE,"
		     else "FALSE,")))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box) button expand fill padding)
    (gtk:gtk_widget_show button)

    (setq padstr (format nil "~d);" padding))
    (setq button (gtk:gtk_button_new_with_label padstr))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box) button expand fill padding)
    (gtk:gtk_widget_show button)

    box))

(defun packbox (which)
  (let ((window nil)
	(button nil)
	(box1 nil)
	(box2 nil)
	(separator nil)
	(label nil)
	(quitbox nil)
	(delete-event-cb (ff:register-foreign-callable 'delete-event)))

    (gtk:gtk_init 0 0)

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))

    (gtk:g_signal_connect (gtk:G_OBJECT window) "delete_event"
			  (gtk:G_CALLBACK delete-event-cb) gtk:NULL)

    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 10)
    
    (setq box1 (gtk:gtk_vbox_new gtk:FALSE 0))

    (case which
      (1
       (setq label (gtk:gtk_label_new "gtk_hbox_new (FALSE, 0);"))
       (gtk:gtk_misc_set_alignment (gtk:GTK_MISC label) 0.0 0.0)
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) label gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show label)

       (setq box2 (make-box gtk:FALSE 0 gtk:FALSE gtk:FALSE 0))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show box2)

       (setq box2 (make-box gtk:FALSE 0 gtk:TRUE gtk:FALSE 0))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show box2)

       (setq box2 (make-box gtk:FALSE 0 gtk:TRUE gtk:TRUE 0))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show box2)

       (setq separator (gtk:gtk_hseparator_new))

       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) separator gtk:FALSE gtk:TRUE
			       5)
       (gtk:gtk_widget_show separator)

       (setq label (gtk:gtk_label_new "gtk_hbox_new (TRUE, 0);"))
       (gtk:gtk_misc_set_alignment (gtk:GTK_MISC label) 0.0 0.0)
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) label gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show label)

       (setq box2 (make-box gtk:TRUE 0 gtk:TRUE gtk:FALSE 0))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show box2)

       (setq box2 (make-box gtk:TRUE 0 gtk:TRUE gtk:TRUE 0))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show box2)
       
       (setq separator (gtk:gtk_hseparator_new))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) separator gtk:FALSE gtk:TRUE
			       5)
       (gtk:gtk_widget_show separator))
      
      (2
       (setq label (gtk:gtk_label_new "gtk_hbox_new (FALSE, 10);"))
       (gtk:gtk_misc_set_alignment (gtk:GTK_MISC label) 0.0 0.0)
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) label gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show label)

       (setq box2 (make-box gtk:FALSE 10 gtk:TRUE gtk:FALSE 0))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show box2)

       (setq box2 (make-box gtk:FALSE 10 gtk:TRUE gtk:TRUE 0))
       (gtk:gtk_box_pack_start box1 box2 gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show box2)

       (setq separator (gtk:gtk_hseparator_new))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) separator gtk:FALSE gtk:TRUE
			       5)
       (gtk:gtk_widget_show separator)

       (setq label (gtk:gtk_label_new "gtk_hbox_new (FALSE, 0);"))
       (gtk:gtk_misc_set_alignment (gtk:GTK_MISC label) 0.0 0.0)
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) label gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show label)

       (setq box2 (make-box gtk:FALSE 0 gtk:TRUE gtk:FALSE 10))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show box2)

       (setq box2 (make-box gtk:FALSE 0 gtk:TRUE gtk:TRUE 10))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show box2)
       
       (setq separator (gtk:gtk_hseparator_new))
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) separator gtk:FALSE gtk:TRUE
			       5)
       (gtk:gtk_widget_show separator))

      (3
       (setq box2 (make-box gtk:FALSE 0 gtk:FALSE gtk:FALSE 0))

       (setq label (gtk:gtk_label_new "end"))
       (gtk:gtk_box_pack_end (gtk:GTK_BOX box2) label gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show label)

       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:FALSE 0)
       (gtk:gtk_widget_show box2)

       (setq separator (gtk:gtk_hseparator_new))
       (gtk:gtk_widget_set_size_request separator 400 5)
       (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) separator gtk:FALSE gtk:TRUE
			       5)
       (gtk:gtk_widget_show separator)))
    
    (setq quitbox (gtk:gtk_hbox_new gtk:FALSE 0))
    
    (setq button (gtk:gtk_button_new_with_label "Quit"))
    
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT button) "clicked"
				  (gtk:G_CALLBACK
				   #+original (ff:get-entry-point
					       "gtk_main_quit")
				   #-original delete-event-cb)
				  window)

    (gtk:gtk_box_pack_start (gtk:GTK_BOX quitbox) button gtk:TRUE gtk:FALSE 0)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) quitbox gtk:FALSE gtk:FALSE 0)

    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) box1)

    (gtk:gtk_widget_show button)
    (gtk:gtk_widget_show quitbox)

    (gtk:gtk_widget_show box1)
    (gtk:gtk_widget_show window)

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
  (run-example "04.03-packbox" #'(lambda ()
				   (packbox 1)
				   (packbox 2)
				   (packbox 3))))
