;
;#include <gtk/gtk.h>
;
;/* This is a callback function. The data arguments are ignored
; * in this example. More on callbacks below. */
;void hello( GtkWidget *widget,
;	     gpointer   data )
;{
;    g_print ("Hello World\n");
;}
;
;gint delete_event( GtkWidget *widget,
;		    GdkEvent  *event,
;		    gpointer   data )
;{
;    /* If you return FALSE in the "delete_event" signal handler,
;     * GTK will emit the "destroy" signal. Returning TRUE means
;     * you don't want the window to be destroyed.
;     * This is useful for popping up 'are you sure you want to quit?'
;     * type dialogs. */
;
;    g_print ("delete event occurred\n");
;
;    /* Change TRUE to FALSE and the main window will be destroyed with
;     * a "delete_event". */
;
;    return TRUE;
;}
;
;/* Another callback */
;void destroy( GtkWidget *widget,
;	       gpointer   data )
;{
;    gtk_main_quit ();
;}
;
;int main( int   argc,
;	   char *argv[] )
;{
;    /* GtkWidget is the storage type for widgets */
;    GtkWidget *window;
;    GtkWidget *button;
;    
;    /* This is called in all GTK applications. Arguments are parsed
;     * from the command line and are returned to the application. */
;    gtk_init (&argc, &argv);
;    
;    /* create a new window */
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;    
;    /* When the window is given the "delete_event" signal (this is given
;     * by the window manager, usually by the "close" option, or on the
;     * titlebar), we ask it to call the delete_event () function
;     * as defined above. The data passed to the callback
;     * function is NULL and is ignored in the callback function. */
;    g_signal_connect (G_OBJECT (window), "delete_event",
;		       G_CALLBACK (delete_event), NULL);
;    
;    /* Here we connect the "destroy" event to a signal handler.  
;     * This event occurs when we call gtk_widget_destroy() on the window,
;     * or if we return FALSE in the "delete_event" callback. */
;    g_signal_connect (G_OBJECT (window), "destroy",
;		       G_CALLBACK (destroy), NULL);
;    
;    /* Sets the border width of the window. */
;    gtk_container_set_border_width (GTK_CONTAINER (window), 10);
;    
;    /* Creates a new button with the label "Hello World". */
;    button = gtk_button_new_with_label ("Hello World");
;    
;    /* When the button receives the "clicked" signal, it will call the
;     * function hello() passing it NULL as its argument.  The hello()
;     * function is defined above. */
;    g_signal_connect (G_OBJECT (button), "clicked",
;		       G_CALLBACK (hello), NULL);
;    
;    /* This will cause the window to be destroyed by calling
;     * gtk_widget_destroy(window) when "clicked".  Again, the destroy
;     * signal could come from here, or the window manager. */
;    g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			       G_CALLBACK (gtk_widget_destroy),
;			       window);
;    
;    /* This packs the button into the window (a gtk container). */
;    gtk_container_add (GTK_CONTAINER (window), button);
;    
;    /* The final step is to display this newly created widget. */
;    gtk_widget_show (button);
;    
;    /* and the window */
;    gtk_widget_show (window);
;    
;    /* All GTK applications must have a gtk_main(). Control ends here
;     * and waits for an event to occur (like a key press or
;     * mouse event). */
;    gtk_main ();
;    
;    return 0;
;}

(defpackage "02.01-helloworld" (:use :excl :common-lisp))
(in-package "02.01-helloworld")

(ff:defun-foreign-callable hello ((widget (* gtk:GtkWidget))
				  (data gtk:gpointer))
  (declare (ignore widget data))
  #+original (format t "~&Hello Lisp World~%")
  #-original (format t "~&Hello Lisp World [~a]~%" mp:*current-process*)
  (values))

(ff:defun-foreign-callable delete-event ((widget (* gtk:GtkWidget))
					 (event (* gtk:GdkEvent))
					 (data (* gtk:gpointer)))
  (declare (ignore widget event data))
  (format t "~&Delete Event Called~%")
  gtk:FALSE)

(ff:defun-foreign-callable destroy ((widget (* gtk:GtkWidget))
				    (data gtk:gpointer))
  (declare (ignore widget data))
  (format t "~&Destroy Event Called~%")
  #+original (gtk:gtk_main_quit)
  #-original (gtk:gtk-main-quit)
  )

(defun helloworld ()
  (gtk:gtk_init 0 0)

  (let ((window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
	(button #+original (gtk:gtk_button_new_with_label "Hello from Lisp")
		#-original (gtk:gtk_button_new_with_label (format nil "~
Hello from Lisp Process: ~a" mp:*current-process*))))

    (gtk:g_signal_connect (gtk:G_OBJECT window) "delete_event"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'delete-event))
			  gtk:NULL)

    (gtk:g_signal_connect (gtk:G_OBJECT window) "destroy"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'destroy))
			  gtk:NULL)

    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 10)

    (gtk:g_signal_connect (gtk:G_OBJECT button) "clicked"
			    (gtk:G_CALLBACK
			     (ff:register-foreign-callable 'hello))
			    gtk:NULL)

    (gtk:g_signal_connect_swapped
     (gtk:G_OBJECT button) "clicked"
     (gtk:G_CALLBACK (ff:get-entry-point "gtk_widget_destroy"))
     window)

    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) button)
    
    (gtk:gtk_widget_show button)
    
    (gtk:gtk_widget_show window)
    
    #+original (gtk:gtk_main)
    #-original (gtk:gtk-main)
    ))



(flet ((run-example (name function)
	 ;; workaround for bogus (imo) redef. warnings generated by defvar
	 (declare (special gtk::*run-example*))
	 (unless (boundp 'gtk::*run-example*)
	   (setq gtk::*run-example* t))
	 (when gtk::*run-example*
	   (mp:process-run-function
	    (format nil "GTK+ Example: ~a" name)
	    function))))
  (run-example "02.01-helloworld" #'helloworld))
