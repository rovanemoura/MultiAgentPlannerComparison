;
;#include <stdio.h>
;#include <gtk/gtk.h>
;
;void destroy( GtkWidget *widget,
;	       gpointer   data )
;{
;    gtk_main_quit ();
;}
;
;int main( int   argc,
;	   char *argv[] )
;{
;    static GtkWidget *window;
;    GtkWidget *scrolled_window;
;    GtkWidget *table;
;    GtkWidget *button;
;    char buffer[32];
;    int i, j;
;    
;    gtk_init (&argc, &argv);
;    
;    /* Create a new dialog window for the scrolled window to be
;     * packed into.  */
;    window = gtk_dialog_new ();
;    g_signal_connect (G_OBJECT (window), "destroy",
;		       G_CALLBACK (destroy), NULL);
;    gtk_window_set_title (GTK_WINDOW (window), "GtkScrolledWindow example");
;    gtk_container_set_border_width (GTK_CONTAINER (window), 0);
;    gtk_widget_set_size_request (window, 300, 300);
;    
;    /* create a new scrolled window. */
;    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
;    
;    gtk_container_set_border_width (GTK_CONTAINER (scrolled_window), 10);
;    
;    /* the policy is one of GTK_POLICY AUTOMATIC, or GTK_POLICY_ALWAYS.
;     * GTK_POLICY_AUTOMATIC will automatically decide whether you need
;     * scrollbars, whereas GTK_POLICY_ALWAYS will always leave the scrollbars
;     * there.  The first one is the horizontal scrollbar, the second, 
;     * the vertical. */
;    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
;				     GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
;    /* The dialog window is created with a vbox packed into it. */								
;    gtk_box_pack_start (GTK_BOX (GTK_DIALOG(window)->vbox), scrolled_window, 
;			 TRUE, TRUE, 0);
;    gtk_widget_show (scrolled_window);
;    
;    /* create a table of 10 by 10 squares. */
;    table = gtk_table_new (10, 10, FALSE);
;    
;    /* set the spacing to 10 on x and 10 on y */
;    gtk_table_set_row_spacings (GTK_TABLE (table), 10);
;    gtk_table_set_col_spacings (GTK_TABLE (table), 10);
;    
;    /* pack the table into the scrolled window */
;    gtk_scrolled_window_add_with_viewport (
;		    GTK_SCROLLED_WINDOW (scrolled_window), table);
;    gtk_widget_show (table);
;    
;    /* this simply creates a grid of toggle buttons on the table
;     * to demonstrate the scrolled window. */
;    for (i = 0; i < 10; i++)
;	for (j = 0; j < 10; j++) {
;	   sprintf (buffer, "button (%d,%d)\n", i, j);
;	   button = gtk_toggle_button_new_with_label (buffer);
;	   gtk_table_attach_defaults (GTK_TABLE (table), button,
;				      i, i+1, j, j+1);
;	   gtk_widget_show (button);
;	}
;    
;    /* Add a "close" button to the bottom of the dialog */
;    button = gtk_button_new_with_label ("close");
;    g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			       G_CALLBACK (gtk_widget_destroy),
;			       window);
;    
;    /* this makes it so the button is the default. */
;    
;    GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
;    gtk_box_pack_start (GTK_BOX (GTK_DIALOG (window)->action_area), button, TRUE, TRUE, 0);
;    
;    /* This grabs this button to be the default button. Simply hitting
;     * the "Enter" key will cause this button to activate. */
;    gtk_widget_grab_default (button);
;    gtk_widget_show (button);
;    
;    gtk_widget_show (window);
;    
;    gtk_main();
;    
;    return 0;
;}

(defpackage "10.09-scrolledwin" (:use :excl :common-lisp))
(in-package "10.09-scrolledwin")

(ff:defun-foreign-callable destroy ((widget (* gtk:GtkWidget))
				    (data gtk:gpointer))
  (declare (ignore widget data))
  #+original (gtk:gtk_main_quit)
  #-original (gtk:gtk-main-quit))

(let ((window nil))
  (defun scrolledwin ()
    (let ((scrolled-window nil)
	  (table nil)
	  (button nil))

      (gtk:gtk_init 0 0)
      
      (setq window (gtk:gtk_dialog_new))
      (gtk:g_signal_connect (gtk:G_OBJECT window) "destroy"
			    (gtk:G_CALLBACK
			     (ff:register-foreign-callable 'destroy))
			    gtk:NULL)
      (gtk:gtk_window_set_title (gtk:GTK_WINDOW window)
				"GtkScrolledWindow example")
      (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 0)
      (gtk:gtk_widget_set_size_request window 300 300)

      (setq scrolled-window (gtk:gtk_scrolled_window_new gtk:NULL gtk:NULL))

      (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER scrolled-window)
					  10)
      (gtk:gtk_scrolled_window_set_policy
       (gtk:GTK_SCROLLED_WINDOW scrolled-window)
       gtk:GTK_POLICY_AUTOMATIC
       gtk:GTK_POLICY_ALWAYS)

      (gtk:gtk_box_pack_start (gtk:GTK_BOX
			       (ff:fslot-value-typed
				'gtk:GtkDialog nil
				(gtk:GTK_DIALOG window)
				'gtk::vbox))
			      scrolled-window 
			      gtk:TRUE gtk:TRUE 0)
      (gtk:gtk_widget_show scrolled-window)

      (setq table (gtk:gtk_table_new 10 10 gtk:FALSE))

      (gtk:gtk_table_set_row_spacings (gtk:GTK_TABLE table) 10)
      (gtk:gtk_table_set_col_spacings (gtk:GTK_TABLE table) 10)

      (gtk:gtk_scrolled_window_add_with_viewport
       (gtk:GTK_SCROLLED_WINDOW scrolled-window) table)
      (gtk:gtk_widget_show table)

      (dotimes (i 10)
	(dotimes (j 10)
	  (let ((buffer (format nil "button (~d~d)~%" i j)))
	    (setq button (gtk:gtk_toggle_button_new_with_label buffer))
	    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) button
					   i (1+ i) j (1+ j))
	    (gtk:gtk_widget_show button))))

      (setq button (gtk:gtk_button_new_with_label "close"))
      (gtk:g_signal_connect_swapped (gtk:G_OBJECT button) "clicked"
				    (gtk:G_CALLBACK
				     (ff:get-entry-point "gtk_widget_destroy"))
				    window)

      (gtk:GTK_WIDGET_SET_FLAGS button gtk:GTK_CAN_DEFAULT)
      (gtk:gtk_box_pack_start
       (gtk:GTK_BOX
	(ff:fslot-value-typed 'gtk:GtkDialog nil
			      (gtk:GTK_DIALOG window)
			      'gtk::action_area))
       button gtk:TRUE gtk:TRUE 0)

      (gtk:gtk_widget_grab_default button)
      (gtk:gtk_widget_show button)

      (gtk:gtk_widget_show window)

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
  (run-example "10.09-scrolledwin" #'scrolledwin))
