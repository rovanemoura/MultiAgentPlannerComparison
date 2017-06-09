;
;#include <stdio.h>
;#include <gtk/gtk.h>
;
;/* This function rotates the position of the tabs */
;void rotate_book( GtkButton   *button,
;		   GtkNotebook *notebook )
;{
;    gtk_notebook_set_tab_pos (notebook, (notebook->tab_pos + 1) % 4);
;}
;
;/* Add/Remove the page tabs and the borders */
;void tabsborder_book( GtkButton   *button,
;		       GtkNotebook *notebook )
;{
;    gint tval = FALSE;
;    gint bval = FALSE;
;    if (notebook->show_tabs == 0)
;	     tval = TRUE; 
;    if (notebook->show_border == 0)
;	     bval = TRUE;
;    
;    gtk_notebook_set_show_tabs (notebook, tval);
;    gtk_notebook_set_show_border (notebook, bval);
;}
;
;/* Remove a page from the notebook */
;void remove_book( GtkButton   *button,
;		   GtkNotebook *notebook )
;{
;    gint page;
;    
;    page = gtk_notebook_get_current_page (notebook);
;    gtk_notebook_remove_page (notebook, page);
;    /* Need to refresh the widget -- 
;     This forces the widget to redraw itself. */
;    gtk_widget_queue_draw (GTK_WIDGET (notebook));
;}
;
;gint delete( GtkWidget *widget,
;	      GtkWidget *event,
;	      gpointer   data )
;{
;    gtk_main_quit ();
;    return FALSE;
;}
;
;int main( int argc,
;	   char *argv[] )
;{
;    GtkWidget *window;
;    GtkWidget *button;
;    GtkWidget *table;
;    GtkWidget *notebook;
;    GtkWidget *frame;
;    GtkWidget *label;
;    GtkWidget *checkbutton;
;    int i;
;    char bufferf[32];
;    char bufferl[32];
;    
;    gtk_init (&argc, &argv);
;    
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;    
;    g_signal_connect (G_OBJECT (window), "delete_event",
;		       G_CALLBACK (delete), NULL);
;    
;    gtk_container_set_border_width (GTK_CONTAINER (window), 10);
;
;    table = gtk_table_new (3, 6, FALSE);
;    gtk_container_add (GTK_CONTAINER (window), table);
;    
;    /* Create a new notebook, place the position of the tabs */
;    notebook = gtk_notebook_new ();
;    gtk_notebook_set_tab_pos (GTK_NOTEBOOK (notebook), GTK_POS_TOP);
;    gtk_table_attach_defaults (GTK_TABLE (table), notebook, 0, 6, 0, 1);
;    gtk_widget_show (notebook);
;    
;    /* Let's append a bunch of pages to the notebook */
;    for (i = 0; i < 5; i++) {
;	 sprintf(bufferf, "Append Frame %d", i + 1);
;	 sprintf(bufferl, "Page %d", i + 1);
;	 
;	 frame = gtk_frame_new (bufferf);
;	 gtk_container_set_border_width (GTK_CONTAINER (frame), 10);
;	 gtk_widget_set_size_request (frame, 100, 75);
;	 gtk_widget_show (frame);
;	 
;	 label = gtk_label_new (bufferf);
;	 gtk_container_add (GTK_CONTAINER (frame), label);
;	 gtk_widget_show (label);
;	 
;	 label = gtk_label_new (bufferl);
;	 gtk_notebook_append_page (GTK_NOTEBOOK (notebook), frame, label);
;    }
;      
;    /* Now let's add a page to a specific spot */
;    checkbutton = gtk_check_button_new_with_label ("Check me please!");
;    gtk_widget_set_size_request (checkbutton, 100, 75);
;    gtk_widget_show (checkbutton);
;   
;    label = gtk_label_new ("Add page");
;    gtk_notebook_insert_page (GTK_NOTEBOOK (notebook), checkbutton, label, 2);
;    
;    /* Now finally let's prepend pages to the notebook */
;    for (i = 0; i < 5; i++) {
;	 sprintf (bufferf, "Prepend Frame %d", i + 1);
;	 sprintf (bufferl, "PPage %d", i + 1);
;	 
;	 frame = gtk_frame_new (bufferf);
;	 gtk_container_set_border_width (GTK_CONTAINER (frame), 10);
;	 gtk_widget_set_size_request (frame, 100, 75);
;	 gtk_widget_show (frame);
;	 
;	 label = gtk_label_new (bufferf);
;	 gtk_container_add (GTK_CONTAINER (frame), label);
;	 gtk_widget_show (label);
;	 
;	 label = gtk_label_new (bufferl);
;	 gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook), frame, label);
;    }
;    
;    /* Set what page to start at (page 4) */
;    gtk_notebook_set_current_page (GTK_NOTEBOOK (notebook), 3);
;
;    /* Create a bunch of buttons */
;    button = gtk_button_new_with_label ("close");
;    g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			       G_CALLBACK (delete), NULL);
;    gtk_table_attach_defaults (GTK_TABLE (table), button, 0, 1, 1, 2);
;    gtk_widget_show (button);
;    
;    button = gtk_button_new_with_label ("next page");
;    g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			       G_CALLBACK (gtk_notebook_next_page),
;			       notebook);
;    gtk_table_attach_defaults (GTK_TABLE (table), button, 1, 2, 1, 2);
;    gtk_widget_show (button);
;    
;    button = gtk_button_new_with_label ("prev page");
;    g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			       G_CALLBACK (gtk_notebook_prev_page),
;			       notebook);
;    gtk_table_attach_defaults (GTK_TABLE (table), button, 2, 3, 1, 2);
;    gtk_widget_show (button);
;    
;    button = gtk_button_new_with_label ("tab position");
;    g_signal_connect (G_OBJECT (button), "clicked",
;		       G_CALLBACK (rotate_book),
;		       notebook);
;    gtk_table_attach_defaults (GTK_TABLE (table), button, 3, 4, 1, 2);
;    gtk_widget_show (button);
;    
;    button = gtk_button_new_with_label ("tabs/border on/off");
;    g_signal_connect (G_OBJECT (button), "clicked",
;		       G_CALLBACK (tabsborder_book),
;		       notebook);
;    gtk_table_attach_defaults (GTK_TABLE (table), button, 4, 5, 1, 2);
;    gtk_widget_show (button);
;    
;    button = gtk_button_new_with_label ("remove page");
;    g_signal_connect (G_OBJECT (button), "clicked",
;		       G_CALLBACK (remove_book),
;		       notebook);
;    gtk_table_attach_defaults (GTK_TABLE (table), button, 5, 6, 1, 2);
;    gtk_widget_show (button);
;    
;    gtk_widget_show (table);
;    gtk_widget_show (window);
;    
;    gtk_main ();
;    
;    return 0;
;}

(defpackage "10.12-notebook" (:use :excl :common-lisp))
(in-package "10.12-notebook")

(ff:defun-foreign-callable rotate-book ((button (* gtk:GtkButton))
					(notebook (* gtk:GtkNotebook)))
  (declare (ignore button))
  (gtk:gtk_notebook_set_tab_pos
   notebook (mod (1+ (ff:fslot-value-typed 'gtk:GtkNotebook nil
					   notebook 'gtk::tab_pos))
		 4)))

(ff:defun-foreign-callable tabsborder-book ((button (* gtk:GtkButton))
					    (notebook (* gtk:GtkNotebook)))
  (declare (ignore button))
  (let ((tval gtk:FALSE)
	(bval gtk:FALSE))
    (when (eql (ff:fslot-value-typed 'gtk:GtkNotebook nil
				     notebook 'gtk::show_tabs)
	       0)
      (setq tval gtk:TRUE))
    (when (eql (ff:fslot-value-typed 'gtk:GtkNotebook nil
				     notebook 'gtk::show_border)
	       0)
      (setq bval gtk:TRUE))

    (gtk:gtk_notebook_set_show_tabs notebook tval)
    (gtk:gtk_notebook_set_show_border notebook bval)))

(ff:defun-foreign-callable remove-book ((button (* gtk:GtkButton))
					(notebook (* gtk:GtkNotebook)))
  (declare (ignore button))
  (let ((page nil))
    (setq page (gtk:gtk_notebook_get_current_page notebook))
    (gtk:gtk_notebook_remove_page notebook page)
    (gtk:gtk_widget_queue_draw (gtk:GTK_WIDGET notebook))))

(ff:defun-foreign-callable nb-delete ((widget (* gtk:GtkWidget))
				      (event (* gtk:GtkWidget))
				      (data gtk:gpointer))
  (declare (ignore widget event data))  
  #+original (gtk:gtk_main_quit)
  #-original (gtk:gtk-main-quit)
  gtk:FALSE)

(defun notebook ()
  (let ((window nil)
	(button nil)
	(table nil)
	(notebook nil)
	(frame nil)
	(label nil)
	(checkbutton nil)
	(bufferf nil)
	(bufferl nil))

    (gtk:gtk_init 0 0)

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))

    (gtk:g_signal_connect (gtk:G_OBJECT window) "delete_event"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'nb-delete))
			  gtk:NULL)

    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 10)

    (setq table (gtk:gtk_table_new 3 6 gtk:FALSE))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) table)

    (setq notebook (gtk:gtk_notebook_new))
    (gtk:gtk_notebook_set_tab_pos (gtk:GTK_NOTEBOOK notebook) gtk:GTK_POS_TOP)
    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) notebook 0 6 0 1) ;
    (gtk:gtk_widget_show notebook)

    (dotimes (i 5)
      (setq bufferf (format nil "Append Frame ~d" (1+ i)))
      (setq bufferl (format nil "Page ~d" (1+ i)))
      
      (setq frame (gtk:gtk_frame_new bufferf))
      (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER frame) 10)
      (gtk:gtk_widget_set_size_request frame 100 75)
      (gtk:gtk_widget_show frame)

      (setq label (gtk:gtk_label_new bufferf))
      (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) label)
      (gtk:gtk_widget_show label)

      (setq label (gtk:gtk_label_new bufferl))
      (gtk:gtk_notebook_append_page (gtk:GTK_NOTEBOOK notebook) frame label))

    (setq checkbutton (gtk:gtk_check_button_new_with_label "Check me please!"))
    (gtk:gtk_widget_set_size_request checkbutton 100 75)
    (gtk:gtk_widget_show checkbutton)

    (setq label (gtk:gtk_label_new "Add page"))
    (gtk:gtk_notebook_insert_page (gtk:GTK_NOTEBOOK notebook) checkbutton
				  label 2)

    (dotimes (i 5)
     (setq bufferf (format nil "Prepend Frame ~d" (1+ i)))
     (setq bufferl (format nil "PPage ~d" (1+ i)))

     (setq frame (gtk:gtk_frame_new bufferf))
     (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER frame) 10)
     (gtk:gtk_widget_set_size_request frame 100 75)
     (gtk:gtk_widget_show frame)

     (setq label (gtk:gtk_label_new bufferf))
     (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) label)
     (gtk:gtk_widget_show label)

     (setq label (gtk:gtk_label_new bufferl))
     (gtk:gtk_notebook_prepend_page (gtk:GTK_NOTEBOOK notebook) frame label))

    (gtk:gtk_notebook_set_current_page (gtk:GTK_NOTEBOOK notebook) 3)

    (setq button (gtk:gtk_button_new_with_label "close"))
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT button) "clicked"
				   (gtk:G_CALLBACK
				    (ff:register-foreign-callable 'nb-delete))
				   gtk:NULL)
    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) button 0 1 1 2)
    (gtk:gtk_widget_show button)

    (setq button (gtk:gtk_button_new_with_label "next page"))
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT button) "clicked"
				  (gtk:G_CALLBACK
				   (ff:get-entry-point
				    "gtk_notebook_next_page"))
				  notebook)
    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) button 1 2 1 2)
    (gtk:gtk_widget_show button)

    (setq button (gtk:gtk_button_new_with_label "prev page"))
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT button) "clicked"
				  (gtk:G_CALLBACK
				   (ff:get-entry-point
				    "gtk_notebook_prev_page"))
				  notebook)
    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) button 2 3 1 2)
    (gtk:gtk_widget_show button)

    (setq button (gtk:gtk_button_new_with_label "tab position"))
    (gtk:g_signal_connect (gtk:G_OBJECT button) "clicked"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'rotate-book))
			  notebook)
    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) button 3 4 1 2)
    (gtk:gtk_widget_show button)

    (setq button (gtk:gtk_button_new_with_label "tabs/border on/off"))
    (gtk:g_signal_connect (gtk:G_OBJECT button) "clicked"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'tabsborder-book))
			  notebook)
    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) button 4 5 1 2)
    (gtk:gtk_widget_show button)

    (setq button (gtk:gtk_button_new_with_label "remove page"))
    (gtk:g_signal_connect (gtk:G_OBJECT button) "clicked"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'remove-book))
			  notebook)
    (gtk:gtk_table_attach_defaults (gtk:GTK_TABLE table) button 5 6 1 2)
    (gtk:gtk_widget_show button)

    (gtk:gtk_widget_show table)
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
  (run-example "10.12-notebook" #'notebook))
