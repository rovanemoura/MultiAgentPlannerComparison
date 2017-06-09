;/* example-start paned paned.c */
;
;#include <gtk/gtk.h>
;   
;/* Create the list of "messages" */
;GtkWidget *create_list( void )
;{
;
;    GtkWidget *scrolled_window;
;    GtkWidget *list;
;    GtkWidget *list_item;
;   
;    int i;
;    char buffer[16];
;   
;    /* Create a new scrolled window, with scrollbars only if needed */
;    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
;    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
;				     GTK_POLICY_AUTOMATIC, 
;				     GTK_POLICY_AUTOMATIC);
;   
;    /* Create a new list and put it in the scrolled window */
;    list = gtk_list_new ();
;    gtk_scrolled_window_add_with_viewport (
;		GTK_SCROLLED_WINDOW (scrolled_window), list);
;    gtk_widget_show (list);
;   
;    /* Add some messages to the window */
;    for (i=0; i<10; i++) {
;
;	 sprintf(buffer,"Message #%d",i);
;	 list_item = gtk_list_item_new_with_label (buffer);
;	 gtk_container_add (GTK_CONTAINER(list), list_item);
;	 gtk_widget_show (list_item);
;
;    }
;   
;    return scrolled_window;
;}
;   
;/* Add some text to our text widget - this is a callback that is invoked
;when our window is realized. We could also force our window to be
;realized with gtk_widget_realize, but it would have to be part of
;a hierarchy first */
;
;void realize_text( GtkWidget *text,
;		    gpointer data )
;{
;    gtk_text_freeze (GTK_TEXT (text));
;    gtk_text_insert (GTK_TEXT (text), NULL, &text->style->black, NULL,
;    "From: pathfinder@nasa.gov\n"
;    "To: mom@nasa.gov\n"
;    "Subject: Made it!\n"
;    "\n"
;    "We just got in this morning. The weather has been\n"
;    "great - clear but cold, and there are lots of fun sights.\n"
;    "Sojourner says hi. See you soon.\n"
;    " -Path\n", -1);
;   
;    gtk_text_thaw (GTK_TEXT (text));
;}
;   
;/* Create a scrolled text area that displays a "message" */
;GtkWidget *create_text( void )
;{
;    GtkWidget *table;
;    GtkWidget *text;
;    GtkWidget *hscrollbar;
;    GtkWidget *vscrollbar;
;   
;    /* Create a table to hold the text widget and scrollbars */
;    table = gtk_table_new (2, 2, FALSE);
;   
;    /* Put a text widget in the upper left hand corner. Note the use of
;     * GTK_SHRINK in the y direction */
;    text = gtk_text_new (NULL, NULL);
;    gtk_table_attach (GTK_TABLE (table), text, 0, 1, 0, 1,
;		       GTK_FILL | GTK_EXPAND,
;		       GTK_FILL | GTK_EXPAND | GTK_SHRINK, 0, 0);
;    gtk_widget_show (text);
;   
;    /* Put a HScrollbar in the lower left hand corner */
;    hscrollbar = gtk_hscrollbar_new (GTK_TEXT (text)->hadj);
;    gtk_table_attach (GTK_TABLE (table), hscrollbar, 0, 1, 1, 2,
;		       GTK_EXPAND | GTK_FILL, GTK_FILL, 0, 0);
;    gtk_widget_show (hscrollbar);
;   
;    /* And a VScrollbar in the upper right */
;    vscrollbar = gtk_vscrollbar_new (GTK_TEXT (text)->vadj);
;    gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1,
;		       GTK_FILL, GTK_EXPAND | GTK_FILL | GTK_SHRINK, 0, 0);
;    gtk_widget_show (vscrollbar);
;   
;    /* Add a handler to put a message in the text widget when it is realized */
;    gtk_signal_connect (GTK_OBJECT (text), "realize",
;			 GTK_SIGNAL_FUNC (realize_text), NULL);
;   
;    return table;
;}
;   
;int main( int   argc,
;	   char *argv[] )
;{
;    GtkWidget *window;
;    GtkWidget *vpaned;
;    GtkWidget *list;
;    GtkWidget *text;
;
;    gtk_init (&argc, &argv);
;   
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;    gtk_window_set_title (GTK_WINDOW (window), "Paned Windows");
;    gtk_signal_connect (GTK_OBJECT (window), "destroy",
;			 GTK_SIGNAL_FUNC (gtk_main_quit), NULL);
;    gtk_container_set_border_width (GTK_CONTAINER (window), 10);
;    gtk_widget_set_usize (GTK_WIDGET(window), 450, 400);
;
;    /* create a vpaned widget and add it to our toplevel window */
;   
;    vpaned = gtk_vpaned_new ();
;    gtk_container_add (GTK_CONTAINER(window), vpaned);
;    gtk_paned_set_handle_size (GTK_PANED(vpaned),
;				10);
;    gtk_paned_set_gutter_size (GTK_PANED(vpaned),
;				15);                       
;    gtk_widget_show (vpaned);
;   
;    /* Now create the contents of the two halves of the window */
;   
;    list = create_list ();
;    gtk_paned_add1 (GTK_PANED(vpaned), list);
;    gtk_widget_show (list);
;   
;    text = create_text ();
;    gtk_paned_add2 (GTK_PANED(vpaned), text);
;    gtk_widget_show (text);
;    gtk_widget_show (window);
;    gtk_main ();
;    return 0;
;}
;/* example-end */

(defpackage "10.07-paned" (:use :excl :common-lisp))
(in-package "10.07-paned")

(defun create-list ()
  (let ((scrolled-window nil)
	(list nil)
	(list-item nil))

    (setq scrolled-window (gtk:gtk_scrolled_window_new gtk:NULL gtk:NULL))
    (gtk:gtk_scrolled_window_set_policy
     (gtk:GTK_SCROLLED_WINDOW scrolled-window)
     gtk:GTK_POLICY_AUTOMATIC 
     gtk:GTK_POLICY_AUTOMATIC)

    (setq list (gtk:gtk_list_new))
    (gtk:gtk_scrolled_window_add_with_viewport
     (gtk:GTK_SCROLLED_WINDOW scrolled-window)
     list)
    (gtk:gtk_widget_show list)

    (dotimes (i 10)
      (let ((buffer (format nil "Message #~d" i)))
	(setq list-item (gtk:gtk_list_item_new_with_label buffer))
	(gtk:gtk_container_add (gtk:GTK_CONTAINER list) list-item)
	(gtk:gtk_widget_show list-item)))

    scrolled-window))

(ff:defun-foreign-callable realize-text ((text (* gtk:GtkWidget))
					 (data gtk:gpointer))
  (declare (ignore data))
  (gtk:gtk_text_freeze (gtk:GTK_TEXT text))
  (gtk:gtk_text_insert (gtk:GTK_TEXT text) gtk:NULL
		       ;; &text->style->black
		       (ff:fslot-address-typed
			'gtk:GdkColor nil
			;; text->style->black
			(ff:fslot-value-typed
			 'gtk:GtkStyle nil
			 ;; text->style
			 (ff:fslot-value-typed
			  'gtk:GtkWidget nil
			  text
			  'gtk::style)
			 'gtk::black))
		       gtk:NULL
		       (format nil "~
From: pathfinder@nasa.gov~%~
To: mom@nasa.gov~%~
Subject: Made it!~%~
~%~
We just got in this morning. The weather has been~%~
great - clear but cold and there are lots of fun sights.
Sojourner says hi. See you soon.~%~
 -Path~%")
		       -1)
  (gtk:gtk_text_thaw (gtk:GTK_TEXT text)))

(defun create-text ()
  (let ((table nil)
	(text nil)
	(hscrollbar nil)
	(vscrollbar nil))
    
    (setq table (gtk:gtk_table_new 2 2 gtk:FALSE))

    (setq text (gtk:gtk_text_new gtk:NULL gtk:NULL))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) text 0 1 0 1
			  (logior gtk:GTK_FILL  gtk:GTK_EXPAND)
			  (logior gtk:GTK_FILL  gtk:GTK_EXPAND  gtk:GTK_SHRINK)
			  0 0)
    (gtk:gtk_widget_show text)

    (setq hscrollbar (gtk:gtk_hscrollbar_new
		      (ff:fslot-value-typed 'gtk:GtkText nil
					    (gtk:GTK_TEXT text) 'gtk::hadj)))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) hscrollbar 0 1 1 2
			  (logior gtk:GTK_EXPAND  gtk:GTK_FILL)
			  gtk:GTK_FILL 0 0)
    (gtk:gtk_widget_show hscrollbar)

    (setq vscrollbar (gtk:gtk_vscrollbar_new
		      (ff:fslot-value-typed 'gtk:GtkText nil
					    (gtk:GTK_TEXT text) 'gtk::vadj)))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) vscrollbar 1 2 0 1
			  gtk:GTK_FILL
			  (logior gtk:GTK_EXPAND  gtk:GTK_FILL gtk:GTK_SHRINK)
			  0 0)
    (gtk:gtk_widget_show vscrollbar)

    (gtk:gtk_signal_connect (gtk:GTK_OBJECT text) "realize"
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'realize-text))
			    gtk:NULL)
    table))

(ff:defun-foreign-callable cb-gtk-main-quit ()
  (gtk:gtk-main-quit))

(defun paned ()
  (let ((window nil)
	(vpaned nil)
	(list nil)
	(text nil))

    (gtk:gtk_init 0 0)

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "Paned Windows")
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "destroy"
			    (gtk:GTK_SIGNAL_FUNC
			     #+original (ff:get-entry-point "gtk_main_quit")
			     #-original (ff:register-foreign-callable
					 'cb-gtk-main-quit))
			    gtk:NULL)
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 10)
    (gtk:gtk_widget_set_usize (gtk:GTK_WIDGET window) 450 400)

    (setq vpaned (gtk:gtk_vpaned_new))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) vpaned)
    (gtk:gtk_paned_set_handle_size (gtk:GTK_PANED vpaned) 10)
    (gtk:gtk_paned_set_gutter_size (gtk:GTK_PANED vpaned) 15)
    (gtk:gtk_widget_show vpaned)

    (setq list (create-list))
    (gtk:gtk_paned_add1 (gtk:GTK_PANED vpaned) list)
    (gtk:gtk_widget_show list)

    (setq text (create-text))
    (gtk:gtk_paned_add2 (gtk:GTK_PANED vpaned) text)
    (gtk:gtk_widget_show text)
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
  (run-example "10.07-paned" #'paned))
