;/* example-start clist clist.c */
;
;#include <gtk/gtk.h>
;
;/* User clicked the "Add List" button. */
;void button_add_clicked( gpointer data )
;{
;    int indx;
; 
;    /* Something silly to add to the list. 4 rows of 2 columns each */
;    gchar *drink[4][2] = { { "Milk",    "3 Oz" },
;			    { "Water",   "6 l" },
;			    { "Carrots", "2" },
;			    { "Snakes",  "55" } };
;
;    /* Here we do the actual adding of the text. It's done once for
;     * each row.
;     */
;    for ( indx=0 ; indx < 4 ; indx++ )
;	 gtk_clist_append( (GtkCList *) data, drink[indx]);
;
;    return;
;}
;
;/* User clicked the "Clear List" button. */
;void button_clear_clicked( gpointer data )
;{
;    /* Clear the list using gtk_clist_clear. This is much faster than
;     * calling gtk_clist_remove once for each row.
;     */
;    gtk_clist_clear( (GtkCList *) data);
;
;    return;
;}
;
;/* The user clicked the "Hide/Show titles" button. */
;void button_hide_show_clicked( gpointer data )
;{
;    /* Just a flag to remember the status. 0 = currently visible */
;    static short int flag = 0;
;
;    if (flag == 0)
;    {
;	 /* Hide the titles and set the flag to 1 */
;	 gtk_clist_column_titles_hide((GtkCList *) data);
;	 flag++;
;    }
;    else
;    {
;	 /* Show the titles and reset flag to 0 */
;	 gtk_clist_column_titles_show((GtkCList *) data);
;	 flag--;
;    }
;
;    return;
;}
;
;/* If we come here, then the user has selected a row in the list. */
;void selection_made( GtkWidget      *clist,
;		      gint            row,
;		      gint            column,
;		      GdkEventButton *event,
;		      gpointer        data )
;{
;    gchar *text;
;
;    /* Get the text that is stored in the selected row and column
;     * which was clicked in. We will receive it as a pointer in the
;     * argument text.
;     */
;    gtk_clist_get_text(GTK_CLIST(clist), row, column, &text);
;
;    /* Just prints some information about the selected row */
;    g_print("You selected row %d. More specifically you clicked in "
;	     "column %d, and the text in this cell is %s\n\n",
;	     row, column, text);
;
;    return;
;}
;
;int main( int    argc,
;	   gchar *argv[] )
;{                                  
;    GtkWidget *window;
;    GtkWidget *vbox, *hbox;
;    GtkWidget *scrolled_window, *clist;
;    GtkWidget *button_add, *button_clear, *button_hide_show;    
;    gchar *titles[2] = { "Ingredients", "Amount" };
;
;    gtk_init(&argc, &argv);
;    
;    window=gtk_window_new(GTK_WINDOW_TOPLEVEL);
;    gtk_widget_set_usize(GTK_WIDGET(window), 300, 150);
;
;    gtk_window_set_title(GTK_WINDOW(window), "GtkCList Example");
;    gtk_signal_connect(GTK_OBJECT(window),
;			"destroy",
;			GTK_SIGNAL_FUNC(gtk_main_quit),
;			NULL);
;    
;    vbox=gtk_vbox_new(FALSE, 5);
;    gtk_container_set_border_width(GTK_CONTAINER(vbox), 5);
;    gtk_container_add(GTK_CONTAINER(window), vbox);
;    gtk_widget_show(vbox);
;    
;    /* Create a scrolled window to pack the CList widget into */
;    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
;    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
;				     GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
;
;    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
;    gtk_widget_show (scrolled_window);
;
;    /* Create the CList. For this example we use 2 columns */
;    clist = gtk_clist_new_with_titles( 2, titles);
;
;    /* When a selection is made, we want to know about it. The callback
;     * used is selection_made, and its code can be found further down */
;    gtk_signal_connect(GTK_OBJECT(clist), "select_row",
;			GTK_SIGNAL_FUNC(selection_made),
;			NULL);
;
;    /* It isn't necessary to shadow the border, but it looks nice :) */
;    gtk_clist_set_shadow_type (GTK_CLIST(clist), GTK_SHADOW_OUT);
;
;    /* What however is important, is that we set the column widths as
;     * they will never be right otherwise. Note that the columns are
;     * numbered from 0 and up (to 1 in this case).
;     */
;    gtk_clist_set_column_width (GTK_CLIST(clist), 0, 150);
;
;    /* Add the CList widget to the vertical box and show it. */
;    gtk_container_add(GTK_CONTAINER(scrolled_window), clist);
;    gtk_widget_show(clist);
;
;    /* Create the buttons and add them to the window. See the button
;     * tutorial for more examples and comments on this.
;     */
;    hbox = gtk_hbox_new(FALSE, 0);
;    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, TRUE, 0);
;    gtk_widget_show(hbox);
;
;    button_add = gtk_button_new_with_label("Add List");
;    button_clear = gtk_button_new_with_label("Clear List");
;    button_hide_show = gtk_button_new_with_label("Hide/Show titles");
;
;    gtk_box_pack_start(GTK_BOX(hbox), button_add, TRUE, TRUE, 0);
;    gtk_box_pack_start(GTK_BOX(hbox), button_clear, TRUE, TRUE, 0);
;    gtk_box_pack_start(GTK_BOX(hbox), button_hide_show, TRUE, TRUE, 0);
;
;    /* Connect our callbacks to the three buttons */
;    gtk_signal_connect_object(GTK_OBJECT(button_add), "clicked",
;			       GTK_SIGNAL_FUNC(button_add_clicked),
;			       (gpointer) clist);
;    gtk_signal_connect_object(GTK_OBJECT(button_clear), "clicked",
;			       GTK_SIGNAL_FUNC(button_clear_clicked),
;			       (gpointer) clist);
;    gtk_signal_connect_object(GTK_OBJECT(button_hide_show), "clicked",
;			       GTK_SIGNAL_FUNC(button_hide_show_clicked),
;			       (gpointer) clist);
;
;    gtk_widget_show(button_add);
;    gtk_widget_show(button_clear);
;    gtk_widget_show(button_hide_show);
;
;    /* The interface is completely set up so we show the window and
;     * enter the gtk_main loop.
;     */
;    gtk_widget_show(window);
;    gtk_main();
;    
;    return(0);
;}
;/* example-end */

(defpackage "11.10-clist" (:use :excl :common-lisp))
(in-package "11.10-clist")

(ff:defun-foreign-callable button-add-clicked ((data gtk:gpointer))
  (let ((drink '(("Milk" "3 Oz")
		 ("Water" "6 l")
		 ("Carrots" "2")
		 ("Snakes" "55")))
	(rarray (ff:allocate-fobject '(:array (* :char) 2)
				     :foreign-static-gc)))

    (dolist (d drink)
      (dotimes (i 2)
	(setf (ff:fslot-value-typed '(:array (* :char)) nil rarray i)
	  (string-to-native (elt d i))))
      (gtk:gtk_clist_append data rarray)
      (dotimes (i 2)
	(aclfree (ff:fslot-value-typed '(:array (* :char)) nil rarray i))))))

(ff:defun-foreign-callable button-clear-clicked ((data gtk:gpointer))
  (gtk:gtk_clist_clear data))

(let ((flag 0))
  (defun button-hide-show-clicked-closure (data)
    (if* (eql flag 0)
       then (gtk:gtk_clist_column_titles_hide data)
	    (incf flag)
       else (gtk:gtk_clist_column_titles_show data)
	    (decf flag))))

(ff:defun-foreign-callable button-hide-show-clicked ((data gtk:gpointer))
  (button-hide-show-clicked-closure data))

(ff:defun-foreign-callable selection-made ((clist (* gtk:GtkWidget))
					   (row gtk:gint); bug11329
					   (column gtk:gint); bug11329
					   (event (* gtk:GdkEventButton))
					   (data gtk:gpointer))
  (declare (ignore event data))
  (let ((text (ff:allocate-fobject '(* gtk:gchar)
				   :foreign-static-gc)))
    (gtk:gtk_clist_get_text (gtk:GTK_CLIST clist) row column text)
    (format t "~
You selected row ~d.  More specifically you clicked in ~%~
column ~d and the text in this cell is ~s~2%"
	    row column (native-to-string
			(ff:fslot-value-typed '(* gtk:gchar) nil text)))))

(ff:defun-foreign-callable cb-gtk-main-quit ()
  (gtk:gtk-main-quit))

(defun clist ()
  (let ((window nil)
	(vbox nil)
	(hbox nil)
	(scrolled-window nil)
	(clist nil)
	(button-add nil)
	(button-clear nil)
	(button-hide-show nil)
	(titles
	 (let ((farray (ff:allocate-fobject '(:array (* :char) 2)
					    :foreign-static-gc)))
	   (setf (ff:fslot-value-typed '(:array (* :char)) nil farray 0)
	     (string-to-native "Ingredients"))
	   (setf (ff:fslot-value-typed '(:array (* :char)) nil farray 1)
	     (string-to-native "Amount"))
	   farray)))

    (gtk:gtk_init 0 0)

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:gtk_widget_set_usize (gtk:GTK_WIDGET window) 300 150)

    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "GtkCList Example")
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "destroy" 
			    (gtk:GTK_SIGNAL_FUNC
			     #+original (ff:get-entry-point "gtk_main_quit")
			     #-original (ff:register-foreign-callable
					 'cb-gtk-main-quit))
			    gtk:NULL)

    (setq vbox (gtk:gtk_vbox_new gtk:FALSE  5))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER vbox) 5)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) vbox)
    (gtk:gtk_widget_show vbox)

    (setq scrolled-window (gtk:gtk_scrolled_window_new gtk:NULL gtk:NULL))
    (gtk:gtk_scrolled_window_set_policy (gtk:GTK_SCROLLED_WINDOW
					 scrolled-window)
					gtk:GTK_POLICY_AUTOMATIC
					gtk:GTK_POLICY_ALWAYS)

    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) scrolled-window gtk:TRUE
			    gtk:TRUE 0)
    (gtk:gtk_widget_show scrolled-window)

    (setq clist (gtk:gtk_clist_new_with_titles 2 titles))

    (gtk:gtk_signal_connect (gtk:GTK_OBJECT clist) "select_row" 
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'selection-made))
			    gtk:NULL)

    (gtk:gtk_clist_set_shadow_type (gtk:GTK_CLIST clist)  gtk:GTK_SHADOW_OUT)

    (gtk:gtk_clist_set_column_width (gtk:GTK_CLIST clist) 0 150)

    (gtk:gtk_container_add (gtk:GTK_CONTAINER scrolled-window) clist)
    (gtk:gtk_widget_show clist)

    (setq hbox (gtk:gtk_hbox_new gtk:FALSE 0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) hbox gtk:FALSE gtk:TRUE 0)
    (gtk:gtk_widget_show hbox)

    (setq button-add (gtk:gtk_button_new_with_label "Add List"))
    (setq button-clear (gtk:gtk_button_new_with_label "Clear List"))
    (setq button-hide-show (gtk:gtk_button_new_with_label "Hide/Show titles"))

    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) button-add gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) button-clear gtk:TRUE gtk:TRUE
			    0)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) button-hide-show gtk:TRUE
			    gtk:TRUE 0)

    (gtk:gtk_signal_connect_object (gtk:GTK_OBJECT button-add) "clicked" 
				   (gtk:GTK_SIGNAL_FUNC
				    (ff:register-foreign-callable
				     'button-add-clicked)) 
				   clist)
    (gtk:gtk_signal_connect_object (gtk:GTK_OBJECT button-clear) "clicked" 
				   (gtk:GTK_SIGNAL_FUNC
				    (ff:register-foreign-callable
				     'button-clear-clicked))
				   clist)
    (gtk:gtk_signal_connect_object (gtk:GTK_OBJECT button-hide-show) "clicked" 
				   (gtk:GTK_SIGNAL_FUNC
				    (ff:register-foreign-callable
				     'button-hide-show-clicked))
				   clist)

    (gtk:gtk_widget_show button-add)
    (gtk:gtk_widget_show button-clear)
    (gtk:gtk_widget_show button-hide-show)

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
  (run-example "11.10-clist" #'clist))
