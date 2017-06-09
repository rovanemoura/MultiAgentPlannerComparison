;/* example-start selection setselection.c */
;
;#include <gtk/gtk.h>
;#include <time.h>
;
;/* Callback when the user toggles the selection */
;void selection_toggled( GtkWidget *widget,
;			 gint      *have_selection )
;{
;  if (GTK_TOGGLE_BUTTON(widget)->active)
;    {
;      *have_selection = gtk_selection_owner_set (widget,
;						  GDK_SELECTION_PRIMARY,
;						  GDK_CURRENT_TIME);
;      /* if claiming the selection failed, we return the button to
;	  the out state */
;      if (!*have_selection)
;	 gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(widget), FALSE);
;    }
;  else
;    {
;      if (*have_selection)
;	 {
;	   /* Before clearing the selection by setting the owner to NULL,
;	      we check if we are the actual owner */
;	   if (gdk_selection_owner_get (GDK_SELECTION_PRIMARY) == widget->window)
;	     gtk_selection_owner_set (NULL, GDK_SELECTION_PRIMARY,
;				      GDK_CURRENT_TIME);
;	   *have_selection = FALSE;
;	 }
;    }
;}
;
;/* Called when another application claims the selection */
;gint selection_clear( GtkWidget         *widget,
;		       GdkEventSelection *event,
;		       gint              *have_selection )
;{
;  *have_selection = FALSE;
;  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(widget), FALSE);
;
;  return TRUE;
;}
;
;/* Supplies the current time as the selection. */
;void selection_handle( GtkWidget        *widget, 
;			GtkSelectionData *selection_data,
;			guint             info,
;			guint             time_stamp,
;			gpointer          data )
;{
;  gchar *timestr;
;  time_t current_time;
;
;  current_time = time(NULL);
;  timestr = asctime (localtime(&current_time)); 
;  /* When we return a single string, it should not be null terminated.
;     That will be done for us */
;
;  gtk_selection_data_set (selection_data, GDK_SELECTION_TYPE_STRING,
;			   8, timestr, strlen(timestr));
;}
;
;int main( int   argc,
;	   char *argv[] )
;{
;  GtkWidget *window;
;  GtkWidget *selection_button;
;
;  static int have_selection = FALSE;
;  
;  gtk_init (&argc, &argv);
;
;  /* Create the toplevel window */
;
;  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;  gtk_window_set_title (GTK_WINDOW (window), "Event Box");
;  gtk_container_set_border_width (GTK_CONTAINER (window), 10);
;
;  gtk_signal_connect (GTK_OBJECT (window), "destroy",
;		       GTK_SIGNAL_FUNC (gtk_exit), NULL);
;
;  /* Create a toggle button to act as the selection */
;
;  selection_button = gtk_toggle_button_new_with_label ("Claim Selection");
;  gtk_container_add (GTK_CONTAINER (window), selection_button);
;  gtk_widget_show (selection_button);
;
;  gtk_signal_connect (GTK_OBJECT(selection_button), "toggled",
;		       GTK_SIGNAL_FUNC (selection_toggled), &have_selection);
;  gtk_signal_connect (GTK_OBJECT(selection_button), "selection_clear_event",
;		       GTK_SIGNAL_FUNC (selection_clear), &have_selection);
;
;  gtk_selection_add_target (selection_button,
;			     GDK_SELECTION_PRIMARY,
;			     GDK_SELECTION_TYPE_STRING,
;			     1);
;  gtk_signal_connect (GTK_OBJECT(selection_button), "selection_get",
;		       GTK_SIGNAL_FUNC (selection_handle), &have_selection);
;
;  gtk_widget_show (selection_button);
;  gtk_widget_show (window);
;  
;  gtk_main ();
;  
;  return 0;
;}
;/* example-end */

(defpackage "20.03-setselection" (:use :excl :common-lisp))
(in-package "20.03-setselection")

(ff:defun-foreign-callable selection-toggled ((widget (* gtk:GtkWidget))
					      (have-selection (* gtk:gint)))
  (if* (not (eql gtk:NULL (ff:fslot-value-typed 'gtk:GtkToggleButton nil
						(gtk:GTK_TOGGLE_BUTTON widget)
						'gtk::active)))
     then (setf (ff:fslot-value-typed 'gtk:gint nil have-selection)
	    (gtk:gtk_selection_owner_set widget
					 gtk:GDK_SELECTION_PRIMARY
					 gtk:GDK_CURRENT_TIME))
	  (when (eql gtk:NULL (ff:fslot-value-typed 'gtk:gint nil
						    have-selection))
	    (gtk:gtk_toggle_button_set_active (gtk:GTK_TOGGLE_BUTTON widget)
					      gtk:FALSE))
     else (unless (eql gtk:NULL
		       (ff:fslot-value-typed 'gtk:gint nil have-selection))
	    (when (eql (gtk:gdk_selection_owner_get gtk:GDK_SELECTION_PRIMARY)
		       (ff:fslot-value-typed 'gtk:GtkWidget nil widget
					     'gtk::window))
	      (gtk:gtk_selection_owner_set gtk:NULL gtk:GDK_SELECTION_PRIMARY
					   gtk:GDK_CURRENT_TIME)
	      (setf (ff:fslot-value-typed 'gtk:gint nil have-selection)
		gtk:FALSE)))))

(ff:defun-foreign-callable selection-clear ((widget (* gtk:GtkWidget))
					    (event (* gtk:GdkEventSelection))
					    (have-selection (* gtk:gint)))
  (declare (ignore event))
  (setf (ff:fslot-value-typed 'gtk:gint nil have-selection) gtk:FALSE)
  (gtk:gtk_toggle_button_set_active (gtk:GTK_TOGGLE_BUTTON widget) gtk:FALSE)

  gtk:TRUE)

(ff:defun-foreign-callable selection-handle ((widget (* gtk:GtkWidget))
					     (selection-data
					      (* gtk:GtkSelectionData))
					     (info gtk:guint)
					     (time-stamp gtk:guint)
					     (data gtk:gpointer))
  (declare (ignore widget info time-stamp data))
  
  (with-native-string (timestr
		       (format nil "~v:@/locale-format-time/"
			       :en_US (get-universal-time))
		       :native-length-var timestr-len)
    (gtk:gtk_selection_data_set selection-data gtk:GDK_SELECTION_TYPE_STRING
				8
				timestr timestr-len)))

(let ((have-selection (ff:allocate-fobject 'gtk:gint
					   :foreign-static-gc)))
  (setf (ff:fslot-value-typed 'gtk:gint nil have-selection) gtk:FALSE)
  (defun setselection ()
    (let ((window nil)
	  (selection-button nil))

      (gtk:gtk_init 0 0)

      (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
      (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "Event Box")
      (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 10)

      (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "destroy"
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:get-entry-point "gtk_exit"))
			      gtk:NULL)

      (setq selection-button (gtk:gtk_toggle_button_new_with_label
			      "Claim Selection"))
      (gtk:gtk_container_add (gtk:GTK_CONTAINER window) selection-button)
      (gtk:gtk_widget_show selection-button)

      (gtk:gtk_signal_connect (gtk:GTK_OBJECT selection-button) "toggled" 
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'selection-toggled))
			      have-selection)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT selection-button)
			      "selection_clear_event" 
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'selection-clear))
			      have-selection)

      (gtk:gtk_selection_add_target selection-button 
				    gtk:GDK_SELECTION_PRIMARY 
				    gtk:GDK_SELECTION_TYPE_STRING 
				    1)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT selection-button)
			      "selection_get" 
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'selection-handle))
			      have-selection)

      (gtk:gtk_widget_show selection-button)
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
  (run-example "20.03-setselection" #'setselection))
