;
;#include <gtk/gtk.h>
;
;typedef struct _ProgressData {
;  GtkWidget *window;
;  GtkWidget *pbar;
;  int timer;
;  gboolean activity_mode;
;} ProgressData;
;
;/* Update the value of the progress bar so that we get
; * some movement */
;gint progress_timeout( gpointer data )
;{
;  ProgressData *pdata = (ProgressData *)data;
;  gdouble new_val;
;  
;  if (pdata->activity_mode) 
;    gtk_progress_bar_pulse (GTK_PROGRESS_BAR (pdata->pbar));
;  else 
;    {
;      /* Calculate the value of the progress bar using the
;	* value range set in the adjustment object */
;      
;      new_val = gtk_progress_bar_get_fraction (GTK_PROGRESS_BAR (pdata->pbar)) + 0.01;
;      
;      if (new_val > 1.0)
;	 new_val = 0.0;
;      
;      /* Set the new value */
;      gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR (pdata->pbar), new_val);
;    }
;  
;  /* As this is a timeout function, return TRUE so that it
;   * continues to get called */
;  return TRUE;
;} 
;
;/* Callback that toggles the text display within the progress bar trough */
;void toggle_show_text( GtkWidget    *widget,
;			ProgressData *pdata )
;{
;  const gchar *text;
;  
;  text = gtk_progress_bar_get_text (GTK_PROGRESS_BAR (pdata->pbar));
;  if (text && *text)
;    gtk_progress_bar_set_text (GTK_PROGRESS_BAR (pdata->pbar), "");
;  else 
;    gtk_progress_bar_set_text (GTK_PROGRESS_BAR (pdata->pbar), "some text");
;}
;
;/* Callback that toggles the activity mode of the progress bar */
;void toggle_activity_mode( GtkWidget    *widget,
;			    ProgressData *pdata )
;{
;  pdata->activity_mode = !pdata->activity_mode;
;  if (pdata->activity_mode) 
;      gtk_progress_bar_pulse (GTK_PROGRESS_BAR (pdata->pbar));
;  else
;      gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR (pdata->pbar), 0.0);
;}
;
; 
;/* Callback that toggles the orientation of the progress bar */
;void toggle_orientation( GtkWidget    *widget,
;			  ProgressData *pdata )
;{
;  switch (gtk_progress_bar_get_orientation (GTK_PROGRESS_BAR (pdata->pbar))) {
;  case GTK_PROGRESS_LEFT_TO_RIGHT:
;    gtk_progress_bar_set_orientation (GTK_PROGRESS_BAR (pdata->pbar), 
;				       GTK_PROGRESS_RIGHT_TO_LEFT);
;    break;
;  case GTK_PROGRESS_RIGHT_TO_LEFT:
;    gtk_progress_bar_set_orientation (GTK_PROGRESS_BAR (pdata->pbar), 
;				       GTK_PROGRESS_LEFT_TO_RIGHT);
;    break;
;  default:
;    // do nothing	
;  }
;}
;
; 
;/* Clean up allocated memory and remove the timer */
;void destroy_progress( GtkWidget     *widget,
;			ProgressData *pdata)
;{
;    gtk_timeout_remove (pdata->timer);
;    pdata->timer = 0;
;    pdata->window = NULL;
;    g_free (pdata);
;    gtk_main_quit ();
;}
;
;int main( int   argc,
;	   char *argv[])
;{
;    ProgressData *pdata;
;    GtkWidget *align;
;    GtkWidget *separator;
;    GtkWidget *table;
;    GtkWidget *button;
;    GtkWidget *check;
;    GtkWidget *vbox;
;
;    gtk_init (&argc, &argv);
;
;    /* Allocate memory for the data that is passed to the callbacks */
;    pdata = g_malloc (sizeof (ProgressData));
;  
;    pdata->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;    gtk_window_set_resizable (GTK_WINDOW (pdata->window), TRUE);
;
;    g_signal_connect (G_OBJECT (pdata->window), "destroy",
;		       G_CALLBACK (destroy_progress),
;		       pdata);
;    gtk_window_set_title (GTK_WINDOW (pdata->window), "GtkProgressBar");
;    gtk_container_set_border_width (GTK_CONTAINER (pdata->window), 0);
;
;    vbox = gtk_vbox_new (FALSE, 5);
;    gtk_container_set_border_width (GTK_CONTAINER (vbox), 10);
;    gtk_container_add (GTK_CONTAINER (pdata->window), vbox);
;    gtk_widget_show (vbox);
;  
;    /* Create a centering alignment object */
;    align = gtk_alignment_new (0.5, 0.5, 0, 0);
;    gtk_box_pack_start (GTK_BOX (vbox), align, FALSE, FALSE, 5);
;    gtk_widget_show (align);
;
;    /* Create the GtkProgressBar */
;    pdata->pbar = gtk_progress_bar_new ();
;
;    gtk_container_add (GTK_CONTAINER (align), pdata->pbar);
;    gtk_widget_show (pdata->pbar);
;
;    /* Add a timer callback to update the value of the progress bar */
;    pdata->timer = gtk_timeout_add (100, progress_timeout, pdata);
;
;    separator = gtk_hseparator_new ();
;    gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, FALSE, 0);
;    gtk_widget_show (separator);
;
;    /* rows, columns, homogeneous */
;    table = gtk_table_new (2, 2, FALSE);
;    gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, TRUE, 0);
;    gtk_widget_show (table);
;
;    /* Add a check button to select displaying of the trough text */
;    check = gtk_check_button_new_with_label ("Show text");
;    gtk_table_attach (GTK_TABLE (table), check, 0, 1, 0, 1,
;		       GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
;		       5, 5);
;    g_signal_connect (G_OBJECT (check), "clicked",
;		       G_CALLBACK (toggle_show_text),
;		       pdata);
;    gtk_widget_show (check);
;
;    /* Add a check button to toggle activity mode */
;    check = gtk_check_button_new_with_label ("Activity mode");
;    gtk_table_attach (GTK_TABLE (table), check, 0, 1, 1, 2,
;		       GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
;		       5, 5);
;    g_signal_connect (G_OBJECT (check), "clicked",
;		       G_CALLBACK (toggle_activity_mode),
;		       pdata);
;    gtk_widget_show (check);
;
;    /* Add a check button to toggle orientation */
;    check = gtk_check_button_new_with_label ("Right to Left");
;    gtk_table_attach (GTK_TABLE (table), check, 0, 1, 2, 3,
;		       GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
;		       5, 5);
;    g_signal_connect (G_OBJECT (check), "clicked",
;		       G_CALLBACK (toggle_orientation),
;		       pdata);
;    gtk_widget_show (check);
;
;    /* Add a button to exit the program */
;    button = gtk_button_new_with_label ("close");
;    g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			       G_CALLBACK (gtk_widget_destroy),
;			       pdata->window);
;    gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, FALSE, 0);
;
;    /* This makes it so the button is the default. */
;    GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
;
;    /* This grabs this button to be the default button. Simply hitting
;     * the "Enter" key will cause this button to activate. */
;    gtk_widget_grab_default (button);
;    gtk_widget_show (button);
;
;    gtk_widget_show (pdata->window);
;
;    gtk_main ();
;    
;    return 0;
;}

(defpackage "09.04-progressbar" (:use :excl :common-lisp))
(in-package "09.04-progressbar")

(defstruct progress-data
  window
  pbar
  timer
  activity-mode)

(ff:defun-foreign-callable progress-timeout ((pdata gtk:gpointer))
  (let ((new-val nil))
    (if* (progress-data-activity-mode (ff:lisp-value pdata))
       then (gtk:gtk_progress_bar_pulse (gtk:GTK_PROGRESS_BAR
					 (progress-data-pbar
					  (ff:lisp-value pdata))))
       else (setq new-val (+ (gtk:gtk_progress_bar_get_fraction
			      (gtk:GTK_PROGRESS_BAR
			       (progress-data-pbar
				(ff:lisp-value pdata))))
			     0.01))
	    (when (> new-val 1.0)
	      (setq new-val 0.0d0))
	    (gtk:gtk_progress_bar_set_fraction
	     (gtk:GTK_PROGRESS_BAR
	      (progress-data-pbar
	       (ff:lisp-value pdata)))
	     new-val)))
  gtk:TRUE)

(ff:defun-foreign-callable toggle-show-text ((widget (* gtk:GtkWidget)) pdata)
  (declare (ignore widget))
  (let ((text (gtk:gtk_progress_bar_get_text
	       (gtk:GTK_PROGRESS_BAR
		(progress-data-pbar (ff:lisp-value pdata))))))
    (if* (and (not (eql gtk:NULL text))
	      (not (string-equal "" (native-to-string text))))
       then (gtk:gtk_progress_bar_set_text
	     (gtk:GTK_PROGRESS_BAR
	      (progress-data-pbar (ff:lisp-value pdata)))
	     "")
       else (gtk:gtk_progress_bar_set_text
	     (gtk:GTK_PROGRESS_BAR
	      (progress-data-pbar (ff:lisp-value pdata)))
	     "some text"))))

(ff:defun-foreign-callable toggle-activity-mode ((widget (* gtk:GtkWidget))
						 pdata)
  (declare (ignore widget))
  (setf (progress-data-activity-mode (ff:lisp-value pdata))
    (not (progress-data-activity-mode (ff:lisp-value pdata))))
  (if* (progress-data-activity-mode (ff:lisp-value pdata))
     then (gtk:gtk_progress_bar_pulse
	   (gtk:GTK_PROGRESS_BAR
	    (progress-data-pbar (ff:lisp-value pdata))))
     else (gtk:gtk_progress_bar_set_fraction
	   (gtk:GTK_PROGRESS_BAR
	    (progress-data-pbar (ff:lisp-value pdata)))
	   0.0d0)))

(ff:defun-foreign-callable toggle-orientation ((widget (* gtk:GtkWidget))
					       pdata)
  (declare (ignore widget))

  (case (gtk:gtk_progress_bar_get_orientation
	 (gtk:GTK_PROGRESS_BAR
	  (progress-data-pbar (ff:lisp-value pdata))))
    (#.gtk:GTK_PROGRESS_LEFT_TO_RIGHT
     (gtk:gtk_progress_bar_set_orientation
      (gtk:GTK_PROGRESS_BAR
       (progress-data-pbar (ff:lisp-value pdata)))
      gtk:GTK_PROGRESS_RIGHT_TO_LEFT))
    (#.gtk:GTK_PROGRESS_RIGHT_TO_LEFT
     (gtk:gtk_progress_bar_set_orientation
      (gtk:GTK_PROGRESS_BAR
       (progress-data-pbar (ff:lisp-value pdata)))
      gtk:GTK_PROGRESS_LEFT_TO_RIGHT))
    (otherwise nil)))

(ff:defun-foreign-callable destroy-progress ((widget (* gtk:GtkWidget))
					     pdata)
  (declare (ignore widget))
  (gtk:gtk_timeout_remove (progress-data-timer (ff:lisp-value pdata)))
  (setf (gtk:gtk-timeout) nil)		; See gtk/eh.cl
  (setf (progress-data-timer (ff:lisp-value pdata)) 0)
  (setf (progress-data-window (ff:lisp-value pdata)) gtk:NULL)
  (ff:unregister-lisp-value pdata)
  #+original (gtk:gtk_main_quit)
  #-original (gtk:gtk-main-quit))

(defun progressbar ()
  (let ((pdata nil)
	(align nil)
	(separator nil)
	(table nil)
	(button nil)
	(check nil)
	(vbox nil))

    (gtk:gtk_init 0 0)

    (setq pdata (ff:register-lisp-value
		 (make-progress-data
		  :window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))))
    

    (gtk:gtk_window_set_resizable
     (gtk:GTK_WINDOW (progress-data-window (ff:lisp-value pdata)))
     gtk:TRUE)

    (gtk:g_signal_connect
     (gtk:G_OBJECT (progress-data-window (ff:lisp-value pdata)))
     "destroy"
     (gtk:G_CALLBACK (ff:register-foreign-callable 'destroy-progress))
     pdata)
    (gtk:gtk_window_set_title
     (gtk:GTK_WINDOW (progress-data-window (ff:lisp-value pdata)))
     "GtkProgressBar")
    (gtk:gtk_container_set_border_width
     (gtk:GTK_CONTAINER (progress-data-window (ff:lisp-value pdata)))
     0)
    
    (setq vbox (gtk:gtk_vbox_new gtk:FALSE 5))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER vbox) 10)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER
			    (progress-data-window (ff:lisp-value pdata)))
			   vbox)
    (gtk:gtk_widget_show vbox)

    (setq align (gtk:gtk_alignment_new 0.5 0.5 0.0 0.0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) align gtk:FALSE gtk:FALSE 5)
    (gtk:gtk_widget_show align)

    (setf (progress-data-pbar (ff:lisp-value pdata))
      (gtk:gtk_progress_bar_new))

    (gtk:gtk_container_add (gtk:GTK_CONTAINER align)
			   (progress-data-pbar (ff:lisp-value pdata)))
    (gtk:gtk_widget_show (progress-data-pbar (ff:lisp-value pdata)))

    (setf (progress-data-timer (ff:lisp-value pdata))
      (gtk:gtk_timeout_add 100
			   (ff:register-foreign-callable 'progress-timeout)
			   pdata))
    (setf (gtk:gtk-timeout) 100)	; See gtk/eh.cl

    (setq separator (gtk:gtk_hseparator_new))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) separator gtk:FALSE gtk:FALSE 0)
    (gtk:gtk_widget_show separator)

    (setq table (gtk:gtk_table_new 2 2 gtk:FALSE))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) table gtk:FALSE gtk:TRUE 0)
    (gtk:gtk_widget_show table)

    (setq check (gtk:gtk_check_button_new_with_label "Show text"))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) check 0 1 0 1
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  5 5)
    (gtk:g_signal_connect (gtk:G_OBJECT check) "clicked"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'toggle-show-text))
			  pdata)
    (gtk:gtk_widget_show check)

    (setq check (gtk:gtk_check_button_new_with_label "Activity mode"))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) check 0 1 1 2
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  5 5)
    (gtk:g_signal_connect (gtk:G_OBJECT check) "clicked"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable
			    'toggle-activity-mode))
			  pdata)
    (gtk:gtk_widget_show check)

    (setq check (gtk:gtk_check_button_new_with_label "Right to Left"))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) check 0 1 2 3
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  5 5)

    (gtk:g_signal_connect (gtk:G_OBJECT check) "clicked"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'toggle-orientation))
			  pdata)
    (gtk:gtk_widget_show check)

    (setq button (gtk:gtk_button_new_with_label "close"))
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT button) "clicked"
				  (ff:get-entry-point "gtk_widget_destroy")
				  (gtk:G_CALLBACK
				   (progress-data-window
				    (ff:lisp-value pdata))))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) button gtk:FALSE gtk:FALSE 0)

    
    (gtk:GTK_WIDGET_SET_FLAGS button gtk:GTK_CAN_DEFAULT)

    (gtk:gtk_widget_grab_default button)
    (gtk:gtk_widget_show button)

    (gtk:gtk_widget_show (progress-data-window (ff:lisp-value pdata)))

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
  (run-example "09.04-progressbar" #'progressbar))
