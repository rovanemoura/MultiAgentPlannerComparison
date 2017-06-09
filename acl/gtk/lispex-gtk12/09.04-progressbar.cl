;/* example-start progressbar progressbar.c */
;
;#include <gtk/gtk.h>
;
;typedef struct _ProgressData {
;    GtkWidget *window;
;    GtkWidget *pbar;
;    int timer;
;} ProgressData;
;
;/* Update the value of the progress bar so that we get
; * some movement */
;gint progress_timeout( gpointer data )
;{
;    gfloat new_val;
;    GtkAdjustment *adj;
;
;    /* Calculate the value of the progress bar using the
;     * value range set in the adjustment object */
;
;    new_val = gtk_progress_get_value( GTK_PROGRESS(data) ) + 1;
;
;    adj = GTK_PROGRESS (data)->adjustment;
;    if (new_val > adj->upper)
;      new_val = adj->lower;
;
;    /* Set the new value */
;    gtk_progress_set_value (GTK_PROGRESS (data), new_val);
;
;    /* As this is a timeout function, return TRUE so that it
;     * continues to get called */
;    return(TRUE);
;} 
;
;/* Callback that toggles the text display within the progress
; * bar trough */
;void toggle_show_text( GtkWidget    *widget,
;			ProgressData *pdata )
;{
;    gtk_progress_set_show_text (GTK_PROGRESS (pdata->pbar),
;				 GTK_TOGGLE_BUTTON (widget)->active);
;}
;
;/* Callback that toggles the activity mode of the progress
; * bar */
;void toggle_activity_mode( GtkWidget    *widget,
;			    ProgressData *pdata )
;{
;    gtk_progress_set_activity_mode (GTK_PROGRESS (pdata->pbar),
;				     GTK_TOGGLE_BUTTON (widget)->active);
;}
;
;/* Callback that toggles the continuous mode of the progress
; * bar */
;void set_continuous_mode( GtkWidget    *widget,
;			   ProgressData *pdata )
;{
;    gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR (pdata->pbar),
;				     GTK_PROGRESS_CONTINUOUS);
;}
;
;/* Callback that toggles the discrete mode of the progress
; * bar */
;void set_discrete_mode( GtkWidget    *widget,
;			 ProgressData *pdata )
;{
;    gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR (pdata->pbar),
;				     GTK_PROGRESS_DISCRETE);
;}
; 
;/* Clean up allocated memory and remove the timer */
;void destroy_progress( GtkWidget     *widget,
;			ProgressData *pdata)
;{
;    gtk_timeout_remove (pdata->timer);
;    pdata->timer = 0;
;    pdata->window = NULL;
;    g_free(pdata);
;    gtk_main_quit();
;}
;
;int main( int   argc,
;	   char *argv[])
;{
;    ProgressData *pdata;
;    GtkWidget *align;
;    GtkWidget *separator;
;    GtkWidget *table;
;    GtkAdjustment *adj;
;    GtkWidget *button;
;    GtkWidget *check;
;    GtkWidget *vbox;
;
;    gtk_init (&argc, &argv);
;
;    /* Allocate memory for the data that is passwd to the callbacks */
;    pdata = g_malloc( sizeof(ProgressData) );
;  
;    pdata->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;    gtk_window_set_policy (GTK_WINDOW (pdata->window), FALSE, FALSE, TRUE);
;
;    gtk_signal_connect (GTK_OBJECT (pdata->window), "destroy",
;			 GTK_SIGNAL_FUNC (destroy_progress),
;			 pdata);
;    gtk_window_set_title (GTK_WINDOW (pdata->window), "GtkProgressBar");
;    gtk_container_set_border_width (GTK_CONTAINER (pdata->window), 0);
;
;    vbox = gtk_vbox_new (FALSE, 5);
;    gtk_container_set_border_width (GTK_CONTAINER (vbox), 10);
;    gtk_container_add (GTK_CONTAINER (pdata->window), vbox);
;    gtk_widget_show(vbox);
;  
;    /* Create a centering alignment object */
;    align = gtk_alignment_new (0.5, 0.5, 0, 0);
;    gtk_box_pack_start (GTK_BOX (vbox), align, FALSE, FALSE, 5);
;    gtk_widget_show(align);
;
;    /* Create a Adjusment object to hold the range of the
;     * progress bar */
;    adj = (GtkAdjustment *) gtk_adjustment_new (0, 1, 150, 0, 0, 0);
;
;    /* Create the GtkProgressBar using the adjustment */
;    pdata->pbar = gtk_progress_bar_new_with_adjustment (adj);
;
;    /* Set the format of the string that can be displayed in the
;     * trough of the progress bar:
;     * %p - percentage
;     * %v - value
;     * %l - lower range value
;     * %u - upper range value */
;    gtk_progress_set_format_string (GTK_PROGRESS (pdata->pbar),
;				     "%v from [%l-%u] (=%p%%)");
;    gtk_container_add (GTK_CONTAINER (align), pdata->pbar);
;    gtk_widget_show(pdata->pbar);
;
;    /* Add a timer callback to update the value of the progress bar */
;    pdata->timer = gtk_timeout_add (100, progress_timeout, pdata->pbar);
;
;    separator = gtk_hseparator_new ();
;    gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, FALSE, 0);
;    gtk_widget_show(separator);
;
;    /* rows, columns, homogeneous */
;    table = gtk_table_new (2, 3, FALSE);
;    gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, TRUE, 0);
;    gtk_widget_show(table);
;
;    /* Add a check button to select displaying of the trough text */
;    check = gtk_check_button_new_with_label ("Show text");
;    gtk_table_attach (GTK_TABLE (table), check, 0, 1, 0, 1,
;		       GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
;		       5, 5);
;    gtk_signal_connect (GTK_OBJECT (check), "clicked",
;			 GTK_SIGNAL_FUNC (toggle_show_text),
;			 pdata);
;    gtk_widget_show(check);
;
;    /* Add a check button to toggle activity mode */
;    check = gtk_check_button_new_with_label ("Activity mode");
;    gtk_table_attach (GTK_TABLE (table), check, 0, 1, 1, 2,
;		       GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
;		       5, 5);
;    gtk_signal_connect (GTK_OBJECT (check), "clicked",
;			 GTK_SIGNAL_FUNC (toggle_activity_mode),
;			 pdata);
;    gtk_widget_show(check);
;
;    separator = gtk_vseparator_new ();
;    gtk_table_attach (GTK_TABLE (table), separator, 1, 2, 0, 2,
;		       GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
;		       5, 5);
;    gtk_widget_show(separator);
;
;    /* Add a radio button to select continuous display mode */
;    button = gtk_radio_button_new_with_label (NULL, "Continuous");
;    gtk_table_attach (GTK_TABLE (table), button, 2, 3, 0, 1,
;		       GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
;		       5, 5);
;    gtk_signal_connect (GTK_OBJECT (button), "clicked",
;			 GTK_SIGNAL_FUNC (set_continuous_mode),
;			 pdata);
;    gtk_widget_show (button);
;
;    /* Add a radio button to select discrete display mode */
;    button = gtk_radio_button_new_with_label(
;		gtk_radio_button_group (GTK_RADIO_BUTTON (button)),
;		"Discrete");
;    gtk_table_attach (GTK_TABLE (table), button, 2, 3, 1, 2,
;		       GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
;		       5, 5);
;    gtk_signal_connect (GTK_OBJECT (button), "clicked",
;			 GTK_SIGNAL_FUNC (set_discrete_mode),
;			 pdata);
;    gtk_widget_show (button);
;
;    separator = gtk_hseparator_new ();
;    gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, FALSE, 0);
;    gtk_widget_show(separator);
;
;    /* Add a button to exit the program */
;    button = gtk_button_new_with_label ("close");
;    gtk_signal_connect_object (GTK_OBJECT (button), "clicked",
;				(GtkSignalFunc) gtk_widget_destroy,
;				GTK_OBJECT (pdata->window));
;    gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, FALSE, 0);
;
;    /* This makes it so the button is the default. */
;    GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
;
;    /* This grabs this button to be the default button. Simply hitting
;     * the "Enter" key will cause this button to activate. */
;    gtk_widget_grab_default (button);
;    gtk_widget_show(button);
;
;    gtk_widget_show (pdata->window);
;
;    gtk_main ();
;    
;    return(0);
;}
;/* example-end */

(defpackage "09.04-progressbar" (:use :excl :common-lisp))
(in-package "09.04-progressbar")

(defstruct progress-data
  window
  pbar
  timer)

(ff:defun-foreign-callable progress-timeout ((data gtk:gpointer))
  (let ((new-val (1+ (gtk:gtk_progress_get_value (gtk:GTK_PROGRESS data))))
	(adj (ff:fslot-value-typed 'gtk:GtkProgress nil (gtk:GTK_PROGRESS data)
				   'gtk::adjustment)))
    (when (> new-val (ff:fslot-value-typed 'gtk:GtkAdjustment nil adj
					   'gtk::upper))
      (setq new-val (ff:fslot-value-typed 'gtk:GtkAdjustment nil adj
					  'gtk::lower)))

    (gtk:gtk_progress_set_value (gtk:GTK_PROGRESS data) new-val)

    gtk:TRUE))

(ff:defun-foreign-callable toggle-show-text ((widget (* gtk:GtkWidget)) pdata)
  (gtk:gtk_progress_set_show_text
   (gtk:GTK_PROGRESS (progress-data-pbar (ff:lisp-value pdata)))
   (ff:fslot-value-typed 'gtk:GtkToggleButton nil
			 (gtk:GTK_TOGGLE_BUTTON widget)
			 'gtk::active)))

(ff:defun-foreign-callable toggle-activity-mode ((widget (* gtk:GtkWidget))
						 pdata)
  (gtk:gtk_progress_set_activity_mode
   (gtk:GTK_PROGRESS (progress-data-pbar (ff:lisp-value pdata)))
   (ff:fslot-value-typed 'gtk:GtkToggleButton nil
			 (gtk:GTK_TOGGLE_BUTTON widget)
			 'gtk::active)))


(ff:defun-foreign-callable set-continuous-mode ((widget (* gtk:GtkWidget))
						pdata)
  (declare (ignore widget))
  (gtk:gtk_progress_bar_set_bar_style
   (gtk:GTK_PROGRESS_BAR (progress-data-pbar (ff:lisp-value pdata)))
   gtk:GTK_PROGRESS_CONTINUOUS))

(ff:defun-foreign-callable set-discrete-mode ((widget (* gtk:GtkWidget))
					      pdata)
  (declare (ignore widget))
  (gtk:gtk_progress_bar_set_bar_style
   (gtk:GTK_PROGRESS_BAR (progress-data-pbar (ff:lisp-value pdata)))
   gtk:GTK_PROGRESS_DISCRETE))

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
	(adj nil)
	(button nil)
	(check nil)
	(vbox nil))

    (gtk:gtk_init 0 0)

    (setq pdata (ff:register-lisp-value
		 (make-progress-data
		  :window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))))
    

    (gtk:gtk_window_set_policy (gtk:GTK_WINDOW (progress-data-window
						(ff:lisp-value pdata)))
			       gtk:FALSE gtk:FALSE gtk:TRUE)

    (gtk:gtk_signal_connect
     (gtk:GTK_OBJECT (progress-data-window (ff:lisp-value pdata)))
     "destroy"
     (gtk:GTK_SIGNAL_FUNC (ff:register-foreign-callable 'destroy-progress))
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

    (setq adj (gtk:gtk_adjustment_new 0.0 1.0 150.0 0.0 0.0 0.0))

    (setf (progress-data-pbar (ff:lisp-value pdata))
      (gtk:gtk_progress_bar_new_with_adjustment adj))
    (gtk:gtk_progress_set_format_string
     (gtk:GTK_PROGRESS (progress-data-pbar (ff:lisp-value pdata)))
     "%v from [%l-%u] (=%p%%)")
    (gtk:gtk_container_add (gtk:GTK_CONTAINER align)
			   (progress-data-pbar (ff:lisp-value pdata)))
    (gtk:gtk_widget_show (progress-data-pbar (ff:lisp-value pdata)))

    (setf (progress-data-timer (ff:lisp-value pdata))
      (gtk:gtk_timeout_add 100
			   (ff:register-foreign-callable 'progress-timeout)
			   (progress-data-pbar (ff:lisp-value pdata))))
    (setf (gtk:gtk-timeout) 100)	; See gtk/eh.cl

    (setq separator (gtk:gtk_hseparator_new))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) separator gtk:FALSE gtk:FALSE 0)
    (gtk:gtk_widget_show separator)

    (setq table (gtk:gtk_table_new 2 3 gtk:FALSE))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) table gtk:FALSE gtk:TRUE 0)
    (gtk:gtk_widget_show table)

    (setq check (gtk:gtk_check_button_new_with_label "Show text"))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) check 0 1 0 1
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  5 5)
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT check) "clicked"
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'toggle-show-text))
			    pdata)
    (gtk:gtk_widget_show check)

    (setq check (gtk:gtk_check_button_new_with_label "Activity mode"))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) check 0 1 1 2
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  5 5)
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT check) "clicked"
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'toggle-activity-mode))
			    pdata)
    (gtk:gtk_widget_show check)

    (setq separator (gtk:gtk_vseparator_new))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) separator 1 2 0 2
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  5 5)
    (gtk:gtk_widget_show separator)

    (setq button (gtk:gtk_radio_button_new_with_label gtk:NULL "Continuous"))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) button 2 3 0 1
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  5 5)
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT button) "clicked"
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'set-continuous-mode))
			    pdata)
    (gtk:gtk_widget_show button)

    (setq button (gtk:gtk_radio_button_new_with_label
		  (gtk:gtk_radio_button_group (gtk:GTK_RADIO_BUTTON button))
		  "Discrete"))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) button 2 3 1 2
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  (logior gtk:GTK_EXPAND gtk:GTK_FILL)
			  5 5)
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT button) "clicked"
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'set-discrete-mode))
			    pdata)
    (gtk:gtk_widget_show button)

    (setq separator (gtk:gtk_hseparator_new))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) separator gtk:FALSE gtk:FALSE 0)
    (gtk:gtk_widget_show separator)

    (setq button (gtk:gtk_button_new_with_label "close"))
    (gtk:gtk_signal_connect_object (gtk:GTK_OBJECT button) "clicked"
				   (ff:get-entry-point "gtk_widget_destroy")
				   (gtk:GTK_OBJECT
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
