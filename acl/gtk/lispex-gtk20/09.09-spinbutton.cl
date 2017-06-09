;
;#include <stdio.h>
;#include <gtk/gtk.h>
;
;static GtkWidget *spinner1;
;
;void toggle_snap( GtkWidget     *widget,
;		   GtkSpinButton *spin )
;{
;  gtk_spin_button_set_snap_to_ticks (spin, GTK_TOGGLE_BUTTON (widget)->active);
;}
;
;void toggle_numeric( GtkWidget *widget,
;		      GtkSpinButton *spin )
;{
;  gtk_spin_button_set_numeric (spin, GTK_TOGGLE_BUTTON (widget)->active);
;}
;
;void change_digits( GtkWidget *widget,
;		     GtkSpinButton *spin )
;{
;  gtk_spin_button_set_digits (GTK_SPIN_BUTTON (spinner1),
;			       gtk_spin_button_get_value_as_int (spin));
;}
;
;void get_value( GtkWidget *widget,
;		 gpointer data )
;{
;  gchar buf[32];
;  GtkLabel *label;
;  GtkSpinButton *spin;
;
;  spin = GTK_SPIN_BUTTON (spinner1);
;  label = GTK_LABEL (g_object_get_data (G_OBJECT (widget), "user_data"));
;  if (GPOINTER_TO_INT (data) == 1)
;    sprintf (buf, "%d", gtk_spin_button_get_value_as_int (spin));
;  else
;    sprintf (buf, "%0.*f", spin->digits,
;	      gtk_spin_button_get_value (spin));
;  gtk_label_set_text (label, buf);
;}
;
;
;int main( int   argc,
;	   char *argv[] )
;{
;  GtkWidget *window;
;  GtkWidget *frame;
;  GtkWidget *hbox;
;  GtkWidget *main_vbox;
;  GtkWidget *vbox;
;  GtkWidget *vbox2;
;  GtkWidget *spinner2;
;  GtkWidget *spinner;
;  GtkWidget *button;
;  GtkWidget *label;
;  GtkWidget *val_label;
;  GtkAdjustment *adj;
;
;  /* Initialise GTK */
;  gtk_init (&argc, &argv);
;
;  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;
;  g_signal_connect (G_OBJECT (window), "destroy",
;		     G_CALLBACK (gtk_main_quit),
;		     NULL);
;
;  gtk_window_set_title (GTK_WINDOW (window), "Spin Button");
;
;  main_vbox = gtk_vbox_new (FALSE, 5);
;  gtk_container_set_border_width (GTK_CONTAINER (main_vbox), 10);
;  gtk_container_add (GTK_CONTAINER (window), main_vbox);
;  
;  frame = gtk_frame_new ("Not accelerated");
;  gtk_box_pack_start (GTK_BOX (main_vbox), frame, TRUE, TRUE, 0);
;  
;  vbox = gtk_vbox_new (FALSE, 0);
;  gtk_container_set_border_width (GTK_CONTAINER (vbox), 5);
;  gtk_container_add (GTK_CONTAINER (frame), vbox);
;  
;  /* Day, month, year spinners */
;  
;  hbox = gtk_hbox_new (FALSE, 0);
;  gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 5);
;  
;  vbox2 = gtk_vbox_new (FALSE, 0);
;  gtk_box_pack_start (GTK_BOX (hbox), vbox2, TRUE, TRUE, 5);
;  
;  label = gtk_label_new ("Day :");
;  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
;  gtk_box_pack_start (GTK_BOX (vbox2), label, FALSE, TRUE, 0);
;  
;  adj = (GtkAdjustment *) gtk_adjustment_new (1.0, 1.0, 31.0, 1.0,
;					       5.0, 0.0);
;  spinner = gtk_spin_button_new (adj, 0, 0);
;  gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
;  gtk_box_pack_start (GTK_BOX (vbox2), spinner, FALSE, TRUE, 0);
;  
;  vbox2 = gtk_vbox_new (FALSE, 0);
;  gtk_box_pack_start (GTK_BOX (hbox), vbox2, TRUE, TRUE, 5);
;  
;  label = gtk_label_new ("Month :");
;  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
;  gtk_box_pack_start (GTK_BOX (vbox2), label, FALSE, TRUE, 0);
;  
;  adj = (GtkAdjustment *) gtk_adjustment_new (1.0, 1.0, 12.0, 1.0,
;					       5.0, 0.0);
;  spinner = gtk_spin_button_new (adj, 0, 0);
;  gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), TRUE);
;  gtk_box_pack_start (GTK_BOX (vbox2), spinner, FALSE, TRUE, 0);
;  
;  vbox2 = gtk_vbox_new (FALSE, 0);
;  gtk_box_pack_start (GTK_BOX (hbox), vbox2, TRUE, TRUE, 5);
;  
;  label = gtk_label_new ("Year :");
;  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
;  gtk_box_pack_start (GTK_BOX (vbox2), label, FALSE, TRUE, 0);
;  
;  adj = (GtkAdjustment *) gtk_adjustment_new (1998.0, 0.0, 2100.0,
;					       1.0, 100.0, 0.0);
;  spinner = gtk_spin_button_new (adj, 0, 0);
;  gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner), FALSE);
;  gtk_widget_set_size_request (spinner, 55, -1);
;  gtk_box_pack_start (GTK_BOX (vbox2), spinner, FALSE, TRUE, 0);
;  
;  frame = gtk_frame_new ("Accelerated");
;  gtk_box_pack_start (GTK_BOX (main_vbox), frame, TRUE, TRUE, 0);
;  
;  vbox = gtk_vbox_new (FALSE, 0);
;  gtk_container_set_border_width (GTK_CONTAINER (vbox), 5);
;  gtk_container_add (GTK_CONTAINER (frame), vbox);
;  
;  hbox = gtk_hbox_new (FALSE, 0);
;  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 5);
;  
;  vbox2 = gtk_vbox_new (FALSE, 0);
;  gtk_box_pack_start (GTK_BOX (hbox), vbox2, TRUE, TRUE, 5);
;  
;  label = gtk_label_new ("Value :");
;  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
;  gtk_box_pack_start (GTK_BOX (vbox2), label, FALSE, TRUE, 0);
;  
;  adj = (GtkAdjustment *) gtk_adjustment_new (0.0, -10000.0, 10000.0,
;					       0.5, 100.0, 0.0);
;  spinner1 = gtk_spin_button_new (adj, 1.0, 2);
;  gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner1), TRUE);
;  gtk_widget_set_size_request (spinner1, 100, -1);
;  gtk_box_pack_start (GTK_BOX (vbox2), spinner1, FALSE, TRUE, 0);
;  
;  vbox2 = gtk_vbox_new (FALSE, 0);
;  gtk_box_pack_start (GTK_BOX (hbox), vbox2, TRUE, TRUE, 5);
;  
;  label = gtk_label_new ("Digits :");
;  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
;  gtk_box_pack_start (GTK_BOX (vbox2), label, FALSE, TRUE, 0);
;  
;  adj = (GtkAdjustment *) gtk_adjustment_new (2, 1, 5, 1, 1, 0);
;  spinner2 = gtk_spin_button_new (adj, 0.0, 0);
;  gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (spinner2), TRUE);
;  g_signal_connect (G_OBJECT (adj), "value_changed",
;		     G_CALLBACK (change_digits),
;		     spinner2);
;  gtk_box_pack_start (GTK_BOX (vbox2), spinner2, FALSE, TRUE, 0);
;  
;  hbox = gtk_hbox_new (FALSE, 0);
;  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 5);
;  
;  button = gtk_check_button_new_with_label ("Snap to 0.5-ticks");
;  g_signal_connect (G_OBJECT (button), "clicked",
;		     G_CALLBACK (toggle_snap),
;		     spinner1);
;  gtk_box_pack_start (GTK_BOX (vbox), button, TRUE, TRUE, 0);
;  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
;  
;  button = gtk_check_button_new_with_label ("Numeric only input mode");
;  g_signal_connect (G_OBJECT (button), "clicked",
;		     G_CALLBACK (toggle_numeric),
;		     spinner1);
;  gtk_box_pack_start (GTK_BOX (vbox), button, TRUE, TRUE, 0);
;  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
;  
;  val_label = gtk_label_new ("");
;  
;  hbox = gtk_hbox_new (FALSE, 0);
;  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 5);
;  button = gtk_button_new_with_label ("Value as Int");
;  g_object_set_data (G_OBJECT (button), "user_data", val_label);
;  g_signal_connect (G_OBJECT (button), "clicked",
;		     G_CALLBACK (get_value),
;		     GINT_TO_POINTER (1));
;  gtk_box_pack_start (GTK_BOX (hbox), button, TRUE, TRUE, 5);
;  
;  button = gtk_button_new_with_label ("Value as Float");
;  g_object_set_data (G_OBJECT (button), "user_data", val_label);
;  g_signal_connect (G_OBJECT (button), "clicked",
;		     G_CALLBACK (get_value),
;		     GINT_TO_POINTER (2));
;  gtk_box_pack_start (GTK_BOX (hbox), button, TRUE, TRUE, 5);
;  
;  gtk_box_pack_start (GTK_BOX (vbox), val_label, TRUE, TRUE, 0);
;  gtk_label_set_text (GTK_LABEL (val_label), "0");
;  
;  hbox = gtk_hbox_new (FALSE, 0);
;  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
;  
;  button = gtk_button_new_with_label ("Close");
;  g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			     G_CALLBACK (gtk_widget_destroy),
;			     window);
;  gtk_box_pack_start (GTK_BOX (hbox), button, TRUE, TRUE, 5);
;
;  gtk_widget_show_all (window);
;
;  /* Enter the event loop */
;  gtk_main ();
;    
;  return 0;
;}
;

(defpackage "09.10-spinbuttons" (:use :excl :common-lisp))
(in-package "09.10-spinbuttons")

(defparameter spinner1 nil)

(ff:defun-foreign-callable toggle-snap ((widget (* gtk:GtkWidget))
					(spin (* gtk:GtkSpinButton)))
  (gtk:gtk_spin_button_set_snap_to_ticks
   spin
   (ff:fslot-value-typed 'gtk:GtkToggleButton nil
			 (gtk:GTK_TOGGLE_BUTTON widget)
			 'gtk::active)))

(ff:defun-foreign-callable toggle-numeric ((widget (* gtk:GtkWidget))
					   (spin (* gtk:GtkSpinButton)))
  (gtk:gtk_spin_button_set_numeric
   spin
   (ff:fslot-value-typed 'gtk:GtkToggleButton nil
			 (gtk:GTK_TOGGLE_BUTTON widget)
			 'gtk::active)))

(ff:defun-foreign-callable change-digits ((widget (* gtk:GtkWidget))
					  (spin (* gtk:GtkSpinButton)))
  (declare (ignore widget))
  (gtk:gtk_spin_button_set_digits
   (gtk:GTK_SPIN_BUTTON spinner1)
   (gtk:gtk_spin_button_get_value_as_int spin)))

(ff:defun-foreign-callable get-value ((widget (* gtk:GtkWidget))
				      (data gtk:gpointer))
  (let ((buf nil)
	(label nil)
	(spin nil))
    
    (setq spin (gtk:GTK_SPIN_BUTTON spinner1))
    (setq label (gtk:GTK_LABEL
		 (gtk:g_object_get_data (gtk:G_OBJECT widget)
					"user_data")))
    (if* (= (gtk:GPOINTER_TO_INT data) 1)
       then (setq buf (format nil "~d"
			      (gtk:gtk_spin_button_get_value_as_int spin)))
       else (setq buf (format nil "~,vf"
			      (ff:fslot-value-typed 'gtk:GtkSpinButton nil
						    spin
						    'gtk::digits)
			      (gtk:gtk_spin_button_get_value spin))))
    (gtk:gtk_label_set_text label buf)))

(ff:defun-foreign-callable cb-gtk-main-quit ()
  (gtk:gtk-main-quit))

(defun spinbuttons ()
  (let ((window nil)
	(frame nil)
	(hbox nil)
	(main_vbox nil)
	(vbox nil)
	(vbox2 nil)
	(spinner2 nil)
	(spinner nil)
	(button nil)
	(label nil)
	(val_label nil)
	(adj nil))

    (gtk:gtk_init 0 0)

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))

    (gtk:g_signal_connect (gtk:G_OBJECT window) "destroy"
			  (gtk:G_CALLBACK
			   #+original (ff:get-entry-point "gtk_main_quit")
			   #-original (ff:register-foreign-callable
				       'cb-gtk-main-quit))
			  gtk:NULL)

    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "Spin Button")

    (setq main_vbox (gtk:gtk_vbox_new gtk:FALSE 5))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER main_vbox) 10)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) main_vbox)

    (setq frame (gtk:gtk_frame_new "Not accelerated"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX main_vbox) frame gtk:TRUE gtk:TRUE 0)

    (setq vbox (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER vbox) 5)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) vbox)

    (setq hbox (gtk:gtk_hbox_new gtk:FALSE 0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) hbox gtk:TRUE gtk:TRUE 5)

    (setq vbox2 (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) vbox2 gtk:TRUE gtk:TRUE 5)

    (setq label (gtk:gtk_label_new "Day :"))
    (gtk:gtk_misc_set_alignment (gtk:GTK_MISC label) 0.0 0.5)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) label gtk:FALSE gtk:TRUE 0)

    (setq adj (gtk:gtk_adjustment_new 1.0d0 1.0d0 31.0d0 1.0d0 5.0d0 0.0d0))
    (setq spinner (gtk:gtk_spin_button_new adj 0.0d0 0))
    (gtk:gtk_spin_button_set_wrap (gtk:GTK_SPIN_BUTTON spinner) gtk:TRUE)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) spinner gtk:FALSE gtk:TRUE 0)

    (setq vbox2 (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) vbox2 gtk:TRUE gtk:TRUE 5)

    (setq label (gtk:gtk_label_new "Month :"))
    (gtk:gtk_misc_set_alignment (gtk:GTK_MISC label) 0.0 0.5)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) label gtk:FALSE gtk:TRUE 0)

    (setq adj (gtk:gtk_adjustment_new 1.0d0 1.0d0 12.0d0 1.0d0 5.0d0 0.0d0))
    (setq spinner (gtk:gtk_spin_button_new adj 0.0d0 0))
    (gtk:gtk_spin_button_set_wrap (gtk:GTK_SPIN_BUTTON spinner) gtk:TRUE)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) spinner gtk:FALSE gtk:TRUE 0)

    (setq vbox2 (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) vbox2 gtk:TRUE gtk:TRUE 5)

    (setq label (gtk:gtk_label_new "Year :"))
    (gtk:gtk_misc_set_alignment (gtk:GTK_MISC label) 0.0 0.5)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) label gtk:FALSE gtk:TRUE 0)

    (setq adj (gtk:gtk_adjustment_new 1998.0d0 0.0d0 2100.0d0 1.0d0 100.0d0
				      0.0d0))
    (setq spinner (gtk:gtk_spin_button_new adj 0.0d0 0))
    (gtk:gtk_spin_button_set_wrap (gtk:GTK_SPIN_BUTTON spinner) gtk:FALSE)
    (gtk:gtk_widget_set_size_request spinner 55 -1)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) spinner gtk:FALSE gtk:TRUE 0)

    (setq frame (gtk:gtk_frame_new "Accelerated"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX main_vbox) frame gtk:TRUE gtk:TRUE 0)

    (setq vbox (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER vbox) 5)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) vbox)

    (setq hbox (gtk:gtk_hbox_new gtk:FALSE 0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) hbox gtk:FALSE gtk:TRUE 5)

    (setq vbox2 (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) vbox2 gtk:TRUE gtk:TRUE 5)

    (setq label (gtk:gtk_label_new "Value :"))	
    (gtk:gtk_misc_set_alignment (gtk:GTK_MISC label)  0.0  0.5)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) label gtk:FALSE gtk:TRUE 0)
  
    (setq adj (gtk:gtk_adjustment_new 0.0d0 -10000.0d0 10000.0d0 0.5d0 100.0d0
				      0.0d0))
    (setq spinner1 (gtk:gtk_spin_button_new adj  1.0d0  2))
    (gtk:gtk_spin_button_set_wrap (gtk:GTK_SPIN_BUTTON spinner1) gtk:TRUE)
    (gtk:gtk_widget_set_size_request spinner1 100 -1)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) spinner1 gtk:FALSE gtk:TRUE 0)
  
    (setq vbox2  (gtk:gtk_vbox_new gtk:FALSE  0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) vbox2 gtk:TRUE gtk:TRUE 5)
  
    (setq label  (gtk:gtk_label_new "Digits :"))
    (gtk:gtk_misc_set_alignment (gtk:GTK_MISC label)  0.0  0.5)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) label gtk:FALSE gtk:TRUE 0)
  
    (setq adj (gtk:gtk_adjustment_new 2.0d0 1.0d0 5.0d0 1.0d0 1.0d0 0.0d0))
    (setq spinner2  (gtk:gtk_spin_button_new adj  0.0d0  0))
    (gtk:gtk_spin_button_set_wrap (gtk:GTK_SPIN_BUTTON spinner2)  gtk:TRUE)
    (gtk:g_signal_connect (gtk:G_OBJECT adj)  "value_changed" 
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'change-digits) )
			  spinner2)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) spinner2 gtk:FALSE gtk:TRUE 0)
  
    (setq hbox (gtk:gtk_hbox_new gtk:FALSE  0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) hbox gtk:FALSE  gtk:TRUE  5)
  
    (setq button (gtk:gtk_check_button_new_with_label "Snap to 0.5-ticks"))
    (gtk:g_signal_connect (gtk:G_OBJECT button)  "clicked" 
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'toggle-snap) )
			  spinner1)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox)  button  gtk:TRUE  gtk:TRUE  0)
    (gtk:gtk_toggle_button_set_active (gtk:GTK_TOGGLE_BUTTON button)  gtk:TRUE)
  
    (setq button (gtk:gtk_check_button_new_with_label
		  "Numeric only input mode"))
    (gtk:g_signal_connect (gtk:G_OBJECT button)  "clicked" 
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'toggle-numeric) )
			  spinner1)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox)  button  gtk:TRUE  gtk:TRUE  0)
    (gtk:gtk_toggle_button_set_active (gtk:GTK_TOGGLE_BUTTON button) gtk:TRUE)
  
    (setq val_label  (gtk:gtk_label_new ""))
  
    (setq hbox (gtk:gtk_hbox_new gtk:FALSE  0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox)  hbox  gtk:FALSE  gtk:TRUE  5)
    (setq button (gtk:gtk_button_new_with_label "Value as Int"))
    (gtk:g_object_set_data (gtk:G_OBJECT button) "user_data" val_label)
    (gtk:g_signal_connect (gtk:G_OBJECT button)  "clicked" 
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'get-value)) 
			  (gtk:GINT_TO_POINTER 1))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox)  button  gtk:TRUE  gtk:TRUE  5)
  
    (setq button (gtk:gtk_button_new_with_label "Value as Float"))
    (gtk:gtk_object_set_data (gtk:G_OBJECT button) "user_data" val_label)
    (gtk:g_signal_connect (gtk:G_OBJECT button)  "clicked" 
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'get-value)) 
			  (gtk:GINT_TO_POINTER 2))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox)  button  gtk:TRUE  gtk:TRUE  5)
  
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) val_label gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_label_set_text (gtk:GTK_LABEL val_label)  "0")
  
    (setq hbox (gtk:gtk_hbox_new gtk:FALSE  0))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX main_vbox) hbox gtk:FALSE gtk:TRUE 0)
  
    (setq button (gtk:gtk_button_new_with_label "Close"))
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT button)  "clicked" 
				  (gtk:G_CALLBACK
				   (ff:get-entry-point "gtk_widget_destroy"))
				  window)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox)  button  gtk:TRUE  gtk:TRUE  5)

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
  (run-example "09.10-spinbuttons" #'spinbuttons))
