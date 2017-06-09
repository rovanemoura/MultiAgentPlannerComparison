;
;#include <stdio.h>
;#include <stdlib.h>
;#include <gtk/gtk.h>
;
;void enter_callback( GtkWidget *widget,
;		      GtkWidget *entry )
;{
;  const gchar *entry_text;
;  entry_text = gtk_entry_get_text (GTK_ENTRY (entry));
;  printf("Entry contents: %s\n", entry_text);
;}
;
;void entry_toggle_editable( GtkWidget *checkbutton,
;			     GtkWidget *entry )
;{
;  gtk_editable_set_editable (GTK_EDITABLE (entry),
;			      GTK_TOGGLE_BUTTON (checkbutton)->active);
;}
;
;void entry_toggle_visibility( GtkWidget *checkbutton,
;			       GtkWidget *entry )
;{
;  gtk_entry_set_visibility (GTK_ENTRY (entry),
;			     GTK_TOGGLE_BUTTON (checkbutton)->active);
;}
;
;int main( int   argc,
;	   char *argv[] )
;{
;
;    GtkWidget *window;
;    GtkWidget *vbox, *hbox;
;    GtkWidget *entry;
;    GtkWidget *button;
;    GtkWidget *check;
;    gint tmp_pos;
;
;    gtk_init (&argc, &argv);
;
;    /* create a new window */
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;    gtk_widget_set_size_request (GTK_WIDGET (window), 200, 100);
;    gtk_window_set_title (GTK_WINDOW (window), "GTK Entry");
;    g_signal_connect (G_OBJECT (window), "destroy",
;		       G_CALLBACK (gtk_main_quit), NULL);
;    g_signal_connect_swapped (G_OBJECT (window), "delete_event",
;			       G_CALLBACK (gtk_widget_destroy), 
;			       window);
;
;    vbox = gtk_vbox_new (FALSE, 0);
;    gtk_container_add (GTK_CONTAINER (window), vbox);
;    gtk_widget_show (vbox);
;
;    entry = gtk_entry_new ();
;    gtk_entry_set_max_length (GTK_ENTRY (entry), 50);
;    g_signal_connect (G_OBJECT (entry), "activate",
;		       G_CALLBACK (enter_callback),
;		       entry);
;    gtk_entry_set_text (GTK_ENTRY (entry), "hello");
;    tmp_pos = GTK_ENTRY (entry)->text_length;
;    gtk_editable_insert_text (GTK_EDITABLE (entry), " world", -1, &tmp_pos);
;    gtk_editable_select_region (GTK_EDITABLE (entry),
;				 0, GTK_ENTRY (entry)->text_length);
;    gtk_box_pack_start (GTK_BOX (vbox), entry, TRUE, TRUE, 0);
;    gtk_widget_show (entry);
;
;    hbox = gtk_hbox_new (FALSE, 0);
;    gtk_container_add (GTK_CONTAINER (vbox), hbox);
;    gtk_widget_show (hbox);
;				   
;    check = gtk_check_button_new_with_label ("Editable");
;    gtk_box_pack_start (GTK_BOX (hbox), check, TRUE, TRUE, 0);
;    g_signal_connect (G_OBJECT (check), "toggled",
;		       G_CALLBACK (entry_toggle_editable), entry);
;    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check), TRUE);
;    gtk_widget_show (check);
;    
;    check = gtk_check_button_new_with_label ("Visible");
;    gtk_box_pack_start (GTK_BOX (hbox), check, TRUE, TRUE, 0);
;    g_signal_connect (G_OBJECT (check), "toggled",
;		       G_CALLBACK (entry_toggle_visibility), entry);
;    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check), TRUE);
;    gtk_widget_show (check);
;				    
;    button = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
;    g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			       G_CALLBACK (gtk_widget_destroy),
;			       window);
;    gtk_box_pack_start (GTK_BOX (vbox), button, TRUE, TRUE, 0);
;    GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
;    gtk_widget_grab_default (button);
;    gtk_widget_show (button);
;    
;    gtk_widget_show (window);
;
;    gtk_main();
;
;    return 0;
;}

(defpackage "09.08-entry" (:use :excl :common-lisp))
(in-package "09.08-entry")

(ff:defun-foreign-callable enter-callback ((widget (* gtk:GtkWidget))
					   (entry (* gtk:GtkWidget)))
  (declare (ignore widget))
  (let ((entry-text (gtk:gtk_entry_get_text (gtk:GTK_ENTRY entry))))
    (format t "~&Entry contents: ~s~%" (native-to-string entry-text))))

(ff:defun-foreign-callable entry-toggle-editable ((checkbutton (*
								gtk:GtkWidget))
						  (entry (* gtk:GtkWidget)))
  (gtk:gtk_editable_set_editable
   (gtk:GTK_EDITABLE entry)
   (ff:fslot-value-typed 'gtk:GtkToggleButton nil
			 (gtk:GTK_TOGGLE_BUTTON checkbutton)
			 'gtk::active)))

(ff:defun-foreign-callable entry-toggle-visibility ((checkbutton
						     (* gtk:GtkWidget))
						    (entry (* gtk:GtkWidget)))
  (gtk:gtk_entry_set_visibility
   (gtk:GTK_ENTRY entry)
   (ff:fslot-value-typed 'gtk:GtkToggleButton nil
			 (gtk:GTK_TOGGLE_BUTTON checkbutton)
			 'gtk::active)))

(ff:defun-foreign-callable cb-gtk-main-quit ()
  (gtk:gtk-main-quit))

(defun entry ()
  (let ((window nil)
	(vbox nil)
	(hbox nil)
	(entry nil)
	(button nil)
	(check nil)
	(tmp-pos nil))

    (gtk:gtk_init 0 0)

    (setq tmp-pos (ff:allocate-fobject 'gtk:gint :foreign-static-gc))

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:gtk_widget_set_size_request (gtk:GTK_WIDGET window) 200 100)
    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "GTK Entry")
    (gtk:g_signal_connect (gtk:G_OBJECT window) "destroy"
			  (gtk:G_CALLBACK
			   #+original (ff:get-entry-point "gtk_main_quit")
			   #-original (ff:register-foreign-callable
				       'cb-gtk-main-quit))
			  gtk:NULL)
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT window) "delete_event"
				  (gtk:G_CALLBACK 
				   (ff:get-entry-point "gtk_widget_destroy"))
				  window)

    (setq vbox (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) vbox)
    (gtk:gtk_widget_show vbox)

    (setq entry (gtk:gtk_entry_new))
    (gtk:gtk_entry_set_max_length (gtk:GTK_ENTRY entry) 50)
    (gtk:g_signal_connect (gtk:G_OBJECT entry) "activate"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'enter-callback))
			  entry)
    (gtk:gtk_entry_set_text (gtk:GTK_ENTRY entry) "hello")
    (setf (ff:fslot-value-typed 'gtk:gint nil tmp-pos)
      (ff:fslot-value-typed 'gtk:GtkEntry nil
			    (gtk:GTK_ENTRY entry)
			    'gtk::text_length))
    (gtk:gtk_editable_insert_text (gtk:GTK_EDITABLE entry) " world" -1
				  (ff:fslot-address-typed
				   'gtk:gint nil tmp-pos))
    (gtk:gtk_editable_select_region (gtk:GTK_EDITABLE entry)
				    0
				    (ff:fslot-value-typed 'gtk:GtkEntry nil
							  (gtk:GTK_ENTRY entry)
							  'gtk::text_length))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) entry gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show entry)

    (setq hbox (gtk:gtk_hbox_new gtk:FALSE 0))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER vbox) hbox)
    (gtk:gtk_widget_show hbox)

    (setq check (gtk:gtk_check_button_new_with_label "Editable"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) check gtk:TRUE gtk:TRUE 0)
    (gtk:g_signal_connect (gtk:G_OBJECT check) "toggled"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable
			    'entry-toggle-editable))
			  entry)
    (gtk:gtk_toggle_button_set_active (gtk:GTK_TOGGLE_BUTTON check) gtk:TRUE)
    (gtk:gtk_widget_show check)

    (setq check (gtk:gtk_check_button_new_with_label "Visible"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) check gtk:TRUE gtk:TRUE 0)
    (gtk:g_signal_connect (gtk:G_OBJECT check) "toggled"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable
			    'entry-toggle-visibility))
			  entry)
    (gtk:gtk_toggle_button_set_active (gtk:GTK_TOGGLE_BUTTON check) gtk:TRUE)
    (gtk:gtk_widget_show check)

    (setq button (gtk:gtk_button_new_from_stock gtk:GTK_STOCK_CLOSE))
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT button) "clicked"
				  (gtk:G_CALLBACK
				   (ff:get-entry-point "gtk_widget_destroy"))
				  window)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) button gtk:TRUE gtk:TRUE 0)
    (gtk:GTK_WIDGET_SET_FLAGS button gtk:GTK_CAN_DEFAULT)
    (gtk:gtk_widget_grab_default button)
    (gtk:gtk_widget_show button)

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
  (run-example "09.08-entry" #'entry))
