;
;#include <glib.h>
;#include <gtk/gtk.h>
;
;gint close_application( GtkWidget *widget,
;			 GdkEvent  *event,
;			 gpointer   data )
;{
;  gtk_main_quit ();
;  return FALSE;
;}
;
;int main( int   argc,
;	   char *argv[] )
;{
;    GtkWidget *window = NULL;
;    GtkWidget *box1;
;    GtkWidget *box2;
;    GtkWidget *button;
;    GtkWidget *separator;
;    GSList *group;
;  
;    gtk_init (&argc, &argv);    
;      
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;  
;    g_signal_connect (G_OBJECT (window), "delete_event",
;		       G_CALLBACK (close_application),
;		       NULL);
;
;    gtk_window_set_title (GTK_WINDOW (window), "radio buttons");
;    gtk_container_set_border_width (GTK_CONTAINER (window), 0);
;
;    box1 = gtk_vbox_new (FALSE, 0);
;    gtk_container_add (GTK_CONTAINER (window), box1);
;    gtk_widget_show (box1);
;
;    box2 = gtk_vbox_new (FALSE, 10);
;    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
;    gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
;    gtk_widget_show (box2);
;
;    button = gtk_radio_button_new_with_label (NULL, "button1");
;    gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
;    gtk_widget_show (button);
;
;    group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button));
;    button = gtk_radio_button_new_with_label (group, "button2");
;    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
;    gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
;    gtk_widget_show (button);
;
;    button = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (button),
;							   "button3");
;    gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
;    gtk_widget_show (button);
;
;    separator = gtk_hseparator_new ();
;    gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 0);
;    gtk_widget_show (separator);
;
;    box2 = gtk_vbox_new (FALSE, 10);
;    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
;    gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, TRUE, 0);
;    gtk_widget_show (box2);
;
;    button = gtk_button_new_with_label ("close");
;    g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			       G_CALLBACK (close_application),
;			       window);
;    gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
;    GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
;    gtk_widget_grab_default (button);
;    gtk_widget_show (button);
;    gtk_widget_show (window);
;     
;    gtk_main ();
;
;    return 0;
;}

(defpackage "06.04-radiobuttons" (:use :excl :common-lisp))
(in-package "06.04-radiobuttons")

(ff:defun-foreign-callable close-application ((widget (* gtk:GtkWidget))
					      (event (* gtk:GdkEvent))
					      (data gtk:gpointer))
  (declare (ignore widget event data))
  #+original (gtk:gtk_main_quit)
  #-original (gtk:gtk-main-quit)
  gtk:FALSE)


(defun radiobuttons ()
  (let ((window nil)
	(box1 nil)
	(box2 nil)
	(button nil)
	(separator nil)
	(group nil)
	(close-application-cb (ff:register-foreign-callable
			       'close-application)))
    
    (gtk:gtk_init 0 0)
    
    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    
    (gtk:g_signal_connect (gtk:G_OBJECT window) "delete_event"
			  (gtk:G_CALLBACK close-application-cb)
			  gtk:NULL)

    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "radio buttons")
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 0)
    
    (setq box1 (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) box1)
    (gtk:gtk_widget_show box1)
    
    (setq box2 (gtk:gtk_vbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq button (gtk:gtk_radio_button_new_with_label gtk:NULL "button1"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) button gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show button)

    (setq group (gtk:gtk_radio_button_get_group (gtk:GTK_RADIO_BUTTON button)))
    (setq button (gtk:gtk_radio_button_new_with_label group "button2"))
    (gtk:gtk_toggle_button_set_active (gtk:GTK_TOGGLE_BUTTON button) gtk:TRUE)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) button gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show button)

    (setq button (gtk:gtk_radio_button_new_with_label_from_widget
		  (gtk:GTK_RADIO_BUTTON button)
		  "button3"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) button gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show button)
    
    (setq separator (gtk:gtk_hseparator_new))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) separator gtk:FALSE gtk:TRUE 0)
    (gtk:gtk_widget_show separator)
    
    (setq box2 (gtk:gtk_vbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq button (gtk:gtk_button_new_with_label "close"))
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT button) "clicked"
				  (gtk:G_CALLBACK close-application-cb)
				  window)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) button gtk:TRUE gtk:TRUE 0)
    #+out
    (let ((f (ff:fslot-value-typed 'gtk:GtkObject nil button 'gtk::flags)))
      (setf (ff:fslot-value-typed 'gtk:GtkObject nil button 'gtk::flags)
	(logior f gtk:GTK_CAN_DEFAULT)))
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
  (run-example "06.04-radiobuttons" #'radiobuttons))
