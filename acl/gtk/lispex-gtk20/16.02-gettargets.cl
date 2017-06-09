;
;#include <stdlib.h>
;#include <gtk/gtk.h>
;
;void selection_received( GtkWidget        *widget, 
;			  GtkSelectionData *selection_data, 
;			  gpointer          data );
;
;/* Signal handler invoked when user clicks on the "Get Targets" button */
;void get_targets( GtkWidget *widget,
;		   gpointer data )
;{
;  static GdkAtom targets_atom = GDK_NONE;
;
;  /* Get the atom corresponding to the string "TARGETS" */
;  if (targets_atom == GDK_NONE)
;    targets_atom = gdk_atom_intern ("TARGETS", FALSE);
;
;  /* And request the "TARGETS" target for the primary selection */
;  gtk_selection_convert (widget, GDK_SELECTION_PRIMARY, targets_atom,
;			  GDK_CURRENT_TIME);
;}
;
;/* Signal handler called when the selections owner returns the data */
;void selection_received( GtkWidget        *widget,
;			  GtkSelectionData *selection_data, 
;			  gpointer          data )
;{
;  GdkAtom *atoms;
;  GList *item_list;
;  int i;
;
;  /* **** IMPORTANT **** Check to see if retrieval succeeded  */
;  if (selection_data->length < 0)
;    {
;      g_print ("Selection retrieval failed\n");
;      return;
;    }
;  /* Make sure we got the data in the expected form */
;  if (selection_data->type != GDK_SELECTION_TYPE_ATOM)
;    {
;      g_print ("Selection \"TARGETS\" was not returned as atoms!\n");
;      return;
;    }
;  
;  /* Print out the atoms we received */
;  atoms = (GdkAtom *)selection_data->data;
;
;  item_list = NULL;
;  for (i = 0; i < selection_data->length / sizeof(GdkAtom); i++)
;    {
;      char *name;
;      name = gdk_atom_name (atoms[i]);
;      if (name != NULL)
;	 g_print ("%s\n",name);
;      else
;	 g_print ("(bad atom)\n");
;    }
;
;  return;
;}
;
;int main( int   argc,
;	   char *argv[] )
;{
;  GtkWidget *window;
;  GtkWidget *button;
;  
;  gtk_init (&argc, &argv);
;
;  /* Create the toplevel window */
;
;  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;  gtk_window_set_title (GTK_WINDOW (window), "Event Box");
;  gtk_container_set_border_width (GTK_CONTAINER (window), 10);
;
;  g_signal_connect (G_OBJECT (window), "destroy",
;		     G_CALLBACK (exit), NULL);
;
;  /* Create a button the user can click to get targets */
;
;  button = gtk_button_new_with_label ("Get Targets");
;  gtk_container_add (GTK_CONTAINER (window), button);
;
;  g_signal_connect (G_OBJECT(button), "clicked",
;		     G_CALLBACK (get_targets), NULL);
;  g_signal_connect (G_OBJECT(button), "selection_received",
;		     G_CALLBACK (selection_received), NULL);
;
;  gtk_widget_show (button);
;  gtk_widget_show (window);
;  
;  gtk_main ();
;  
;  return 0;
;}

(defpackage "16.02-gettargets" (:use :excl :common-lisp))
(in-package "16.02-gettargets")

(ff:defun-foreign-callable get-targets ((widget (* gtk:GtkWidget))
					(data gtk:gpointer))
  (get-targets-closure widget data))

(let ((targets-atom gtk:GDK_NONE))
  (defun get-targets-closure (widget data)

    (declare (ignore data))
    (when (eql gtk:GDK_NONE targets-atom)
      (setq targets-atom (gtk:gdk_atom_intern "TARGETS" gtk:FALSE)))

    (gtk:gtk_selection_convert widget gtk:GDK_SELECTION_PRIMARY targets-atom
			       gtk:GDK_CURRENT_TIME)))

(ff:defun-foreign-callable selection-received ((widget (* gtk:GtkWidget))
					       (selection-data
						(* gtk:GtkSelectionData))
					       (data gtk:gpointer))
  (declare (ignore widget data))
  (let ((atoms nil))

    (when (< (ff:fslot-value-typed 'gtk:GtkSelectionData nil
				   selection-data 'gtk::length)
	     0)
      (format t "~&Selection retrieval failed~%")
      (return-from selection-received))

    (unless (eql (ff:fslot-value-typed 'gtk:GtkSelectionData nil
				       selection-data 'gtk::type)
		 gtk:GDK_SELECTION_TYPE_ATOM)
      (format t "~&Selection \"TARGETS\" was not returned as atoms!~%")
      (return-from selection-received))

    (setq atoms (ff:fslot-value-typed 'gtk:GtkSelectionData nil
				      selection-data 'gtk::data))
	  
    (dotimes (i (/ (ff:fslot-value-typed 'gtk:GtkSelectionData nil
					 selection-data 'gtk::length)
		   (ff:sizeof-fobject 'gtk:GdkAtom)))
      (let ((name (gtk:gdk_atom_name
		   (ff:fslot-value-typed '(:array gtk:GdkAtom) nil atoms i))))
	(if* (not (eql name gtk:NULL))
	   then (format t "~&~s~%" (native-to-string name))
	   else (format t "~&(bad atom)~%"))))))

(defun gettargets ()
  (let ((window nil)
	(button nil))

    (gtk:gtk_init 0 0)

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "Event Box")
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 10)

    (gtk:g_signal_connect (gtk:G_OBJECT window) "destroy" 
			  (gtk:G_CALLBACK
			   (ff:get-entry-point "gtk_exit"))
			  gtk:NULL)

    (setq button (gtk:gtk_button_new_with_label "Get Targets"))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) button)

    (gtk:g_signal_connect (gtk:G_OBJECT button) "clicked" 
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'get-targets))
			  gtk:NULL)
    (gtk:g_signal_connect (gtk:G_OBJECT button) "selection_received" 
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable
			    'selection-received))
			  gtk:NULL)

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
  (run-example "16.02-gettargets" #'gettargets))
