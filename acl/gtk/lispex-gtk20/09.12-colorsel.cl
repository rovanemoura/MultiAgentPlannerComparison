;#include <glib.h>
;#include <gdk/gdk.h>
;#include <gtk/gtk.h>
;
;GtkWidget *colorseldlg = NULL;
;GtkWidget *drawingarea = NULL;
;
;/* Color changed handler */
;
;void color_changed_cb( GtkWidget         *widget,
;			GtkColorSelection *colorsel )
;{
;  GdkColor gdk_color;
;  GdkColormap *colormap;
;
;  /* Get drawingarea colormap */
;
;  colormap = gdk_window_get_colormap (drawingarea->window);
;
;  /* Get current color */
;
;  gtk_color_selection_get_current_color (colorsel, &gdk_color);
;
;  /* Allocate color */
;
;  gdk_color_alloc (colormap, &gdk_color);
;
;  /* Set window background color */
;
;  gdk_window_set_background (drawingarea->window, &gdk_color);
;
;  /* Clear window */
;
;  gdk_window_clear (drawingarea->window);
;}
;
;/* Drawingarea event handler */
;
;gint area_event( GtkWidget *widget,
;		  GdkEvent  *event,
;		  gpointer   client_data )
;{
;  gint handled = FALSE;
;  GtkWidget *colorsel;
;
;  /* Check if we've received a button pressed event */
;
;  if (event->type == GDK_BUTTON_PRESS && colorseldlg == NULL)
;    {
;      /* Yes, we have an event and there's no colorseldlg yet! */
;
;      handled = TRUE;
;
;      /* Create color selection dialog */
;
;      colorseldlg = gtk_color_selection_dialog_new ("Select background color");
;
;      /* Get the ColorSelection widget */
;
;      colorsel = GTK_COLOR_SELECTION_DIALOG (colorseldlg)->colorsel;
;
;      /* Connect to the "color_changed" signal, set the client-data
;	* to the colorsel widget */
;
;      g_signal_connect (G_OBJECT (colorsel), "color_changed",
;			 G_CALLBACK (color_changed_cb), (gpointer)colorsel);
;
;      /* Show the dialog */
;
;      gtk_widget_show (colorseldlg);
;    }
;
;  return handled;
;}
;
;/* Close down and exit handler */
;
;gint destroy_window( GtkWidget *widget,
;		      GdkEvent  *event,
;		      gpointer   client_data )
;{
;  gtk_main_quit ();
;  return TRUE;
;}
;
;/* Main */
;
;gint main( gint   argc,
;	    gchar *argv[] )
;{
;  GtkWidget *window;
;
;  /* Initialize the toolkit, remove gtk-related commandline stuff */
;
;  gtk_init (&argc, &argv);
;
;  /* Create toplevel window, set title and policies */
;
;  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;  gtk_window_set_title (GTK_WINDOW (window), "Color selection test");
;  gtk_window_set_policy (GTK_WINDOW (window), TRUE, TRUE, TRUE);
;
;  /* Attach to the "delete" and "destroy" events so we can exit */
;
;  g_signal_connect (GTK_OBJECT (window), "delete_event",
;		     GTK_SIGNAL_FUNC (destroy_window), (gpointer)window);
;  
;  /* Create drawingarea, set size and catch button events */
;
;  drawingarea = gtk_drawing_area_new ();
;
;  gtk_widget_set_size_request (GTK_WIDGET (drawingarea), 200, 200);
;
;  gtk_widget_set_events (drawingarea, GDK_BUTTON_PRESS_MASK);
;
;  g_signal_connect (GTK_OBJECT (drawingarea), "event", 
;		     GTK_SIGNAL_FUNC (area_event), (gpointer)drawingarea);
;  
;  /* Add drawingarea to window, then show them both */
;
;  gtk_container_add (GTK_CONTAINER (window), drawingarea);
;
;  gtk_widget_show (drawingarea);
;  gtk_widget_show (window);
;  
;  /* Enter the gtk main loop (this never returns) */
;
;  gtk_main ();
;
;  /* Satisfy grumpy compilers */
;
;  return 0;
;}

(defpackage "09.12-colorsel" (:use :excl :common-lisp))
(in-package "09.12-colorsel")

(defparameter colorseldlg nil)
(defparameter drawingarea nil)

(ff:defun-foreign-callable color-changed-cb ((widget (* gtk:GtkWidget))
					     (colorsel
					      (* gtk:GtkColorSelection)))
  (declare (ignore widget))
  (let ((gdk-color (ff:allocate-fobject 'gtk:GdkColor
					:foreign-static-gc))
	(colormap nil))

    (setq colormap (gtk:gdk_window_get_colormap
		    (ff:fslot-value-typed 'gtk:GtkWidget nil
					  drawingarea 'gtk::window)))

    (gtk:gtk_color_selection_get_current_color
     colorsel
     (ff:fslot-address-typed 'gtk:GdkColor nil gdk-color))

    (gtk:gdk_color_alloc colormap (ff:fslot-address-typed 'gtk:GdkColor nil
							  gdk-color))

    (gtk:gdk_window_set_background (ff:fslot-value-typed
				    'gtk:GtkWidget nil
				    drawingarea 'gtk::window)
				   (ff:fslot-address-typed 'gtk:GdkColor nil
							   gdk-color))
    (gtk:gdk_window_clear (ff:fslot-value-typed
			   'gtk:GtkWidget nil
			   drawingarea 'gtk::window))))

(ff:defun-foreign-callable area-event ((widget (* gtk:GtkWidget))
				       (event (* gtk:GdkEvent))
				       (client-data gtk:gpointer))
  (declare (ignore client-data widget))
  (let ((handled gtk:FALSE)
	(colorsel nil))

    (when (and
	   (eql (ff:fslot-value-typed 'gtk:GdkEvent nil
				      event 'gtk::type)
		gtk:GDK_BUTTON_PRESS)
	   (not colorseldlg))

      (setq handled gtk:TRUE)

      (setq colorseldlg (gtk:gtk_color_selection_dialog_new
			 "Select background color"))

      (setq colorsel 
	(ff:fslot-value-typed 'gtk:GtkColorSelectionDialog nil
			      (gtk:GTK_COLOR_SELECTION_DIALOG colorseldlg)
			      'gtk::colorsel))

      (gtk:g_signal_connect (gtk:G_OBJECT colorsel) "color_changed"
			    (gtk:G_CALLBACK
			     (ff:register-foreign-callable 'color-changed-cb))
			    colorsel)
      
      (gtk:gtk_widget_show colorseldlg))
    handled))

(ff:defun-foreign-callable destroy-window ((widget (* gtk:GtkWidget))
					   (event (* gtk:GdkEvent))
					   (client-data gtk:gpointer))
  (declare (ignore widget event client-data))
  #+original (gtk:gtk_main_quit)
  #-original (gtk:gtk-main-quit)
  gtk:TRUE)

(defun colorsel ()
  (let ((window nil))
    
    (gtk:gtk_init 0 0)

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "Color selection test")
    (gtk:gtk_window_set_policy (gtk:GTK_WINDOW window) gtk:TRUE gtk:TRUE
			       gtk:TRUE)

    (gtk:g_signal_connect (gtk:G_OBJECT window) "delete_event"
			  (gtk:GTK_SIGNAL_FUNC
			   (ff:register-foreign-callable 'destroy-window))
			  window)

    (setq drawingarea (gtk:gtk_drawing_area_new))

    (gtk:gtk_widget_set_size_request (gtk:GTK_WIDGET drawingarea) 200 200)

    (gtk:gtk_widget_set_events drawingarea gtk:GDK_BUTTON_PRESS_MASK)

    (gtk:g_signal_connect (gtk:GTK_OBJECT drawingarea) "event"
			  (gtk:GTK_SIGNAL_FUNC
			   (ff:register-foreign-callable 'area-event))
			  drawingarea)

    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) drawingarea)

    (gtk:gtk_widget_show drawingarea)
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
  (run-example "09.12-colorsel" #'colorsel))
