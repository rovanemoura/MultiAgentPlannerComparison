;/* example-start colorsel colorsel.c */
;
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
;  gdouble color[3];
;  GdkColor gdk_color;
;  GdkColormap *colormap;
;
;  /* Get drawingarea colormap */
;
;  colormap = gdk_window_get_colormap (drawingarea->window);
;
;  /* Get current color */
;
;  gtk_color_selection_get_color (colorsel,color);
;
;  /* Fit to a unsigned 16 bit integer (0..65535) and
;   * insert into the GdkColor structure */
;
;  gdk_color.red = (guint16)(color[0]*65535.0);
;  gdk_color.green = (guint16)(color[1]*65535.0);
;  gdk_color.blue = (guint16)(color[2]*65535.0);
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
;      colorseldlg = gtk_color_selection_dialog_new("Select background color");
;
;      /* Get the ColorSelection widget */
;
;      colorsel = GTK_COLOR_SELECTION_DIALOG(colorseldlg)->colorsel;
;
;      /* Connect to the "color_changed" signal, set the client-data
;	* to the colorsel widget */
;
;      gtk_signal_connect(GTK_OBJECT(colorsel), "color_changed",
;	 (GtkSignalFunc)color_changed_cb, (gpointer)colorsel);
;
;      /* Show the dialog */
;
;      gtk_widget_show(colorseldlg);
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
;  return(TRUE);
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
;  gtk_init (&argc,&argv);
;
;  /* Create toplevel window, set title and policies */
;
;  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;  gtk_window_set_title (GTK_WINDOW(window), "Color selection test");
;  gtk_window_set_policy (GTK_WINDOW(window), TRUE, TRUE, TRUE);
;
;  /* Attach to the "delete" and "destroy" events so we can exit */
;
;  gtk_signal_connect (GTK_OBJECT(window), "delete_event",
;    (GtkSignalFunc)destroy_window, (gpointer)window);
;  
;  /* Create drawingarea, set size and catch button events */
;
;  drawingarea = gtk_drawing_area_new ();
;
;  gtk_drawing_area_size (GTK_DRAWING_AREA(drawingarea), 200, 200);
;
;  gtk_widget_set_events (drawingarea, GDK_BUTTON_PRESS_MASK);
;
;  gtk_signal_connect (GTK_OBJECT(drawingarea), "event", 
;    (GtkSignalFunc)area_event, (gpointer)drawingarea);
;  
;  /* Add drawingarea to window, then show them both */
;
;  gtk_container_add (GTK_CONTAINER(window), drawingarea);
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
;  return(0);
;}
;/* example-end */


(defpackage "09.13-colorsel" (:use :excl :common-lisp))
(in-package "09.13-colorsel")

(defparameter colorseldlg nil)
(defparameter drawingarea nil)

(ff:defun-foreign-callable color-changed-cb ((widget (* gtk:GtkWidget))
					     (colorsel
					      (* gtk:GtkColorSelection)))
  (declare (ignore widget))
  (let ((color (ff:allocate-fobject '(:array gtk:gdouble 3)
				    :foreign-static-gc))
	(gdk-color (ff:allocate-fobject 'gtk:GdkColor
					:foreign-static-gc))
	(colormap nil))

    (setq colormap (gtk:gdk_window_get_colormap
		    (ff:fslot-value-typed 'gtk:GtkWidget nil
					  drawingarea 'gtk::window)))

    (gtk:gtk_color_selection_get_color colorsel color)

    (setf (ff:fslot-value-typed 'gtk:GdkColor nil gdk-color 'gtk::red)
      (round
       (* (ff:fslot-value-typed '(:array gtk:gdouble) nil
				color 0)
	  65535.0)))
    (setf (ff:fslot-value-typed 'gtk:GdkColor nil gdk-color 'gtk::green)
      (round
       (* (ff:fslot-value-typed '(:array gtk:gdouble) nil
				color 1)
	  65535.0)))
    (setf (ff:fslot-value-typed 'gtk:GdkColor nil gdk-color 'gtk::blue)
      (round
       (* (ff:fslot-value-typed '(:array gtk:gdouble) nil
				color 2)
	  65535.0)))

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

      (gtk:gtk_signal_connect (gtk:GTK_OBJECT colorsel) "color_changed"
			      (ff:register-foreign-callable 'color-changed-cb)
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

    (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "delete_event"
			    (ff:register-foreign-callable 'destroy-window)
			    window)

    (setq drawingarea (gtk:gtk_drawing_area_new))

    (gtk:gtk_drawing_area_size (gtk:GTK_DRAWING_AREA drawingarea) 200 200)

    (gtk:gtk_widget_set_events drawingarea gtk:GDK_BUTTON_PRESS_MASK)

    (gtk:gtk_signal_connect (gtk:GTK_OBJECT drawingarea) "event"
			    (ff:register-foreign-callable 'area-event)
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
  (run-example "09.13-colorsel" #'colorsel))
