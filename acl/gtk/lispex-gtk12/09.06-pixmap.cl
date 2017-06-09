;/* example-start pixmap pixmap.c */
;
;#include <gtk/gtk.h>
;
;
;/* XPM data of Open-File icon */
;static const char * xpm_data[] = {
;"16 16 3 1",
;"       c None",
;".      c #000000000000",
;"X      c #FFFFFFFFFFFF",
;"                ",
;"   ......       ",
;"   .XXX.X.      ",
;"   .XXX.XX.     ",
;"   .XXX.XXX.    ",
;"   .XXX.....    ",
;"   .XXXXXXX.    ",
;"   .XXXXXXX.    ",
;"   .XXXXXXX.    ",
;"   .XXXXXXX.    ",
;"   .XXXXXXX.    ",
;"   .XXXXXXX.    ",
;"   .XXXXXXX.    ",
;"   .........    ",
;"                ",
;"                "};
;
;
;/* when invoked (via signal delete_event), terminates the application.
; */
;gint close_application( GtkWidget *widget,
;			 GdkEvent  *event,
;			 gpointer   data )
;{
;    gtk_main_quit();
;    return(FALSE);
;}
;
;
;/* is invoked when the button is clicked.  It just prints a message.
; */
;void button_clicked( GtkWidget *widget,
;		      gpointer   data ) {
;    g_print( "button clicked\n" );
;}
;
;int main( int   argc,
;	   char *argv[] )
;{
;    /* GtkWidget is the storage type for widgets */
;    GtkWidget *window, *pixmapwid, *button;
;    GdkPixmap *pixmap;
;    GdkBitmap *mask;
;    GtkStyle *style;
;    
;    /* create the main window, and attach delete_event signal to terminating
;	the application */
;    gtk_init( &argc, &argv );
;    window = gtk_window_new( GTK_WINDOW_TOPLEVEL );
;    gtk_signal_connect( GTK_OBJECT (window), "delete_event",
;			 GTK_SIGNAL_FUNC (close_application), NULL );
;    gtk_container_set_border_width( GTK_CONTAINER (window), 10 );
;    gtk_widget_show( window );
;
;    /* now for the pixmap from gdk */
;    style = gtk_widget_get_style( window );
;    pixmap = gdk_pixmap_create_from_xpm_d( window->window,  &mask,
;					    &style->bg[GTK_STATE_NORMAL],
;					    (gchar **)xpm_data );
;
;    /* a pixmap widget to contain the pixmap */
;    pixmapwid = gtk_pixmap_new( pixmap, mask );
;    gtk_widget_show( pixmapwid );
;
;    /* a button to contain the pixmap widget */
;    button = gtk_button_new();
;    gtk_container_add( GTK_CONTAINER(button), pixmapwid );
;    gtk_container_add( GTK_CONTAINER(window), button );
;    gtk_widget_show( button );
;
;    gtk_signal_connect( GTK_OBJECT(button), "clicked",
;			 GTK_SIGNAL_FUNC(button_clicked), NULL );
;
;    /* show the window */
;    gtk_main ();
;	   
;    return 0;
;}
;/* example-end */

(defpackage "09.06-pixmap" (:use :excl :common-lisp))
(in-package "09.06-pixmap")

(defun lisp-string-array-to-c-string-array (a)
  (let ((r (ff:allocate-fobject (list ':array '(* :char) (length a))
				:foreign-static-gc)))
    (dotimes (i (length a))
      (setf (ff:fslot-value-typed '(:array (* :char)) nil r i)
	(string-to-native (aref a i))))
    r))

(defparameter xpm-data
    (lisp-string-array-to-c-string-array
     '#("16 16 3 1"
	"       c None"
	".      c #000000000000"
	"X      c #FFFFFFFFFFFF"
	"                "
	"   ......       "
	"   .XXX.X.      "
	"   .XXX.XX.     "
	"   .XXX.XXX.    "
	"   .XXX.....    "
	"   .XXXXXXX.    "
	"   .XXXXXXX.    "
	"   .XXXXXXX.    "
	"   .XXXXXXX.    "
	"   .XXXXXXX.    "
	"   .XXXXXXX.    "
	"   .XXXXXXX.    "
	"   .........    "
	"                "
	"                ")))

(ff:defun-foreign-callable close-application ((widget (* gtk:GtkWidget))
					      (event (* gtk:GdkEvent))
					      (data gtk:gpointer))
  (declare (ignore widget event data))
  #+original (gtk:gtk_main_quit)
  #-original (gtk:gtk-main-quit)
  gtk:FALSE)

(ff:defun-foreign-callable button-clicked ((widget (* gtk:GtkWidget))
					   (data gtk:gpointer))
  (declare (ignore widget data))
  (format t "~&Button clicked~%"))

(defun pixmap ()
  (let ((window nil)
	(pixmapwid nil)
	(button nil)
	(pixmap nil)
	(mask nil)
	(amask nil)
	(style nil))
    
    (gtk:gtk_init 0 0)
    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "delete_event"
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'close-application))
			    gtk:NULL)
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 10)
    (gtk:gtk_widget_show window)

    (setq style (gtk:gtk_widget_get_style window))

    ;; So that we can pass &mask to gdk_pixmap_create_from_xpm
    (setq amask (ff:allocate-fobject '(* gtk:GdkBitmap)
				     :foreign-static-gc))
    (setq pixmap (gtk:gdk_pixmap_create_from_xpm_d
		  ;; window->window
		  (ff:fslot-value-typed 'gtk:GtkWidget nil window 'gtk::window)
		  ;; equivalent to passing &mask to a GdkBitmap**.  mask is
		  ;; then retrieved below.
		  amask
		  ;; &style->bg[GTK_STATE_NORMAL]
		  (ff:fslot-address-typed 'gtk:GtkStyle nil style
					  'gtk::bg gtk:GTK_STATE_NORMAL)
		  ;; xpm_data
		  xpm-data))
    (setq mask (ff:fslot-value-typed '(* gtk:GdkBitmap) nil amask '*))

    (setq pixmapwid (gtk:gtk_pixmap_new pixmap mask))
    (gtk:gtk_widget_show pixmapwid)

    (setq button (gtk:gtk_button_new))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER button) pixmapwid)
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) button)
    (gtk:gtk_widget_show button)

    (gtk:gtk_signal_connect (gtk:GTK_OBJECT button) "clicked"
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'button-clicked))
			    gtk:NULL)

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
  (run-example "09.06-pixmap" #'pixmap))
