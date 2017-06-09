;/* example-start buttons buttons.c */
;
;#include <gtk/gtk.h>
;
;/* Create a new hbox with an image and a label packed into it
; * and return the box. */
;
;GtkWidget *xpm_label_box( GtkWidget *parent,
;                          gchar     *xpm_filename,
;                          gchar     *label_text )
;{
;    GtkWidget *box1;
;    GtkWidget *label;
;    GtkWidget *pixmapwid;
;    GdkPixmap *pixmap;
;    GdkBitmap *mask;
;    GtkStyle *style;
;
;    /* Create box for xpm and label */
;    box1 = gtk_hbox_new (FALSE, 0);
;    gtk_container_set_border_width (GTK_CONTAINER (box1), 2);
;
;    /* Get the style of the button to get the
;     * background color. */
;    style = gtk_widget_get_style(parent);
;
;    /* Now on to the xpm stuff */
;    pixmap = gdk_pixmap_create_from_xpm (parent->window, &mask,
;					 &style->bg[GTK_STATE_NORMAL],
;					 xpm_filename);
;    pixmapwid = gtk_pixmap_new (pixmap, mask);
;
;    /* Create a label for the button */
;    label = gtk_label_new (label_text);
;
;    /* Pack the pixmap and label into the box */
;    gtk_box_pack_start (GTK_BOX (box1),
;			pixmapwid, FALSE, FALSE, 3);
;
;    gtk_box_pack_start (GTK_BOX (box1), label, FALSE, FALSE, 3);
;
;    gtk_widget_show(pixmapwid);
;    gtk_widget_show(label);
;
;    return(box1);
;}
;
;/* Our usual callback function */
;void callback( GtkWidget *widget,
;               gpointer   data )
;{
;    g_print ("Hello again - %s was pressed\n", (char *) data);
;}
;
;
;int main( int   argc,
;          char *argv[] )
;{
;    /* GtkWidget is the storage type for widgets */
;    GtkWidget *window;
;    GtkWidget *button;
;    GtkWidget *box1;
;
;    gtk_init (&argc, &argv);
;
;    /* Create a new window */
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;
;    gtk_window_set_title (GTK_WINDOW (window), "Pixmap'd Buttons!");
;
;    /* It's a good idea to do this for all windows. */
;    gtk_signal_connect (GTK_OBJECT (window), "destroy",
;			GTK_SIGNAL_FUNC (gtk_exit), NULL);
;
;    gtk_signal_connect (GTK_OBJECT (window), "delete_event",
;			GTK_SIGNAL_FUNC (gtk_exit), NULL);
;
;    /* Sets the border width of the window. */
;    gtk_container_set_border_width (GTK_CONTAINER (window), 10);
;    gtk_widget_realize(window);
;
;    /* Create a new button */
;    button = gtk_button_new ();
;
;    /* Connect the "clicked" signal of the button to our callback */
;    gtk_signal_connect (GTK_OBJECT (button), "clicked",
;			GTK_SIGNAL_FUNC (callback), (gpointer) "cool button");
;
;    /* This calls our box creating function */
;    box1 = xpm_label_box(window, "info.xpm", "cool button");
;
;    /* Pack and show all our widgets */
;    gtk_widget_show(box1);
;
;    gtk_container_add (GTK_CONTAINER (button), box1);
;
;    gtk_widget_show(button);
;
;    gtk_container_add (GTK_CONTAINER (window), button);
;
;    gtk_widget_show (window);
;
;    /* Rest in gtk_main and wait for the fun to begin! */
;    gtk_main ();
;
;    return(0);
;}
;/* example-end */

(defpackage "06.01-buttons" (:use :excl :common-lisp))
(in-package "06.01-buttons")

(defun xpm-label-box (parent xpm-filename label-text)
  (let ((box1 nil)
	(label nil)
	(pixmapwid nil)
	(pixmap nil)
	(mask nil)
	(amask nil)
	(style nil))

    (setq box1 (gtk:gtk_hbox_new gtk:FALSE 0))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box1) 2)
    (setq style (gtk:gtk_widget_get_style parent))

    ;; So that we can pass &mask to gdk_pixmap_create_from_xpm
    (setq amask (ff:allocate-fobject '(* gtk:GdkBitmap)
				     :foreign-static-gc))
    (setq pixmap (gtk:gdk_pixmap_create_from_xpm
		  ;; parent->window
		  (ff:fslot-value-typed 'gtk:GtkWidget nil parent 'gtk::window)
		  ;; equivalent to passing &mask to a GdkBitmap**.  mask is
		  ;; then retrieved below.
		  amask
		  ;; &style->bg[GTK_STATE_NORMAL]
		  (ff:fslot-address-typed 'gtk:GtkStyle nil style
					  'gtk::bg gtk:GTK_STATE_NORMAL)
		  ;; xpm_filename
		  xpm-filename))
    (setq mask (ff:fslot-value-typed '(* gtk:GdkBitmap) nil amask '*))

    (setq pixmapwid (gtk:gtk_pixmap_new pixmap mask))
    
    (setq label (gtk:gtk_label_new label-text))

    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) pixmapwid gtk:FALSE gtk:FALSE 3)

    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) label gtk:FALSE gtk:FALSE 3)
    
    (gtk:gtk_widget_show pixmapwid)
    (gtk:gtk_widget_show label)
    
    box1))

(ff:defun-foreign-callable callback ((widget (* gtk:GtkWidget))
				     (data gtk:gpointer))
  (declare (ignore widget))
  (format t "~&Hello again - ~a was pressed~%"
	  (native-to-string data :external-format gtk:gpointer-to-string-ef))
  (values))

(defun buttons ()
  (let ((window nil)
	(button nil)
	(box1 nil)
	(callback-cb (ff:register-foreign-callable 'callback)))

    (gtk:gtk_init 0 0)
    
    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))

    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "Pixmap'd Buttons!")
    
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "destroy"
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:get-entry-point "gtk_exit"))
			    gtk:NULL)

    (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "delete_event"
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:get-entry-point "gtk_exit"))
			    gtk:NULL)

    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 10)
    
    (gtk:gtk_widget_realize window)
    
    (setq button (gtk:gtk_button_new))
    
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT button) "clicked"
			    (gtk:GTK_SIGNAL_FUNC callback-cb) "cool button")
    
    (setq box1
      (xpm-label-box
       window
       (namestring
	(merge-pathnames "info.xpm" (load-time-value *load-pathname*)))
       "cool button"))

    (gtk:gtk_widget_show box1)
    
    (gtk:gtk_container_add (gtk:GTK_CONTAINER button) box1)
    
    (gtk:gtk_widget_show button)
    
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) button)
    
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
  (run-example "06.01-buttons" #'buttons))
