;/* example-start text text.c */
;
;/* text.c */
;
;#include <stdio.h>
;#include <gtk/gtk.h>
;
;void text_toggle_editable (GtkWidget *checkbutton,
;			    GtkWidget *text)
;{
;  gtk_text_set_editable(GTK_TEXT(text),
;			 GTK_TOGGLE_BUTTON(checkbutton)->active);
;}
;
;void text_toggle_word_wrap (GtkWidget *checkbutton,
;			     GtkWidget *text)
;{
;  gtk_text_set_word_wrap(GTK_TEXT(text),
;			  GTK_TOGGLE_BUTTON(checkbutton)->active);
;}
;
;void close_application( GtkWidget *widget,
;			 gpointer   data )
;{
;	gtk_main_quit();
;}
;
;int main( int argc,
;	   char *argv[] )
;{
;  GtkWidget *window;
;  GtkWidget *box1;
;  GtkWidget *box2;
;  GtkWidget *hbox;
;  GtkWidget *button;
;  GtkWidget *check;
;  GtkWidget *separator;
;  GtkWidget *table;
;  GtkWidget *vscrollbar;
;  GtkWidget *text;
;  GdkColormap *cmap;
;  GdkColor color;
;  GdkFont *fixed_font;
;
;  FILE *infile;
;
;  gtk_init (&argc, &argv);
; 
;  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;  gtk_widget_set_usize (window, 600, 500);
;  gtk_window_set_policy (GTK_WINDOW(window), TRUE, TRUE, FALSE);  
;  gtk_signal_connect (GTK_OBJECT (window), "destroy",
;		       GTK_SIGNAL_FUNC(close_application),
;		       NULL);
;  gtk_window_set_title (GTK_WINDOW (window), "Text Widget Example");
;  gtk_container_set_border_width (GTK_CONTAINER (window), 0);
;  
;  
;  box1 = gtk_vbox_new (FALSE, 0);
;  gtk_container_add (GTK_CONTAINER (window), box1);
;  gtk_widget_show (box1);
;  
;  
;  box2 = gtk_vbox_new (FALSE, 10);
;  gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
;  gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
;  gtk_widget_show (box2);
;  
;  
;  table = gtk_table_new (2, 2, FALSE);
;  gtk_table_set_row_spacing (GTK_TABLE (table), 0, 2);
;  gtk_table_set_col_spacing (GTK_TABLE (table), 0, 2);
;  gtk_box_pack_start (GTK_BOX (box2), table, TRUE, TRUE, 0);
;  gtk_widget_show (table);
;  
;  /* Create the GtkText widget */
;  text = gtk_text_new (NULL, NULL);
;  gtk_text_set_editable (GTK_TEXT (text), TRUE);
;  gtk_table_attach (GTK_TABLE (table), text, 0, 1, 0, 1,
;		     GTK_EXPAND | GTK_SHRINK | GTK_FILL,
;		     GTK_EXPAND | GTK_SHRINK | GTK_FILL, 0, 0);
;  gtk_widget_show (text);
;
;  /* Add a vertical scrollbar to the GtkText widget */
;  vscrollbar = gtk_vscrollbar_new (GTK_TEXT (text)->vadj);
;  gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1,
;		     GTK_FILL, GTK_EXPAND | GTK_SHRINK | GTK_FILL, 0, 0);
;  gtk_widget_show (vscrollbar);
;
;  /* Get the system color map and allocate the color red */
;  cmap = gdk_colormap_get_system();
;  color.red = 0xffff;
;  color.green = 0;
;  color.blue = 0;
;  if (!gdk_color_alloc(cmap, &color)) {
;    g_error("couldn't allocate color");
;  }
;
;  /* Load a fixed font */
;  fixed_font = gdk_font_load ("-misc-fixed-medium-r-*-*-*-140-*-*-*-*-*-*");
;
;  /* Realizing a widget creates a window for it,
;   * ready for us to insert some text */
;  gtk_widget_realize (text);
;
;  /* Freeze the text widget, ready for multiple updates */
;  gtk_text_freeze (GTK_TEXT (text));
;  
;  /* Insert some colored text */
;  gtk_text_insert (GTK_TEXT (text), NULL, &text->style->black, NULL,
;		    "Supports ", -1);
;  gtk_text_insert (GTK_TEXT (text), NULL, &color, NULL,
;		    "colored ", -1);
;  gtk_text_insert (GTK_TEXT (text), NULL, &text->style->black, NULL,
;		    "text and different ", -1);
;  gtk_text_insert (GTK_TEXT (text), fixed_font, &text->style->black, NULL,
;		    "fonts\n\n", -1);
;  
;  /* Load the file text.c into the text window */
;
;  infile = fopen("text.c", "r");
;  
;  if (infile) {
;    char buffer[1024];
;    int nchars;
;    
;    while (1)
;      {
;	 nchars = fread(buffer, 1, 1024, infile);
;	 gtk_text_insert (GTK_TEXT (text), fixed_font, NULL,
;			  NULL, buffer, nchars);
;	 
;	 if (nchars < 1024)
;	   break;
;      }
;    
;    fclose (infile);
;  }
;
;  /* Thaw the text widget, allowing the updates to become visible */  
;  gtk_text_thaw (GTK_TEXT (text));
;  
;  hbox = gtk_hbutton_box_new ();
;  gtk_box_pack_start (GTK_BOX (box2), hbox, FALSE, FALSE, 0);
;  gtk_widget_show (hbox);
;
;  check = gtk_check_button_new_with_label("Editable");
;  gtk_box_pack_start (GTK_BOX (hbox), check, FALSE, FALSE, 0);
;  gtk_signal_connect (GTK_OBJECT(check), "toggled",
;		       GTK_SIGNAL_FUNC(text_toggle_editable), text);
;  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), TRUE);
;  gtk_widget_show (check);
;  check = gtk_check_button_new_with_label("Wrap Words");
;  gtk_box_pack_start (GTK_BOX (hbox), check, FALSE, TRUE, 0);
;  gtk_signal_connect (GTK_OBJECT(check), "toggled",
;		       GTK_SIGNAL_FUNC(text_toggle_word_wrap), text);
;  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), FALSE);
;  gtk_widget_show (check);
;
;  separator = gtk_hseparator_new ();
;  gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 0);
;  gtk_widget_show (separator);
;
;  box2 = gtk_vbox_new (FALSE, 10);
;  gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
;  gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, TRUE, 0);
;  gtk_widget_show (box2);
;  
;  button = gtk_button_new_with_label ("close");
;  gtk_signal_connect (GTK_OBJECT (button), "clicked",
;		       GTK_SIGNAL_FUNC(close_application),
;		       NULL);
;  gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
;  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
;  gtk_widget_grab_default (button);
;  gtk_widget_show (button);
;
;  gtk_widget_show (window);
;
;  gtk_main ();
;  
;  return(0);       
;}
;/* example-end */


(defpackage "15.04-text" (:use :excl :common-lisp))
(in-package "15.04-text")

(ff:defun-foreign-callable text-toggle-editable ((checkbutton
						  (* gtk:GtkWidget))
						 (text (* gtk:GtkWidget)))
  (gtk:gtk_text_set_editable (gtk:GTK_TEXT text)
			     (ff:fslot-value-typed
			      'gtk:GtkToggleButton nil
			      (gtk:GTK_TOGGLE_BUTTON checkbutton)
			      'gtk::active)))

(ff:defun-foreign-callable text-toggle-word-wrap ((checkbutton
						   (* gtk:GtkWidget))
						  (text (* gtk:GtkWidget)))
  (gtk:gtk_text_set_word_wrap (gtk:GTK_TEXT text)
			      (ff:fslot-value-typed
			       'gtk:GtkToggleButton nil
			       (gtk:GTK_TOGGLE_BUTTON checkbutton)
			       'gtk::active)))

(ff:defun-foreign-callable close-application ((widget (* gtk:GtkWidget))
					      (data gtk:gpointer))
  (declare (ignore widget data))
  #+original (gtk:gtk_main_quit)
  #-original (gtk:gtk-main-quit))

(defun text ()
  (let ((window nil)
	(box1 nil)
	(box2 nil)
	(hbox nil)
	(button nil)
	(check nil)
	(separator nil)
	(table nil)
	(vscrollbar nil)
	(text nil)
	(cmap nil)
	(color nil)
	(fixed-font nil))

    (gtk:gtk_init 0 0)

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:gtk_widget_set_usize window 600 500)
    (gtk:gtk_window_set_policy (gtk:GTK_WINDOW window) gtk:TRUE gtk:TRUE
			       gtk:FALSE)
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "destroy" 
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable 'close-application))
			    gtk:NULL)
    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "Text Widget Example")
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER window) 0)

    (setq box1 (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) box1)
    (gtk:gtk_widget_show box1)

    (setq box2 (gtk:gtk_vbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq table (gtk:gtk_table_new 2 2 gtk:FALSE))
    (gtk:gtk_table_set_row_spacing (gtk:GTK_TABLE table) 0 2)
    (gtk:gtk_table_set_col_spacing (gtk:GTK_TABLE table) 0 2)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) table gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show table)

    (setq text (gtk:gtk_text_new gtk:NULL gtk:NULL))
    (gtk:gtk_text_set_editable (gtk:GTK_TEXT text) gtk:TRUE)
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) text 0 1 0 1
			  (logior gtk:GTK_EXPAND gtk:GTK_SHRINK gtk:GTK_FILL)
			  (logior gtk:GTK_EXPAND gtk:GTK_SHRINK gtk:GTK_FILL)
			  0 0)
    (gtk:gtk_widget_show text)

    (setq vscrollbar (gtk:gtk_vscrollbar_new
		      (ff:fslot-value-typed 'gtk:GtkText nil
					    (gtk:GTK_TEXT text)
					    'gtk::vadj)))
    (gtk:gtk_table_attach (gtk:GTK_TABLE table) vscrollbar 1 2 0 1 
			  gtk:GTK_FILL
			  (logior gtk:GTK_EXPAND  gtk:GTK_SHRINK gtk:GTK_FILL)
			  0 0)
    (gtk:gtk_widget_show vscrollbar)

    (setq cmap (gtk:gdk_colormap_get_system))
    (setq color (ff:allocate-fobject 'gtk:GdkColor
				     :foreign-static-gc))
    (setf (ff:fslot-value-typed 'gtk:GdkColor nil color 'gtk::red) #xffff)
    (setf (ff:fslot-value-typed 'gtk:GdkColor nil color 'gtk::green) 0)
    (setf (ff:fslot-value-typed 'gtk:GdkColor nil color 'gtk::blue) 0)
    (when (eql gtk:NULL (gtk:gdk_color_alloc cmap color))
      (error "couldn't allocate color"))

    (setq fixed-font (gtk:gdk_font_load
		      "-misc-fixed-medium-r-*-*-*-140-*-*-*-*-*-*"))

    (gtk:gtk_widget_realize text)

    (gtk:gtk_text_freeze (gtk:GTK_TEXT text))

    (gtk:gtk_text_insert (gtk:GTK_TEXT text) gtk:NULL
			 (ff:fslot-value-typed
			  'gtk:GtkStyle nil
			  (ff:fslot-value-typed 'gtk:GtkWidget nil
						text 'gtk::style)
			  'gtk::black)
			 gtk:NULL 
			 "Supports "
			 -1)
    (gtk:gtk_text_insert (gtk:GTK_TEXT text) gtk:NULL color gtk:NULL 
			 "colored " -1)
    (gtk:gtk_text_insert (gtk:GTK_TEXT text) gtk:NULL
			 (ff:fslot-address-typed
			  'gtk:GdkColor nil
			  (ff:fslot-value-typed
			   'gtk:GtkStyle nil
			   (ff:fslot-value-typed 'gtk:GtkWidget nil
						 text 'gtk::style)
			   'gtk::black))
			 gtk:NULL 
			 "text and different "
			 -1)
    (gtk:gtk_text_insert (gtk:GTK_TEXT text) fixed-font
			 (ff:fslot-address-typed
			  'gtk:GdkColor nil
			  (ff:fslot-value-typed
			   'gtk:GtkStyle nil
			   (ff:fslot-value-typed 'gtk:GtkWidget nil
						 text 'gtk::style)
			   'gtk::black))
			 gtk:NULL 
			 #.(format nil "fonts~2%")
			 -1)

    (with-open-file (infile
		     #+original "15.04-text.cl"
		     #-original "/etc/printcap"
		     :direction :input)
      (let* ((buffer (make-array (file-length infile)
				 :element-type '(unsigned-byte 8)))
	     (nchars (read-sequence buffer infile)))
	(gtk:gtk_text_insert (gtk:GTK_TEXT text) fixed-font gtk:NULL gtk:NULL
			     buffer nchars)))

    (gtk:gtk_text_thaw (gtk:GTK_TEXT text))

    (setq hbox (gtk:gtk_hbutton_box_new))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) hbox gtk:FALSE gtk:FALSE 0)
    (gtk:gtk_widget_show hbox)

    (setq check (gtk:gtk_check_button_new_with_label "Editable"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) check gtk:FALSE gtk:FALSE 0)
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT check) "toggled" 
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable
			      'text-toggle-editable))
			    text)
    (gtk:gtk_toggle_button_set_active (gtk:GTK_TOGGLE_BUTTON check) gtk:TRUE)
    (gtk:gtk_widget_show check)
    (setq check (gtk:gtk_check_button_new_with_label "Wrap Words"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) check gtk:FALSE gtk:TRUE 0)
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT check) "toggled" 
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable
			      'text-toggle-word-wrap))
			    text)
    (gtk:gtk_toggle_button_set_active (gtk:GTK_TOGGLE_BUTTON check) gtk:FALSE)
    (gtk:gtk_widget_show check)

    (setq separator (gtk:gtk_hseparator_new))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) separator gtk:FALSE gtk:TRUE 0)
    (gtk:gtk_widget_show separator)

    (setq box2 (gtk:gtk_vbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq button (gtk:gtk_button_new_with_label "close"))
    (gtk:gtk_signal_connect (gtk:GTK_OBJECT button) "clicked" 
			    (gtk:GTK_SIGNAL_FUNC
			     (ff:register-foreign-callable
			      'close-application) )
			    gtk:NULL)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) button gtk:TRUE gtk:TRUE 0)
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
  (run-example "15.04-text" #'text))
