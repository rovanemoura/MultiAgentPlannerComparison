;
;#include <stdio.h>
;#include <gtk/gtk.h>
;
;static gint button_press (GtkWidget *, GdkEvent *);
;static void menuitem_response (gchar *);
;
;int main( int   argc,
;	   char *argv[] )
;{
;
;    GtkWidget *window;
;    GtkWidget *menu;
;    GtkWidget *menu_bar;
;    GtkWidget *root_menu;
;    GtkWidget *menu_items;
;    GtkWidget *vbox;
;    GtkWidget *button;
;    char buf[128];
;    int i;
;
;    gtk_init (&argc, &argv);
;
;    /* create a new window */
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;    gtk_widget_set_size_request (GTK_WIDGET (window), 200, 100);
;    gtk_window_set_title (GTK_WINDOW (window), "GTK Menu Test");
;    g_signal_connect (G_OBJECT (window), "delete_event",
;		       G_CALLBACK (gtk_main_quit), NULL);
;
;    /* Init the menu-widget, and remember -- never
;     * gtk_show_widget() the menu widget!! 
;     * This is the menu that holds the menu items, the one that
;     * will pop up when you click on the "Root Menu" in the app */
;    menu = gtk_menu_new ();
;
;    /* Next we make a little loop that makes three menu-entries for "test-menu".
;     * Notice the call to gtk_menu_append.  Here we are adding a list of
;     * menu items to our menu.  Normally, we'd also catch the "clicked"
;     * signal on each of the menu items and setup a callback for it,
;     * but it's omitted here to save space. */
;
;    for (i = 0; i < 3; i++)
;	 {
;	     /* Copy the names to the buf. */
;	     sprintf (buf, "Test-undermenu - %d", i);
;
;	     /* Create a new menu-item with a name... */
;	     menu_items = gtk_menu_item_new_with_label (buf);
;
;	     /* ...and add it to the menu. */
;	     gtk_menu_shell_append (GTK_MENU_SHELL (menu), menu_items);
;
;	     /* Do something interesting when the menuitem is selected */
;	     g_signal_connect_swapped (G_OBJECT (menu_items), "activate",
;				       G_CALLBACK (menuitem_response), 
;				       g_strdup (buf));
;
;	     /* Show the widget */
;	     gtk_widget_show (menu_items);
;	 }
;
;    /* This is the root menu, and will be the label
;     * displayed on the menu bar.  There won't be a signal handler attached,
;     * as it only pops up the rest of the menu when pressed. */
;    root_menu = gtk_menu_item_new_with_label ("Root Menu");
;
;    gtk_widget_show (root_menu);
;
;    /* Now we specify that we want our newly created "menu" to be the menu
;     * for the "root menu" */
;    gtk_menu_item_set_submenu (GTK_MENU_ITEM (root_menu), menu);
;
;    /* A vbox to put a menu and a button in: */
;    vbox = gtk_vbox_new (FALSE, 0);
;    gtk_container_add (GTK_CONTAINER (window), vbox);
;    gtk_widget_show (vbox);
;
;    /* Create a menu-bar to hold the menus and add it to our main window */
;    menu_bar = gtk_menu_bar_new ();
;    gtk_box_pack_start (GTK_BOX (vbox), menu_bar, FALSE, FALSE, 2);
;    gtk_widget_show (menu_bar);
;
;    /* Create a button to which to attach menu as a popup */
;    button = gtk_button_new_with_label ("press me");
;    g_signal_connect_swapped (G_OBJECT (button), "event",
;			       G_CALLBACK (button_press), 
;			       menu);
;    gtk_box_pack_end (GTK_BOX (vbox), button, TRUE, TRUE, 2);
;    gtk_widget_show (button);
;
;    /* And finally we append the menu-item to the menu-bar -- this is the
;     * "root" menu-item I have been raving about =) */
;    gtk_menu_shell_append (GTK_MENU_SHELL (menu_bar), root_menu);
;
;    /* always display the window as the last step so it all splashes on
;     * the screen at once. */
;    gtk_widget_show (window);
;
;    gtk_main ();
;
;    return 0;
;}
;
;/* Respond to a button-press by posting a menu passed in as widget.
; *
; * Note that the "widget" argument is the menu being posted, NOT
; * the button that was pressed.
; */
;
;static gint button_press( GtkWidget *widget,
;			   GdkEvent *event )
;{
;
;    if (event->type == GDK_BUTTON_PRESS) {
;	 GdkEventButton *bevent = (GdkEventButton *) event; 
;	 gtk_menu_popup (GTK_MENU (widget), NULL, NULL, NULL, NULL,
;			 bevent->button, bevent->time);
;	 /* Tell calling code that we have handled this event; the buck
;	  * stops here. */
;	 return TRUE;
;    }
;
;    /* Tell calling code that we have not handled this event; pass it on. */
;    return FALSE;
;}
;
;
;/* Print a string when a menu item is selected */
;
;static void menuitem_response( gchar *string )
;{
;    printf ("%s\n", string);
;}

(defpackage "11.02-menu" (:use :excl :common-lisp))
(in-package "11.02-menu")

(ff:defun-foreign-callable cb-gtk-main-quit ()
  (gtk:gtk-main-quit))

(defun menu ()
  (let ((window nil)
	(menu nil)
	(menu-bar nil)
	(root-menu nil)
	(menu-items nil)
	(vbox nil)
	(button nil)
	(buf nil))

    (gtk:gtk_init 0 0)
    
    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:gtk_widget_set_size_request (gtk:GTK_WIDGET window)  200  100)
    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window)  "GTK Menu Test")
    (gtk:g_signal_connect (gtk:G_OBJECT window) "delete_event" 
			  (gtk:G_CALLBACK
			   #+original (ff:get-entry-point "gtk_main_quit")
			   #-original (ff:register-foreign-callable
				       'cb-gtk-main-quit))
			  gtk:NULL)

    (setq menu (gtk:gtk_menu_new))

    (dotimes (i 3)
      (setq buf (format nil "Test-undermenu - ~d" i))
      (setq menu-items (gtk:gtk_menu_item_new_with_label buf))

      (gtk:gtk_menu_shell_append (gtk:GTK_MENU_SHELL menu) menu-items)

      (gtk:g_signal_connect_swapped (gtk:G_OBJECT menu-items) "activate" 
				    (gtk:G_CALLBACK
				     (ff:register-foreign-callable
				      'menuitem-response))
				    (string-to-native
				     buf
				     :external-format
				     gtk:gpointer-to-string-ef))
      (gtk:gtk_widget_show menu-items))

    (setq root-menu (gtk:gtk_menu_item_new_with_label "Root Menu"))

    (gtk:gtk_widget_show root-menu)

    (gtk:gtk_menu_item_set_submenu (gtk:GTK_MENU_ITEM root-menu) menu)

    (setq vbox (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) vbox)
    (gtk:gtk_widget_show vbox)

    (setq menu-bar (gtk:gtk_menu_bar_new))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) menu-bar gtk:FALSE gtk:FALSE 2)
    (gtk:gtk_widget_show menu-bar)

    (setq button (gtk:gtk_button_new_with_label "press me"))
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT button)  "event" 
				  (gtk:G_CALLBACK
				   (ff:register-foreign-callable
				    'button-press))
				  menu)
    (gtk:gtk_box_pack_end (gtk:GTK_BOX vbox)  button  gtk:TRUE  gtk:TRUE  2)
    (gtk:gtk_widget_show button)

    (gtk:gtk_menu_shell_append (gtk:GTK_MENU_SHELL menu-bar) root-menu)

    (gtk:gtk_widget_show window)

    #+original (gtk:gtk_main)
    #-original (gtk:gtk-main)))

(ff:defun-foreign-callable button-press ((widget (* gtk:GtkWidget))
					 (event (* gtk:GdkEvent)))
  (when (eql (ff:fslot-value-typed 'gtk:GdkEvent nil event 'gtk::type)
	     gtk:GDK_BUTTON_PRESS)
    (let ((bevent event))
      (gtk:gtk_menu_popup (gtk:GTK_MENU widget) gtk:NULL gtk:NULL gtk:NULL
			  gtk:NULL
			  (ff:fslot-value-typed 'gtk:GdkEventButton nil
						bevent 'gtk::button)
			  (ff:fslot-value-typed 'gtk:GdkEventButton nil
						bevent 'gtk::time))
      (return-from button-press gtk:TRUE)))

  gtk:FALSE)

(ff:defun-foreign-callable menuitem-response ((string (* gtk:gchar)))
  (format t "~s~%" (native-to-string
		    string
		    :external-format gtk:gpointer-to-string-ef))
  (values))


(flet ((run-example (name function)
	 ;; workaround for bogus (imo) redef. warnings generated by defvar
	 (declare (special gtk::*run-example*))
	 (unless (boundp 'gtk::*run-example*)
	   (setq gtk::*run-example* t))
	 (when gtk::*run-example*
	   (mp:process-run-function
	    (format nil "GTK+ Example: ~a" name)
	    function))))
  (run-example "11.02-menu" #'menu))
