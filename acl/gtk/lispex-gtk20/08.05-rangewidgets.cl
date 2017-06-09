;
;#include <gtk/gtk.h>
;
;GtkWidget *hscale, *vscale;
;
;void cb_pos_menu_select( GtkWidget       *item,
;			  GtkPositionType  pos )
;{
;    /* Set the value position on both scale widgets */
;    gtk_scale_set_value_pos (GTK_SCALE (hscale), pos);
;    gtk_scale_set_value_pos (GTK_SCALE (vscale), pos);
;}
;
;void cb_update_menu_select( GtkWidget     *item,
;			     GtkUpdateType  policy )
;{
;    /* Set the update policy for both scale widgets */
;    gtk_range_set_update_policy (GTK_RANGE (hscale), policy);
;    gtk_range_set_update_policy (GTK_RANGE (vscale), policy);
;}
;
;void cb_digits_scale( GtkAdjustment *adj )
;{
;    /* Set the number of decimal places to which adj->value is rounded */
;    gtk_scale_set_digits (GTK_SCALE (hscale), (gint) adj->value);
;    gtk_scale_set_digits (GTK_SCALE (vscale), (gint) adj->value);
;}
;
;void cb_page_size( GtkAdjustment *get,
;		    GtkAdjustment *set )
;{
;    /* Set the page size and page increment size of the sample
;     * adjustment to the value specified by the "Page Size" scale */
;    set->page_size = get->value;
;    set->page_increment = get->value;
;
;    /* This sets the adjustment and makes it emit the "changed" signal to 
;	reconfigure all the widgets that are attached to this signal.  */
;    gtk_adjustment_set_value (set, CLAMP (set->value,
;					   set->lower,
;					   (set->upper - set->page_size)));
;}
;
;void cb_draw_value( GtkToggleButton *button )
;{
;    /* Turn the value display on the scale widgets off or on depending
;     *  on the state of the checkbutton */
;    gtk_scale_set_draw_value (GTK_SCALE (hscale), button->active);
;    gtk_scale_set_draw_value (GTK_SCALE (vscale), button->active);  
;}
;
;/* Convenience functions */
;
;GtkWidget *make_menu_item (gchar     *name,
;			    GCallback  callback,
;			    gpointer   data)
;{
;    GtkWidget *item;
;  
;    item = gtk_menu_item_new_with_label (name);
;    g_signal_connect (G_OBJECT (item), "activate",
;		       callback, data);
;    gtk_widget_show (item);
;
;    return item;
;}
;
;void scale_set_default_values( GtkScale *scale )
;{
;    gtk_range_set_update_policy (GTK_RANGE (scale),
;				  GTK_UPDATE_CONTINUOUS);
;    gtk_scale_set_digits (scale, 1);
;    gtk_scale_set_value_pos (scale, GTK_POS_TOP);
;    gtk_scale_set_draw_value (scale, TRUE);
;}
;
;/* makes the sample window */
;
;void create_range_controls( void )
;{
;    GtkWidget *window;
;    GtkWidget *box1, *box2, *box3;
;    GtkWidget *button;
;    GtkWidget *scrollbar;
;    GtkWidget *separator;
;    GtkWidget *opt, *menu, *item;
;    GtkWidget *label;
;    GtkWidget *scale;
;    GtkObject *adj1, *adj2;
;
;    /* Standard window-creating stuff */
;    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;    g_signal_connect (G_OBJECT (window), "destroy",
;		       G_CALLBACK (gtk_main_quit),
;		       NULL);
;    gtk_window_set_title (GTK_WINDOW (window), "range controls");
;
;    box1 = gtk_vbox_new (FALSE, 0);
;    gtk_container_add (GTK_CONTAINER (window), box1);
;    gtk_widget_show (box1);
;
;    box2 = gtk_hbox_new (FALSE, 10);
;    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
;    gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
;    gtk_widget_show (box2);
;
;    /* value, lower, upper, step_increment, page_increment, page_size */
;    /* Note that the page_size value only makes a difference for
;     * scrollbar widgets, and the highest value you'll get is actually
;     * (upper - page_size). */
;    adj1 = gtk_adjustment_new (0.0, 0.0, 101.0, 0.1, 1.0, 1.0);
;  
;    vscale = gtk_vscale_new (GTK_ADJUSTMENT (adj1));
;    scale_set_default_values (GTK_SCALE (vscale));
;    gtk_box_pack_start (GTK_BOX (box2), vscale, TRUE, TRUE, 0);
;    gtk_widget_show (vscale);
;
;    box3 = gtk_vbox_new (FALSE, 10);
;    gtk_box_pack_start (GTK_BOX (box2), box3, TRUE, TRUE, 0);
;    gtk_widget_show (box3);
;
;    /* Reuse the same adjustment */
;    hscale = gtk_hscale_new (GTK_ADJUSTMENT (adj1));
;    gtk_widget_set_size_request (GTK_WIDGET (hscale), 200, -1);
;    scale_set_default_values (GTK_SCALE (hscale));
;    gtk_box_pack_start (GTK_BOX (box3), hscale, TRUE, TRUE, 0);
;    gtk_widget_show (hscale);
;
;    /* Reuse the same adjustment again */
;    scrollbar = gtk_hscrollbar_new (GTK_ADJUSTMENT (adj1));
;    /* Notice how this causes the scales to always be updated
;     * continuously when the scrollbar is moved */
;    gtk_range_set_update_policy (GTK_RANGE (scrollbar), 
;				  GTK_UPDATE_CONTINUOUS);
;    gtk_box_pack_start (GTK_BOX (box3), scrollbar, TRUE, TRUE, 0);
;    gtk_widget_show (scrollbar);
;
;    box2 = gtk_hbox_new (FALSE, 10);
;    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
;    gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
;    gtk_widget_show (box2);
;
;    /* A checkbutton to control whether the value is displayed or not */
;    button = gtk_check_button_new_with_label("Display value on scale widgets");
;    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
;    g_signal_connect (G_OBJECT (button), "toggled",
;		       G_CALLBACK (cb_draw_value), NULL);
;    gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
;    gtk_widget_show (button);
;  
;    box2 = gtk_hbox_new (FALSE, 10);
;    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
;
;    /* An option menu to change the position of the value */
;    label = gtk_label_new ("Scale Value Position:");
;    gtk_box_pack_start (GTK_BOX (box2), label, FALSE, FALSE, 0);
;    gtk_widget_show (label);
;  
;    opt = gtk_option_menu_new ();
;    menu = gtk_menu_new ();
;
;    item = make_menu_item ("Top",
;			    G_CALLBACK (cb_pos_menu_select),
;			    GINT_TO_POINTER (GTK_POS_TOP));
;    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
;  
;    item = make_menu_item ("Bottom", G_CALLBACK (cb_pos_menu_select), 
;			    GINT_TO_POINTER (GTK_POS_BOTTOM));
;    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
;  
;    item = make_menu_item ("Left", G_CALLBACK (cb_pos_menu_select),
;			    GINT_TO_POINTER (GTK_POS_LEFT));
;    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
;  
;    item = make_menu_item ("Right", G_CALLBACK (cb_pos_menu_select),
;			    GINT_TO_POINTER (GTK_POS_RIGHT));
;    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
;  
;    gtk_option_menu_set_menu (GTK_OPTION_MENU (opt), menu);
;    gtk_box_pack_start (GTK_BOX (box2), opt, TRUE, TRUE, 0);
;    gtk_widget_show (opt);
;
;    gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
;    gtk_widget_show (box2);
;
;    box2 = gtk_hbox_new (FALSE, 10);
;    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
;
;    /* Yet another option menu, this time for the update policy of the
;     * scale widgets */
;    label = gtk_label_new ("Scale Update Policy:");
;    gtk_box_pack_start (GTK_BOX (box2), label, FALSE, FALSE, 0);
;    gtk_widget_show (label);
;  
;    opt = gtk_option_menu_new ();
;    menu = gtk_menu_new ();
;  
;    item = make_menu_item ("Continuous",
;			    G_CALLBACK (cb_update_menu_select),
;			    GINT_TO_POINTER (GTK_UPDATE_CONTINUOUS));
;    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
;  
;    item = make_menu_item ("Discontinuous",
;			    G_CALLBACK (cb_update_menu_select),
;			    GINT_TO_POINTER (GTK_UPDATE_DISCONTINUOUS));
;    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
;  
;    item = make_menu_item ("Delayed",
;			    G_CALLBACK (cb_update_menu_select),
;			    GINT_TO_POINTER (GTK_UPDATE_DELAYED));
;    gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
;  
;    gtk_option_menu_set_menu (GTK_OPTION_MENU (opt), menu);
;    gtk_box_pack_start (GTK_BOX (box2), opt, TRUE, TRUE, 0);
;    gtk_widget_show (opt);
;  
;    gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
;    gtk_widget_show (box2);
;
;    box2 = gtk_hbox_new (FALSE, 10);
;    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
;  
;    /* An HScale widget for adjusting the number of digits on the
;     * sample scales. */
;    label = gtk_label_new ("Scale Digits:");
;    gtk_box_pack_start (GTK_BOX (box2), label, FALSE, FALSE, 0);
;    gtk_widget_show (label);
;
;    adj2 = gtk_adjustment_new (1.0, 0.0, 5.0, 1.0, 1.0, 0.0);
;    g_signal_connect (G_OBJECT (adj2), "value_changed",
;		       G_CALLBACK (cb_digits_scale), NULL);
;    scale = gtk_hscale_new (GTK_ADJUSTMENT (adj2));
;    gtk_scale_set_digits (GTK_SCALE (scale), 0);
;    gtk_box_pack_start (GTK_BOX (box2), scale, TRUE, TRUE, 0);
;    gtk_widget_show (scale);
;
;    gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
;    gtk_widget_show (box2);
;  
;    box2 = gtk_hbox_new (FALSE, 10);
;    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
;  
;    /* And, one last HScale widget for adjusting the page size of the
;     * scrollbar. */
;    label = gtk_label_new ("Scrollbar Page Size:");
;    gtk_box_pack_start (GTK_BOX (box2), label, FALSE, FALSE, 0);
;    gtk_widget_show (label);
;
;    adj2 = gtk_adjustment_new (1.0, 1.0, 101.0, 1.0, 1.0, 0.0);
;    g_signal_connect (G_OBJECT (adj2), "value_changed",
;		       G_CALLBACK (cb_page_size), adj1);
;    scale = gtk_hscale_new (GTK_ADJUSTMENT (adj2));
;    gtk_scale_set_digits (GTK_SCALE (scale), 0);
;    gtk_box_pack_start (GTK_BOX (box2), scale, TRUE, TRUE, 0);
;    gtk_widget_show (scale);
;
;    gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
;    gtk_widget_show (box2);
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
;    button = gtk_button_new_with_label ("Quit");
;    g_signal_connect_swapped (G_OBJECT (button), "clicked",
;			       G_CALLBACK (gtk_main_quit),
;			       NULL);
;    gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
;    GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
;    gtk_widget_grab_default (button);
;    gtk_widget_show (button);
;
;    gtk_widget_show (window);
;}
;
;int main( int   argc,
;	   char *argv[] )
;{
;    gtk_init (&argc, &argv);
;
;    create_range_controls ();
;
;    gtk_main ();
;
;    return 0;
;}
;

(defpackage "08.05-rangewidgets" (:use :excl :common-lisp))
(in-package "08.05-rangewidgets")

(defvar hscale gtk:NULL)
(defvar vscale gtk:NULL)

(ff:defun-foreign-callable cb-pos-menu-select ((item (* gtk:GtkWidget))
					       (pos gtk:GtkPositionType))
  (declare (ignore item))
  (gtk:gtk_scale_set_value_pos (gtk:GTK_SCALE hscale) pos)
  (gtk:gtk_scale_set_value_pos (gtk:GTK_SCALE vscale) pos)
  (values))

(ff:defun-foreign-callable cb-update-menu-select ((item (* gtk:GtkWidget))
						  (policy gtk:GtkUpdateType))
  (declare (ignore item))
  (gtk:gtk_range_set_update_policy (gtk:GTK_RANGE hscale) policy)
  (gtk:gtk_range_set_update_policy (gtk:GTK_RANGE vscale) policy)
  (values))

(ff:defun-foreign-callable cb-digits-scale ((adj (* gtk:GtkAdjustment)))
  (gtk:gtk_scale_set_digits
   (gtk:GTK_SCALE hscale)
   (floor (ff:fslot-value-typed 'gtk:GtkAdjustment nil adj 'gtk::value)))
  (gtk:gtk_scale_set_digits
   (gtk:GTK_SCALE vscale)
   (floor (ff:fslot-value-typed 'gtk:GtkAdjustment nil adj 'gtk::value)))
  (values))

(ff:defun-foreign-callable cb-page-size ((get (* gtk:GtkAdjustment))
					 (set (* gtk:GtkAdjustment)))
  (setf (ff:fslot-value-typed 'gtk:GtkAdjustment nil set 'gtk::page_size)
    (ff:fslot-value-typed 'gtk:GtkAdjustment nil get 'gtk::value))
  (setf (ff:fslot-value-typed 'gtk:GtkAdjustment nil set 'gtk::page_increment)
    (ff:fslot-value-typed 'gtk:GtkAdjustment nil get 'gtk::value))
  (gtk:gtk_adjustment_set_value
   set
   (gtk:CLAMP
    (ff:fslot-value-typed 'gtk:GtkAdjustment nil set 'gtk::value)
    (ff:fslot-value-typed 'gtk:GtkAdjustment nil set 'gtk::lower)
    (-
     (ff:fslot-value-typed 'gtk:GtkAdjustment nil set 'gtk::upper)
     (ff:fslot-value-typed 'gtk:GtkAdjustment nil set 'gtk::page_size))))
  (values))

(ff:defun-foreign-callable cb-draw-value ((button (* gtk:GtkToggleButton)))
  (gtk:gtk_scale_set_draw_value
   (gtk:GTK_SCALE hscale)
   (ff:fslot-value-typed 'gtk:GtkToggleButton nil button 'gtk::active))
  (gtk:gtk_scale_set_draw_value
   (gtk:GTK_SCALE vscale)
   (ff:fslot-value-typed 'gtk:GtkToggleButton nil button 'gtk::active))
  (values))

(ff:defun-foreign-callable cb-gtk-main-quit ()
  (gtk:gtk-main-quit))

(defun make-menu-item (name callback data)
  (let ((item (gtk:gtk_menu_item_new_with_label name)))
    (gtk:g_signal_connect (gtk:G_OBJECT item) "activate"
			  callback data)
    (gtk:gtk_widget_show item)
    item))

(defun scale-set-default-values (scale)
  (gtk:gtk_range_set_update_policy (gtk:GTK_RANGE scale)
				   gtk:GTK_UPDATE_CONTINUOUS)
  (gtk:gtk_scale_set_digits scale 1)
  (gtk:gtk_scale_set_value_pos scale gtk:GTK_POS_TOP)
  (gtk:gtk_scale_set_draw_value scale gtk:TRUE))

(defun create-range-controls ()
  (let ((window nil)
	(box1 nil)
	(box2 nil)
	(box3 nil)
	(button nil)
	(scrollbar nil)
	(separator nil)
	(opt nil)
	(menu nil)
	(item nil)
	(label nil)
	(scale nil)
	(adj1 nil)
	(adj2 nil)

	(cb-pos-menu-select-cb (ff:register-foreign-callable
				'cb-pos-menu-select
				:reuse))
	(cb-update-menu-select-cb (ff:register-foreign-callable
				   'cb-update-menu-select
				   :reuse))
	(cb-digits-scale-cb (ff:register-foreign-callable
			     'cb-digits-scale
			     :reuse))
	(cb-page-size-cb (ff:register-foreign-callable
			  'cb-page-size
			  :reuse)))

    (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
    (gtk:g_signal_connect (gtk:G_OBJECT window) "destroy"
			  (gtk:G_CALLBACK
			   #+original (ff:get-entry-point "gtk_main_quit")
			   #-original (ff:register-foreign-callable
				       'cb-gtk-main-quit))
			  gtk:NULL)
    (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "range controls")
    
    (setq box1 (gtk:gtk_vbox_new gtk:FALSE 0))
    (gtk:gtk_container_add (gtk:GTK_CONTAINER window) box1)
    (gtk:gtk_widget_show box1)
    
    (setq box2 (gtk:gtk_hbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq adj1 (gtk:gtk_adjustment_new 0.0d0 0.0d0 101.0d0 0.1d0 1.0d0 1.0d0))
    
    (setq vscale (gtk:gtk_vscale_new (gtk:GTK_ADJUSTMENT adj1)))
    (scale-set-default-values (gtk:GTK_SCALE vscale))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) vscale gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show vscale)
    
    (setq box3 (gtk:gtk_vbox_new gtk:FALSE 10))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) box3 gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show box3)
    
    (setq hscale (gtk:gtk_hscale_new (gtk:GTK_ADJUSTMENT adj1)))
    (gtk:gtk_widget_set_size_request (gtk:GTK_WIDGET hscale) 200 -1)
    (scale-set-default-values (gtk:GTK_SCALE hscale))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box3) hscale gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show hscale)

    (setq scrollbar (gtk:gtk_hscrollbar_new (gtk:GTK_ADJUSTMENT adj1)))
    (gtk:gtk_range_set_update_policy (gtk:GTK_RANGE scrollbar)
				     gtk:GTK_UPDATE_CONTINUOUS)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box3) scrollbar gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show scrollbar)
    
    (setq box2 (gtk:gtk_hbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq button (gtk:gtk_check_button_new_with_label
		  "Display value on scale widgets"))

    (gtk:gtk_toggle_button_set_active (gtk:GTK_TOGGLE_BUTTON button) gtk:TRUE)
    (gtk:g_signal_connect (gtk:G_OBJECT button) "toggled"
			  (gtk:G_CALLBACK
			   (ff:register-foreign-callable 'cb-draw-value))
			  gtk:NULL)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) button gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show button)

    (setq box2 (gtk:gtk_hbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)

    (setq label (gtk:gtk_label_new "Scale Value Position:"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) label gtk:FALSE gtk:FALSE 0)
    (gtk:gtk_widget_show label)

    (setq opt (gtk:gtk_option_menu_new))
    (setq menu (gtk:gtk_menu_new))

    (setq item (make-menu-item "Top"
			       (gtk:G_CALLBACK cb-pos-menu-select-cb)
			       (gtk:GINT_TO_POINTER gtk:GTK_POS_TOP)))
    (gtk:gtk_menu_shell_append (gtk:GTK_MENU menu) item)

    (setq item (make-menu-item "Bottom"
			       (gtk:G_CALLBACK cb-pos-menu-select-cb)
			       (gtk:GINT_TO_POINTER gtk:GTK_POS_BOTTOM)))
    (gtk:gtk_menu_shell_append (gtk:GTK_MENU menu) item)

    (setq item (make-menu-item "Left"
			       (gtk:G_CALLBACK cb-pos-menu-select-cb)
			       (gtk:GINT_TO_POINTER gtk:GTK_POS_LEFT)))
    (gtk:gtk_menu_shell_append (gtk:GTK_MENU menu) item)

    (setq item (make-menu-item "Right"
			       (gtk:G_CALLBACK cb-pos-menu-select-cb)
			       (gtk:GINT_TO_POINTER gtk:GTK_POS_RIGHT)))
    (gtk:gtk_menu_shell_append (gtk:GTK_MENU menu) item)

    (gtk:gtk_option_menu_set_menu (gtk:GTK_OPTION_MENU opt) menu)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) opt gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show opt)

    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq box2 (gtk:gtk_hbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)

    (setq label (gtk:gtk_label_new "Scale Update Policy:"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) label gtk:FALSE gtk:FALSE 0)
    (gtk:gtk_widget_show label)

    (setq opt (gtk:gtk_option_menu_new))
    (setq menu (gtk:gtk_menu_new))

    (setq item (make-menu-item "Continuous"
			       (gtk:G_CALLBACK cb-update-menu-select-cb)
			       (gtk:GINT_TO_POINTER
				gtk:GTK_UPDATE_CONTINUOUS)))
    (gtk:gtk_menu_shell_append (gtk:GTK_MENU menu) item)

    (setq item (make-menu-item "Discontinuous"
			       (gtk:G_CALLBACK cb-update-menu-select-cb)
			       (gtk:GINT_TO_POINTER
				gtk:GTK_UPDATE_DISCONTINUOUS)))
    (gtk:gtk_menu_shell_append (gtk:GTK_MENU menu) item)

    (setq item (make-menu-item "Delayed"
			       (gtk:G_CALLBACK cb-update-menu-select-cb)
			       (gtk:GINT_TO_POINTER
				gtk:GTK_UPDATE_DELAYED)))
    (gtk:gtk_menu_shell_append (gtk:GTK_MENU menu) item)

    (gtk:gtk_option_menu_set_menu (gtk:GTK_OPTION_MENU opt) menu)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) opt gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show opt)

    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq box2 (gtk:gtk_hbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)

    (setq label (gtk:gtk_label_new "Scale Digits:"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) label gtk:FALSE gtk:FALSE 0)
    (gtk:gtk_widget_show label)

    (setq adj2 (gtk:gtk_adjustment_new 1.0d0 0.0d0 5.0d0 1.0d0 1.0d0 0.0d0))
    (gtk:g_signal_connect (gtk:G_OBJECT adj2) "value_changed"
			  (gtk:G_CALLBACK cb-digits-scale-cb)
			  gtk:NULL)
    (setq scale (gtk:gtk_hscale_new (gtk:GTK_ADJUSTMENT adj2)))
    (gtk:gtk_scale_set_digits (gtk:GTK_SCALE scale) 0)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) scale gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show scale)

    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq box2 (gtk:gtk_hbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)

    (setq label (gtk:gtk_label_new "Scrollbar Page Size:"))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) label gtk:FALSE gtk:FALSE 0)
    (gtk:gtk_widget_show label)

    (setq adj2 (gtk:gtk_adjustment_new 1.0d0 1.0d0 101.0d0 1.0d0 1.0d0 0.0d0))
    (gtk:g_signal_connect (gtk:G_OBJECT adj2) "value_changed"
			  (gtk:G_CALLBACK cb-page-size-cb)
			  adj1)
    (setq scale (gtk:gtk_hscale_new (gtk:GTK_ADJUSTMENT adj2)))
    (gtk:gtk_scale_set_digits (gtk:GTK_SCALE scale) 0)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) scale gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show scale)

    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:TRUE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq separator (gtk:gtk_hseparator_new))
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) separator gtk:FALSE gtk:TRUE 0)
    (gtk:gtk_widget_show separator)

    (setq box2 (gtk:gtk_vbox_new gtk:FALSE 10))
    (gtk:gtk_container_set_border_width (gtk:GTK_CONTAINER box2) 10)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box1) box2 gtk:FALSE gtk:TRUE 0)
    (gtk:gtk_widget_show box2)

    (setq button (gtk:gtk_button_new_with_label "Quit"))
    (gtk:g_signal_connect_swapped (gtk:G_OBJECT button) "clicked"
				  (gtk:G_CALLBACK
				   #+original (ff:get-entry-point
					       "gtk_main_quit")
				   #-original (ff:register-foreign-callable
					       'cb-gtk-main-quit))
				  gtk:NULL)
    (gtk:gtk_box_pack_start (gtk:GTK_BOX box2) button gtk:TRUE gtk:TRUE 0)
    (gtk:GTK_WIDGET_SET_FLAGS button gtk:GTK_CAN_DEFAULT)
    (gtk:gtk_widget_grab_default button)
    (gtk:gtk_widget_show button)

    (gtk:gtk_widget_show window)))

(defun rangewidgets ()
  (gtk:gtk_init 0 0)
  (create-range-controls)
  #+original (gtk:gtk_main)
  #-original (gtk:gtk-main))


(flet ((run-example (name function)
	 ;; workaround for bogus (imo) redef. warnings generated by defvar
	 (declare (special gtk::*run-example*))
	 (unless (boundp 'gtk::*run-example*)
	   (setq gtk::*run-example* t))
	 (when gtk::*run-example*
	   (mp:process-run-function
	    (format nil "GTK+ Example: ~a" name)
	    function))))
  (run-example "08.05-rangewidgets" #'rangewidgets))
