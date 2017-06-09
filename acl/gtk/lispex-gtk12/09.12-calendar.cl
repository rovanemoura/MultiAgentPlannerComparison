;/* example-start calendar calendar.c */
;/*
; * Copyright (C) 1998 Cesar Miquel, Shawn T. Amundson, Mattias Grönlund
; * Copyright (C) 2000 Tony Gale
; *
; * This program is free software; you can redistribute it and/or modify
; * it under the terms of the GNU General Public License as published by
; * the Free Software Foundation; either version 2 of the License, or
; * (at your option) any later version.
; *
; * This program is distributed in the hope that it will be useful,
; * but WITHOUT ANY WARRANTY; without even the implied warranty of
; * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; * GNU General Public License for more details.
; *
; * You should have received a copy of the GNU General Public License
; * along with this program; if not, write to the Free Software
; * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; */
;
;#include <gtk/gtk.h>
;#include <stdio.h>
;#include <string.h>
;#include <time.h>
;
;#define DEF_PAD 10
;#define DEF_PAD_SMALL 5
;
;#define TM_YEAR_BASE 1900
;
;typedef struct _CalendarData {
;  GtkWidget *flag_checkboxes[5];
;  gboolean  settings[5];
;  gchar     *font;
;  GtkWidget *font_dialog;
;  GtkWidget *window;
;  GtkWidget *prev2_sig;
;  GtkWidget *prev_sig;
;  GtkWidget *last_sig;
;  GtkWidget *month;
;} CalendarData;
;
;enum {
;  calendar_show_header,
;  calendar_show_days,
;  calendar_month_change, 
;  calendar_show_week,
;  calendar_monday_first
;};
;
;/*
; * GtkCalendar
; */
;
;void calendar_date_to_string( CalendarData *data,
;			       char         *buffer,
;			       gint          buff_len )
;{
;  struct tm tm;
;  time_t time;
;
;  memset (&tm, 0, sizeof (tm));
;  gtk_calendar_get_date (GTK_CALENDAR(data->window),
;			  &tm.tm_year, &tm.tm_mon, &tm.tm_mday);
;  tm.tm_year -= TM_YEAR_BASE;
;  time = mktime(&tm);
;  strftime (buffer, buff_len-1, "%x", gmtime(&time));
;}
;
;void calendar_set_signal_strings( char         *sig_str,
;				   CalendarData *data)
;{
;  gchar *prev_sig;
;
;  gtk_label_get (GTK_LABEL (data->prev_sig), &prev_sig);
;  gtk_label_set (GTK_LABEL (data->prev2_sig), prev_sig);
;
;  gtk_label_get (GTK_LABEL (data->last_sig), &prev_sig);
;  gtk_label_set (GTK_LABEL (data->prev_sig), prev_sig);
;  gtk_label_set (GTK_LABEL (data->last_sig), sig_str);
;}
;
;void calendar_month_changed( GtkWidget    *widget,
;			      CalendarData *data )
;{
;  char buffer[256] = "month_changed: ";
;
;  calendar_date_to_string (data, buffer+15, 256-15);
;  calendar_set_signal_strings (buffer, data);
;}
;
;void calendar_day_selected( GtkWidget    *widget,
;			     CalendarData *data )
;{
;  char buffer[256] = "day_selected: ";
;
;  calendar_date_to_string (data, buffer+14, 256-14);
;  calendar_set_signal_strings (buffer, data);
;}
;
;void calendar_day_selected_double_click( GtkWidget    *widget,
;					  CalendarData *data )
;{
;  struct tm tm;
;  char buffer[256] = "day_selected_double_click: ";
;
;  calendar_date_to_string (data, buffer+27, 256-27);
;  calendar_set_signal_strings (buffer, data);
;
;  memset (&tm, 0, sizeof (tm));
;  gtk_calendar_get_date (GTK_CALENDAR(data->window),
;			  &tm.tm_year, &tm.tm_mon, &tm.tm_mday);
;  tm.tm_year -= TM_YEAR_BASE;
;
;  if(GTK_CALENDAR(data->window)->marked_date[tm.tm_mday-1] == 0) {
;    gtk_calendar_mark_day(GTK_CALENDAR(data->window),tm.tm_mday);
;  } else { 
;    gtk_calendar_unmark_day(GTK_CALENDAR(data->window),tm.tm_mday);
;  }
;}
;
;void calendar_prev_month( GtkWidget    *widget,
;			     CalendarData *data )
;{
;  char buffer[256] = "prev_month: ";
;
;  calendar_date_to_string (data, buffer+12, 256-12);
;  calendar_set_signal_strings (buffer, data);
;}
;
;void calendar_next_month( GtkWidget    *widget,
;			     CalendarData *data )
;{
;  char buffer[256] = "next_month: ";
;
;  calendar_date_to_string (data, buffer+12, 256-12);
;  calendar_set_signal_strings (buffer, data);
;}
;
;void calendar_prev_year( GtkWidget    *widget,
;			     CalendarData *data )
;{
;  char buffer[256] = "prev_year: ";
;
;  calendar_date_to_string (data, buffer+11, 256-11);
;  calendar_set_signal_strings (buffer, data);
;}
;
;void calendar_next_year( GtkWidget    *widget,
;			     CalendarData *data )
;{
;  char buffer[256] = "next_year: ";
;
;  calendar_date_to_string (data, buffer+11, 256-11);
;  calendar_set_signal_strings (buffer, data);
;}
;
;
;void calendar_set_flags( CalendarData *calendar )
;{
;  gint i;
;  gint options=0;
;  for (i=0;i<5;i++) 
;    if (calendar->settings[i])
;      {
;	 options=options + (1<<i);
;      }
;  if (calendar->window)
;    gtk_calendar_display_options (GTK_CALENDAR (calendar->window), options);
;}
;
;void calendar_toggle_flag( GtkWidget    *toggle,
;			    CalendarData *calendar )
;{
;  gint i;
;  gint j;
;  j=0;
;  for (i=0; i<5; i++)
;    if (calendar->flag_checkboxes[i] == toggle)
;      j = i;
;
;  calendar->settings[j]=!calendar->settings[j];
;  calendar_set_flags(calendar);
;  
;}
;
;void calendar_font_selection_ok( GtkWidget    *button,
;				  CalendarData *calendar )
;{
;  GtkStyle *style;
;  GdkFont  *font;
;
;  calendar->font = gtk_font_selection_dialog_get_font_name(
;			 GTK_FONT_SELECTION_DIALOG (calendar->font_dialog));
;  if (calendar->window)
;    {
;      font = gtk_font_selection_dialog_get_font(GTK_FONT_SELECTION_DIALOG(calendar->font_dialog));
;      if (font) 
;	 {
;	   style = gtk_style_copy (gtk_widget_get_style (calendar->window));
;	   gdk_font_unref (style->font);
;	   style->font = font;
;	   gdk_font_ref (style->font);
;	   gtk_widget_set_style (calendar->window, style);
;	 }
;    }
;}
;
;void calendar_select_font( GtkWidget    *button,
;			    CalendarData *calendar )
;{
;  GtkWidget *window;
;
;  if (!calendar->font_dialog) {
;    window = gtk_font_selection_dialog_new ("Font Selection Dialog");
;    g_return_if_fail(GTK_IS_FONT_SELECTION_DIALOG(window));
;    calendar->font_dialog = window;
;    
;    gtk_window_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);
;    
;    gtk_signal_connect (GTK_OBJECT (window), "destroy",
;			 GTK_SIGNAL_FUNC (gtk_widget_destroyed),
;			 &calendar->font_dialog);
;    
;    gtk_signal_connect (GTK_OBJECT (GTK_FONT_SELECTION_DIALOG (window)->ok_button),
;			 "clicked", GTK_SIGNAL_FUNC(calendar_font_selection_ok),
;			 calendar);
;    gtk_signal_connect_object (GTK_OBJECT (GTK_FONT_SELECTION_DIALOG (window)->cancel_button),
;				"clicked",
;				GTK_SIGNAL_FUNC (gtk_widget_destroy), 
;				GTK_OBJECT (calendar->font_dialog));
;  }
;  window=calendar->font_dialog;
;  if (!GTK_WIDGET_VISIBLE (window))
;    gtk_widget_show (window);
;  else
;    gtk_widget_destroy (window);
;
;}
;
;void create_calendar()
;{
;  GtkWidget *window;
;  GtkWidget *vbox, *vbox2, *vbox3;
;  GtkWidget *hbox;
;  GtkWidget *hbbox;
;  GtkWidget *calendar;
;  GtkWidget *toggle;
;  GtkWidget *button;
;  GtkWidget *frame;
;  GtkWidget *separator;
;  GtkWidget *label;
;  GtkWidget *bbox;
;  static CalendarData calendar_data;
;  gint i;
;  
;  struct {
;    char *label;
;  } flags[] =
;    {
;      { "Show Heading" },
;      { "Show Day Names" },
;      { "No Month Change" },
;      { "Show Week Numbers" },
;      { "Week Start Monday" }
;    };
;
;  
;  calendar_data.window = NULL;
;  calendar_data.font = NULL;
;  calendar_data.font_dialog = NULL;
;
;  for (i=0; i<5; i++) {
;    calendar_data.settings[i]=0;
;  }
;
;  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
;  gtk_window_set_title(GTK_WINDOW(window), "GtkCalendar Example");
;  gtk_container_border_width (GTK_CONTAINER (window), 5);
;  gtk_signal_connect(GTK_OBJECT(window), "destroy",
;		      GTK_SIGNAL_FUNC(gtk_main_quit),
;		      NULL);
;  gtk_signal_connect(GTK_OBJECT(window), "delete-event",
;		      GTK_SIGNAL_FUNC(gtk_false),
;		      NULL);
;
;  gtk_window_set_policy(GTK_WINDOW(window), FALSE, FALSE, TRUE);
;
;  vbox = gtk_vbox_new(FALSE, DEF_PAD);
;  gtk_container_add (GTK_CONTAINER (window), vbox);
;
;  /*
;   * The top part of the window, Calendar, flags and fontsel.
;   */
;
;  hbox = gtk_hbox_new(FALSE, DEF_PAD);
;  gtk_box_pack_start (GTK_BOX(vbox), hbox, TRUE, TRUE, DEF_PAD);
;  hbbox = gtk_hbutton_box_new();
;  gtk_box_pack_start(GTK_BOX(hbox), hbbox, FALSE, FALSE, DEF_PAD);
;  gtk_button_box_set_layout(GTK_BUTTON_BOX(hbbox), GTK_BUTTONBOX_SPREAD);
;  gtk_button_box_set_spacing(GTK_BUTTON_BOX(hbbox), 5);
;
;  /* Calendar widget */
;  frame = gtk_frame_new("Calendar");
;  gtk_box_pack_start(GTK_BOX(hbbox), frame, FALSE, TRUE, DEF_PAD);
;  calendar=gtk_calendar_new();
;  calendar_data.window = calendar;
;  calendar_set_flags(&calendar_data);
;  gtk_calendar_mark_day ( GTK_CALENDAR(calendar), 19);	
;  gtk_container_add( GTK_CONTAINER( frame), calendar);
;  gtk_signal_connect (GTK_OBJECT (calendar), "month_changed", 
;		       GTK_SIGNAL_FUNC (calendar_month_changed),
;		       &calendar_data);
;  gtk_signal_connect (GTK_OBJECT (calendar), "day_selected", 
;		       GTK_SIGNAL_FUNC (calendar_day_selected),
;		       &calendar_data);
;  gtk_signal_connect (GTK_OBJECT (calendar), "day_selected_double_click", 
;		       GTK_SIGNAL_FUNC (calendar_day_selected_double_click),
;		       &calendar_data);
;  gtk_signal_connect (GTK_OBJECT (calendar), "prev_month", 
;		       GTK_SIGNAL_FUNC (calendar_prev_month),
;		       &calendar_data);
;  gtk_signal_connect (GTK_OBJECT (calendar), "next_month", 
;		       GTK_SIGNAL_FUNC (calendar_next_month),
;		       &calendar_data);
;  gtk_signal_connect (GTK_OBJECT (calendar), "prev_year", 
;		       GTK_SIGNAL_FUNC (calendar_prev_year),
;		       &calendar_data);
;  gtk_signal_connect (GTK_OBJECT (calendar), "next_year", 
;		       GTK_SIGNAL_FUNC (calendar_next_year),
;		       &calendar_data);
;
;
;  separator = gtk_vseparator_new ();
;  gtk_box_pack_start (GTK_BOX (hbox), separator, FALSE, TRUE, 0);
;
;  vbox2 = gtk_vbox_new(FALSE, DEF_PAD);
;  gtk_box_pack_start(GTK_BOX(hbox), vbox2, FALSE, FALSE, DEF_PAD);
;  
;  /* Build the Right frame with the flags in */ 
;
;  frame = gtk_frame_new("Flags");
;  gtk_box_pack_start(GTK_BOX(vbox2), frame, TRUE, TRUE, DEF_PAD);
;  vbox3 = gtk_vbox_new(TRUE, DEF_PAD_SMALL);
;  gtk_container_add(GTK_CONTAINER(frame), vbox3);
;
;  for (i = 0; i < 5; i++)
;    {
;      toggle = gtk_check_button_new_with_label(flags[i].label);
;      gtk_signal_connect (GTK_OBJECT (toggle),
;			     "toggled",
;			     GTK_SIGNAL_FUNC(calendar_toggle_flag),
;			     &calendar_data);
;      gtk_box_pack_start (GTK_BOX (vbox3), toggle, TRUE, TRUE, 0);
;      calendar_data.flag_checkboxes[i]=toggle;
;    }
;  /* Build the right font-button */ 
;  button = gtk_button_new_with_label("Font...");
;  gtk_signal_connect (GTK_OBJECT (button),
;		       "clicked",
;		       GTK_SIGNAL_FUNC(calendar_select_font),
;		       &calendar_data);
;  gtk_box_pack_start (GTK_BOX (vbox2), button, FALSE, FALSE, 0);
;
;  /*
;   *  Build the Signal-event part.
;   */
;
;  frame = gtk_frame_new("Signal events");
;  gtk_box_pack_start(GTK_BOX(vbox), frame, TRUE, TRUE, DEF_PAD);
;
;  vbox2 = gtk_vbox_new(TRUE, DEF_PAD_SMALL);
;  gtk_container_add(GTK_CONTAINER(frame), vbox2);
;  
;  hbox = gtk_hbox_new (FALSE, 3);
;  gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, TRUE, 0);
;  label = gtk_label_new ("Signal:");
;  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);
;  calendar_data.last_sig = gtk_label_new ("");
;  gtk_box_pack_start (GTK_BOX (hbox), calendar_data.last_sig, FALSE, TRUE, 0);
;
;  hbox = gtk_hbox_new (FALSE, 3);
;  gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, TRUE, 0);
;  label = gtk_label_new ("Previous signal:");
;  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);
;  calendar_data.prev_sig = gtk_label_new ("");
;  gtk_box_pack_start (GTK_BOX (hbox), calendar_data.prev_sig, FALSE, TRUE, 0);
;
;  hbox = gtk_hbox_new (FALSE, 3);
;  gtk_box_pack_start (GTK_BOX (vbox2), hbox, FALSE, TRUE, 0);
;  label = gtk_label_new ("Second previous signal:");
;  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);
;  calendar_data.prev2_sig = gtk_label_new ("");
;  gtk_box_pack_start (GTK_BOX (hbox), calendar_data.prev2_sig, FALSE, TRUE, 0);
;
;  bbox = gtk_hbutton_box_new ();
;  gtk_box_pack_start (GTK_BOX (vbox), bbox, FALSE, FALSE, 0);
;  gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_END);
;
;  button = gtk_button_new_with_label ("Close");
;  gtk_signal_connect (GTK_OBJECT (button), "clicked", 
;		       GTK_SIGNAL_FUNC (gtk_main_quit), 
;		       NULL);
;  gtk_container_add (GTK_CONTAINER (bbox), button);
;  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
;  gtk_widget_grab_default (button);
;
;  gtk_widget_show_all(window);
;}
;
;
;int main(int   argc,
;	  char *argv[] )
;{
;  gtk_set_locale ();
;  gtk_init (&argc, &argv);
;
;  create_calendar();
;
;  gtk_main();
;
;  return(0);
;}
;/* example-end */


(defpackage "09.12-calendar" (:use :excl :common-lisp))
(in-package "09.12-calendar")

(defconstant DEF_PAD 10)
(defconstant DEF_PAD_SMALL 5)

(defconstant TM_YEAR_BASE 1900)

(defclass CalendarData ()
  ((flag_checkboxes
    :initform (make-array 5)
    :accessor calendar-flag-checkboxes)
   (settings
    :initform (make-array 5)
    :accessor calendar-settings)
   (font
    :accessor calendar-font)
   (font_dialog
    :initform (ff:allocate-fobject '(* gtk:GtkWidget)
				   :foreign-static-gc)
    :accessor calendar-font-dialog)
   (window
    :accessor calendar-window)
   (prev2_sig
    :accessor calendar-prev2-sig)
   (prev_sig
    :accessor calendar-prev-sig)
   (last_sig
    :accessor calendar-last-sig)
   (month
    :accessor calendar-month)))
   

(macrolet ((init-calendar-consts (&rest consts &aux result)
	     (dotimes (i (length consts))
	       (push `(defconstant ,(elt consts i) ,i) result))
	     (setq result (nreverse result))
	     (push 'progn result)
	     result))
  (init-calendar-consts calendar_show_header
			calendar_show_days
			calendar_month_change 
			calendar_show_week
			calendar_monday_first))

(defun calendar-date-to-string (data)
  (let ((year (ff:allocate-fobject 'gtk:gint
				   :foreign-static-gc))
	(mon (ff:allocate-fobject 'gtk:gint
				  :foreign-static-gc))
	(mday (ff:allocate-fobject 'gtk:gint
				   :foreign-static-gc)))
    (setf (ff:fslot-value-typed 'gtk:gint nil year) 0)
    (setf (ff:fslot-value-typed 'gtk:gint nil mon) 0)
    (setf (ff:fslot-value-typed 'gtk:gint nil mday) 0)
    (gtk:gtk_calendar_get_date (gtk:GTK_CALENDAR (calendar-window data))
			       year mon mday)
    (decf (ff:fslot-value-typed 'gtk:gint nil year) TM_YEAR_BASE)
    ;; Make nicer later
    (format nil "Year: ~s Month: ~s Day: ~s"
	    (ff:fslot-value-typed 'gtk:gint nil year)
	    (ff:fslot-value-typed 'gtk:gint nil mon)
	    (ff:fslot-value-typed 'gtk:gint nil mday))))
    

(defun calendar-set-signal-strings (sig-str data)
  (let ((prev-sig (ff:allocate-fobject '(* gtk:gchar)
				       :foreign-static-gc)))
    (gtk:gtk_label_get (gtk:GTK_LABEL (calendar-prev-sig data))
		       (ff:fslot-address-typed '(* gtk:gchar) nil prev-sig))
    (gtk:gtk_label_set (gtk:GTK_LABEL (calendar-prev2-sig data))
		       (ff:fslot-value-typed '(* gtk:gchar) nil prev-sig))

    (gtk:gtk_label_get (gtk:GTK_LABEL (calendar-last-sig data))
		       (ff:fslot-address-typed '(* gtk:gchar) nil prev-sig))
    (gtk:gtk_label_set (gtk:GTK_LABEL (calendar-prev-sig data))
		       (ff:fslot-value-typed '(* gtk:gchar) nil prev-sig))
    (gtk:gtk_label_set (gtk:GTK_LABEL (calendar-last-sig data)) sig-str)))
		  
(ff:defun-foreign-callable calendar-month-changed ((widget (* gtk:GtkWidget))
						   data)
  (declare (ignore widget))
  (setq data
    (ff:lisp-value data))
  (calendar-set-signal-strings
   (format nil "month_changed: ~a" (calendar-date-to-string data))
   data))

(ff:defun-foreign-callable calendar-day-selected ((widget (* gtk:GtkWidget))
						  data)
  (declare (ignore widget))
  (setq data
    (ff:lisp-value data))
  (calendar-set-signal-strings
   (format nil "day_selected: ~a" (calendar-date-to-string data))
   data))

(ff:defun-foreign-callable calendar-day-selected-double-click
    ((widget (* gtk:GtkWidget))
     data)
  (declare (ignore widget))
  (setq data (ff:lisp-value data))

  (calendar-set-signal-strings
   (format nil "day_selected: ~a" (calendar-date-to-string data))
   data)

  (let ((year (ff:allocate-fobject 'gtk:gint
				   :foreign-static-gc))
	(mon (ff:allocate-fobject 'gtk:gint
				  :foreign-static-gc))
	(mday (ff:allocate-fobject 'gtk:gint
				   :foreign-static-gc)))
    (setf (ff:fslot-value-typed 'gtk:gint nil year) 0)
    (setf (ff:fslot-value-typed 'gtk:gint nil mon) 0)
    (setf (ff:fslot-value-typed 'gtk:gint nil mday) 0)
    (gtk:gtk_calendar_get_date (gtk:GTK_CALENDAR (calendar-window data))
			       year mon mday)
    (decf (ff:fslot-value-typed 'gtk:gint nil year) TM_YEAR_BASE)
    
    (if* (zerop
	  (ff:fslot-value-typed 'gtk:GtkCalendar nil
				(gtk:GTK_CALENDAR (calendar-window data))
				'gtk::marked_date
				(1- mday)))
       then (gtk:gtk_calendar_mark_day
	     (gtk:GTK_CALENDAR (calendar-window data))
	     mday)
       else (gtk:gtk_calendar_unmark_day
	     (gtk:GTK_CALENDAR (calendar-window data))
	     mday))))

(ff:defun-foreign-callable calendar-prev-month ((widget (* gtk:GtkWidget))
						data)
  (declare (ignore widget))
  (setq data
    (ff:lisp-value data))
  (calendar-set-signal-strings
   (format nil "prev_month: ~a" (calendar-date-to-string data))
   data))

(ff:defun-foreign-callable calendar-next-month ((widget (* gtk:GtkWidget))
						data)
  (declare (ignore widget))
  (setq data
    (ff:lisp-value data))
  (calendar-set-signal-strings
   (format nil "next_month: ~a" (calendar-date-to-string data))
   data))

(ff:defun-foreign-callable calendar-prev-year ((widget (* gtk:GtkWidget))
					       data)
  (declare (ignore widget))
  (setq data
    (ff:lisp-value data))
  (calendar-set-signal-strings
   (format nil "prev_month: ~a" (calendar-date-to-string data))
   data))

(ff:defun-foreign-callable calendar-next-year ((widget (* gtk:GtkWidget))
					       data)
  (declare (ignore widget))
  (setq data
    (ff:lisp-value data))
  (calendar-set-signal-strings
   (format nil "next_year: ~a" (calendar-date-to-string data))
   data))

(defun calendar-set-flags (calendar)
  (let ((options 0))
    (dotimes (i 5)
      (when (aref (calendar-settings calendar) i)
	(incf options (ash 1 i))))
    (unless (eql gtk:NULL (calendar-window calendar))
      (gtk:gtk_calendar_display_options
       (gtk:GTK_CALENDAR (calendar-window calendar))
       options))))

(ff:defun-foreign-callable calendar-toggle-flag ((toggle (* gtk:GtkWidget))
						 calendar)
  (setq calendar
    (ff:lisp-value
     calendar))
  (let ((j 0))
    (dotimes (i 5)
      (when (eql (aref (calendar-flag-checkboxes calendar) i) toggle)
	(setq j i)))
    (setf (aref (calendar-settings calendar) j)
      (not (aref (calendar-settings calendar) j)))
    (calendar-set-flags calendar)))
  
(ff:defun-foreign-callable calendar-font-selection-ok ((button
							(* gtk:GtkWidget))
						       calendar)
  (declare (ignore button))
  (setq calendar
    (ff:lisp-value
     calendar))
  (let ((style nil)
	(font nil))
    (setf (calendar-font calendar)
      (gtk:gtk_font_selection_dialog_get_font_name
       (gtk:GTK_FONT_SELECTION_DIALOG
	(ff:fslot-value-typed '(* gtk:GtkWidget) nil
			      (calendar-font-dialog calendar)))))
    (when (not (eql gtk:NULL (calendar-window calendar)))
      (setq font (gtk:gtk_font_selection_dialog_get_font
		  (gtk:GTK_FONT_SELECTION_DIALOG
		   (ff:fslot-value-typed '(* gtk:GtkWidget) nil
					 (calendar-font-dialog calendar)))))
      (when (not (eql gtk:NULL font))
	(setq style (gtk:gtk_style_copy
		     (gtk:gtk_widget_get_style (calendar-window calendar))))
	(gtk:gdk_font_unref
	 (ff:fslot-value-typed 'gtk:GtkStyle nil
			       style 'gtk::font))
	(setf (ff:fslot-value-typed 'gtk:GtkStyle nil
				    style 'gtk::font)
	  font)
	(gtk:gdk_font_ref (ff:fslot-value-typed 'gtk:GtkStyle nil
						style 'gtk::font))
	(gtk:gtk_widget_set_style (calendar-window calendar) style)))))

(ff:defun-foreign-callable calendar-select-font ((button (* gtk:GtkWidget))
						 calendar-cb)
  (declare (ignore button))
  (let ((calendar (ff:lisp-value calendar-cb))
	(window nil))
    (when (eql gtk:NULL (ff:fslot-value-typed '(* gtk:GtkWidget) nil
					      (calendar-font-dialog calendar)))
      (setq window (gtk:gtk_font_selection_dialog_new "Font Selection Dialog"))
      ;; (gtk:g_return_if_fail (gtk:GTK_IS_FONT_SELECTION_DIALOG window))
      (unless (gtk:GTK_IS_FONT_SELECTION_DIALOG window)
	(error "Assertion Failed: (gtk:GTK_IS_FONT_SELECTION_DIALOG ~s)"
	       window))
      (setf (ff:fslot-value-typed '(* gtk:GtkWidget) nil
				  (calendar-font-dialog calendar))
	window)

      (gtk:gtk_window_position (gtk:GTK_WINDOW window) gtk:GTK_WIN_POS_MOUSE)

      (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "destroy"
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:get-entry-point "gtk_widget_destroyed"))
			      (ff:fslot-address-typed
			       '(* gtk:GtkWidget) nil
			       (calendar-font-dialog calendar)))

      (gtk:gtk_signal_connect
       (gtk:GTK_OBJECT (ff:fslot-value-typed
			'gtk:GtkFontSelectionDialog nil
			(gtk:GTK_FONT_SELECTION_DIALOG window)
			'gtk::ok_button))
       "clicked"
       (gtk:GTK_SIGNAL_FUNC
	(ff:register-foreign-callable 'calendar-font-selection-ok))
       calendar-cb)
      (gtk:gtk_signal_connect_object
       (gtk:GTK_OBJECT (ff:fslot-value-typed
			'gtk:GtkFontSelectionDialog nil
			(gtk:GTK_FONT_SELECTION_DIALOG window)
			'gtk::cancel_button))
       "clicked"
       (gtk:GTK_SIGNAL_FUNC
	(ff:get-entry-point "gtk_widget_destroy"))
       (gtk:GTK_OBJECT (ff:fslot-value-typed
			'(* gtk:GtkObject) nil
			(calendar-font-dialog calendar)))))
    
    (setq window (ff:fslot-value-typed '(* gtk:GtkWidget) nil
				       (calendar-font-dialog calendar)))
    (if* (gtk:GTK_WIDGET_VISIBLE window)
       then (gtk:gtk_widget_destroy window)
       else (gtk:gtk_widget_show window))))

(ff:defun-foreign-callable cb-gtk-main-quit ()
  (gtk:gtk-main-quit))

(let ((calendar-data (ff:register-lisp-value (make-instance 'CalendarData))))
  (defun create-calendar ()
    (let ((window nil)
	  (vbox nil)
	  (vbox2 nil)
	  (vbox3 nil)
	  (hbox nil)
	  (hbbox nil)
	  (calendar nil)
	  (toggle nil)
	  (button nil)
	  (frame nil)
	  (separator nil)
	  (label nil)
	  (bbox nil)
	  (flags '#("Show Heading"
		    "Show Day Names"
		    "No Month Change"
		    "Show Week Numbers"
		    "Week Start Monday")))
      (setf (calendar-window (ff:lisp-value calendar-data)) gtk:NULL)
      (setf (calendar-font (ff:lisp-value calendar-data)) gtk:NULL)
      (setf (ff:fslot-value-typed
	     '(* gtk:GtkWidget) nil
	     (calendar-font-dialog (ff:lisp-value calendar-data)))
	gtk:NULL)

      (dotimes (i 5)
	(setf (aref (calendar-settings (ff:lisp-value calendar-data)) i) nil))

      (setq window (gtk:gtk_window_new gtk:GTK_WINDOW_TOPLEVEL))
      (gtk:gtk_window_set_title (gtk:GTK_WINDOW window) "GtkCalendar Example")
      (gtk:gtk_container_border_width (gtk:GTK_CONTAINER window) 5)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "destroy"
			      (gtk:GTK_SIGNAL_FUNC
			       #+original (ff:get-entry-point "gtk_main_quit")
			       #-original (ff:register-foreign-callable
					   'cb-gtk-main-quit))
			      gtk:NULL)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT window) "delete-event"
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:get-entry-point "gtk_false"))
			      gtk:NULL)

      (gtk:gtk_window_set_policy (gtk:GTK_WINDOW window) gtk:FALSE gtk:FALSE
				 gtk:TRUE)

      (setq vbox (gtk:gtk_vbox_new gtk:FALSE DEF_PAD))
      (gtk:gtk_container_add (gtk:GTK_CONTAINER window) vbox)

      (setq hbox (gtk:gtk_hbox_new gtk:FALSE DEF_PAD))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) hbox gtk:TRUE gtk:TRUE
			      DEF_PAD)
      (setq hbbox (gtk:gtk_hbutton_box_new))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) hbbox gtk:FALSE gtk:FALSE
			      DEF_PAD)
      (gtk:gtk_button_box_set_layout (gtk:GTK_BUTTON_BOX hbbox)
				     gtk:GTK_BUTTONBOX_SPREAD)
      (gtk:gtk_button_box_set_spacing (gtk:GTK_BUTTON_BOX hbbox) 5)

      (setq frame (gtk:gtk_frame_new "Calendar"))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbbox) frame gtk:FALSE gtk:TRUE
			      DEF_PAD)
      (setq calendar (gtk:gtk_calendar_new))
      (setf (calendar-window (ff:lisp-value calendar-data)) calendar)
      (calendar-set-flags (ff:lisp-value calendar-data))
      (gtk:gtk_calendar_mark_day (gtk:GTK_CALENDAR calendar) 19)
      (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) calendar)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT calendar) "month_changed"  
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'calendar-month-changed)) 
			      calendar-data)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT calendar) "day_selected"  
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'calendar-day-selected))
			      calendar-data)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT calendar)
			      "day_selected_double_click"  
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'calendar-day-selected-double-click) )
			      calendar-data)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT calendar) "prev_month"  
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'calendar-prev-month))
			      calendar-data)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT calendar) "next_month"  
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'calendar-next-month))
			      calendar-data)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT calendar) "prev_year"  
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'calendar-prev-year))
			      calendar-data)
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT calendar) "next_year"  
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'calendar-next-year))
			      calendar-data)


      (setq separator (gtk:gtk_vseparator_new))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) separator gtk:FALSE gtk:TRUE
			      0)

      (setq vbox2 (gtk:gtk_vbox_new gtk:FALSE DEF_PAD))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) vbox2 gtk:FALSE gtk:FALSE
			      DEF_PAD)

      (setq frame (gtk:gtk_frame_new "Flags"))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) frame gtk:TRUE gtk:TRUE
			      DEF_PAD)
      (setq vbox3 (gtk:gtk_vbox_new gtk:TRUE DEF_PAD_SMALL))
      (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) vbox3)

      (dotimes (i 5)
	(setq toggle (gtk:gtk_check_button_new_with_label (aref flags i)))
	(gtk:gtk_signal_connect (gtk:GTK_OBJECT toggle) "toggled" 
				(gtk:GTK_SIGNAL_FUNC
				 (ff:register-foreign-callable
				  'calendar-toggle-flag))
				calendar-data)
	(gtk:gtk_box_pack_start (gtk:GTK_BOX vbox3) toggle gtk:TRUE gtk:TRUE 0)
	(setf (aref (calendar-flag-checkboxes (ff:lisp-value calendar-data)) i)
	  toggle))
      
      (setq button (gtk:gtk_button_new_with_label "Font..."))
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT button) "clicked" 
			      (gtk:GTK_SIGNAL_FUNC
			       (ff:register-foreign-callable
				'calendar-select-font))
			      calendar-data)
      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) button gtk:FALSE gtk:FALSE 0)

      (setq frame (gtk:gtk_frame_new "Signal events"))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) frame gtk:TRUE gtk:TRUE
			      DEF_PAD)

      (setq vbox2 (gtk:gtk_vbox_new gtk:TRUE DEF_PAD_SMALL))
      (gtk:gtk_container_add (gtk:GTK_CONTAINER frame) vbox2)

      (setq hbox (gtk:gtk_hbox_new gtk:FALSE 3))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) hbox gtk:FALSE gtk:TRUE 0)
      (setq label (gtk:gtk_label_new "Signal:"))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) label gtk:FALSE gtk:TRUE 0)
      (setf (calendar-last-sig (ff:lisp-value calendar-data))
	(gtk:gtk_label_new ""))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) (calendar-last-sig
						  (ff:lisp-value
						   calendar-data))
			      gtk:FALSE gtk:TRUE 0)

      (setq hbox (gtk:gtk_hbox_new gtk:FALSE 3))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) hbox gtk:FALSE gtk:TRUE 0)
      (setq label (gtk:gtk_label_new "Previous Signal:"))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) label gtk:FALSE gtk:TRUE 0)
      (setf (calendar-prev-sig (ff:lisp-value calendar-data))
	(gtk:gtk_label_new ""))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) (calendar-prev-sig
						  (ff:lisp-value
						   calendar-data))
			      gtk:FALSE gtk:TRUE 0)

      (setq hbox (gtk:gtk_hbox_new gtk:FALSE 3))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox2) hbox gtk:FALSE gtk:TRUE 0)
      (setq label (gtk:gtk_label_new "Second Previous Signal:"))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) label gtk:FALSE gtk:TRUE 0)
      (setf (calendar-prev2-sig (ff:lisp-value calendar-data))
	(gtk:gtk_label_new ""))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX hbox) (calendar-prev2-sig
						  (ff:lisp-value
						   calendar-data))
			      gtk:FALSE gtk:TRUE 0)

      (setq bbox (gtk:gtk_hbutton_box_new))
      (gtk:gtk_box_pack_start (gtk:GTK_BOX vbox) bbox gtk:FALSE gtk:FALSE 0)
      (gtk:gtk_button_box_set_layout (gtk:GTK_BUTTON_BOX bbox)
				     gtk:GTK_BUTTONBOX_END)

      (setq button (gtk:gtk_button_new_with_label "Close"))
      (gtk:gtk_signal_connect (gtk:GTK_OBJECT button) "clicked"  
			      (gtk:GTK_SIGNAL_FUNC
			       #+original (ff:get-entry-point "gtk_main_quit")
			       #-original (ff:register-foreign-callable
					   'cb-gtk-main-quit))
			      gtk:NULL)
      (gtk:gtk_container_add (gtk:GTK_CONTAINER bbox) button)
      (gtk:GTK_WIDGET_SET_FLAGS button gtk:GTK_CAN_DEFAULT)
      (gtk:gtk_widget_grab_default button)

      (gtk:gtk_widget_show_all window))))

(defun calendar ()
  (gtk:gtk_set_locale)
  (gtk:gtk_init 0 0)

  (create-calendar)

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
  (run-example "09.12-calendar" #'calendar))
