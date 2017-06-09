(in-package :gtk)

;;; This is missing from gtk20.cl.  Seems to return zero,
;;; though the functions that take a display expect a pointer.
(ff:bind-c-function gtk::gdk_x11_get_default_xdisplay
     :unconverted-entry-name "gdk_x11_get_default_xdisplay"
     :c-return-type ("Display" "*")
     :return-type (* Display)
     :c-arg-types nil
     :c-arg-names nil
     :arguments (:void)
     :strings-convert t
     )
(ff:bind-c-function gtk::gdk_x11_get_default_screen
     :unconverted-entry-name "gdk_x11_get_default_screen"
     :c-return-type ("gint")
     :return-type gint
     :c-arg-types nil
     :c-arg-names nil
     :arguments (:void)
     :strings-convert t
     )
(ff:bind-c-function gtk::gdk_x11_drawable_get_xid
     :unconverted-entry-name "gdk_x11_drawable_get_xid"
     :c-return-type ("gint")
     :return-type gint
     :c-arg-types (("GdkDrawable" "*"))
     :c-arg-names (drawable)
     :arguments ((* GdkDrawable))
     :strings-convert t
     )
