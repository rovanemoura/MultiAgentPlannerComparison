Allegro CL GTK+ 2.0 Interface Examples.


The lisp files in this directory are translations of the examples from
the GTK+ 2.0 tutorial.

For the most part, these are line-by-line translations of the original
C example programs.  The lisp programs are written this way so that
readers can compare how to use GTK+ 2.0 functions in Lisp.

The name of each lisp example file is nn.mm-name.cl, where nn.mm is
the chapter and section of the tutorial from which the example is found
in the GTK+ 2.0 tutorial.

Notes:

--The examples, as written in C, are meant to be standalone
  executables.  In particular, the application generally exits when
  the main window receives a signal to close or be deleted.  It is the
  program's exiting which actually removes the window from the screen.

  With the Lisp equivalent of some of the examples, though, the
  examples run in separate processes with a single Lisp environment.
  Since the Lisp doesn't necessarily exit when the example quits, its
  windows may not get removed until the Lisp itself exits.  Advanced
  examples demonstrate how to remove windows, but the simpler
  examples, which are copied rather strictly from the tutorial don't
  necessarily "clean up" even after clicking a suggestive button such
  as "quit".  Thus, when running examples that weren't coded to remove
  their windows, you may have to exit Lisp to clear their windows'
  remnants from the screen.

  Rather than modify the examples to run cleanly in a Lisp environment
  which does not necessarily start and exit each example separately,
  the examples are left, for instructional purposes, coded as closely
  to their original C form as possible.

--For simplicity, the examples call gtk:gtk_init with effectively null
  arguments.  A lisp gtk+ application programmer may wish to construct
  a C-style argv/argc set from, say, sys:command-line-argument.

--Many examples call gtk:gtk-main instead of gtk:gtk_main.  The latter
  is defined by gtk+.  gtk:gtk-main is meant to be a lisp substitute
  for gtk:gtk_main.  gtk:gtk-main works with the Lisp multi-processing
  facility so that other processes within Lisp can execute while
  running gtk+ functionality.  If one uses gtk:gtk_main (what is
  supplied by gtk+), then the entire Lisp process can effectively
  freeze up until gtk:gtk_main exits.

  At this writing, gtk:gtk-main, along with gtk:gtk-events-pending,
  and gtk:gtk-main-quite, are defined in eh.cl.

