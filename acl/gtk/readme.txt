Allegro CL GTK+ Interface.

Introduction.

The files in this directory define Allegro CL interfaces to GTK+.
Separate interfaces are defined for GTK+ 1.2 and GTK+ 2.0.

Neither interface is meant to be regarded as a Lisp language binding
to GTK+.  Rather, it is a foreign function interface to the C binding
of GTK+.  To use the interface, you should be familiar with both the
Allegro CL foreign function interface and the C binding of GTK+.

GTK+ is assumed already to be installed.  You can download GTK+ from
the http://www.gtk.org web site.

Documentation.

GTK+ documentation can be found at the http://www.gtk.org web site.

There is no specific documentation for the Allegro CL GTK+ interface.
There are several examples in the interface's lispex-gtk??/
subdirectory (where 'gtk??' is gtk12 for GTK+ 1.2, and gtk20 for GTK+
2.0).  For the most part, these examples are line-by-line translations
from C to Lisp of the examples in the GTK+ tutorial.  These examples
demonstrate the various capabilities of GTK+ and how to use these
capabilities in Lisp via the Allegro CL GTK+ interface.  More
information about the examples can be found in lispex/readme.txt.

GTK+ Interface Usage.

To use the interface, you must be running Allegro CL 6.2 or later.
The LD_LIBRARY_PATH environment variable must include the directory
containing the GTK+ libraries.

Start Allegro CL and load the file "loadgtk??.cl" in this directory
(where 'loadgtk??.cl' is loadgtk12.cl for GTK+ 1.2, and loadgtk20.cl
for GTK+ 2.0).  Lisp will automatically try to compile (if necessary)
and then load the appropriate files from this gtk directory (i.e., the
directory containing this readme.txt file).  If the file gtk??-lib.so
doesn't already exist, Lisp will also automatically try to create
gtk??-lib.so ('gtk??-lib.so' is gtk12-lib.so for GTK+ 1.2, and
gtk20-lib.so for GTK+ 2.0).  This file is a shared-library file used
to access the GTK+ libraries.

Each lispex-gtk??/ subdirectory contains examples.  An example file
can be loaded into Lisp after load.cl has been loaded.  For more
information on the examples, see lispex-gtk??/readme.txt.

Support.

This release of the Allegro CL GTK+ Interface is a work-in-progress.
It is being made available AS IS and not officially supported.  If you
have questions, comments, or suggestions about this interface, please
feel free to relay them to Franz Inc.  We welcome your input about
this release of the interface.

At the time of this writing, this interface has been targeted for the
RedHat Linux and Solaris platforms only.

