This directory contains an OpenGL interface.  Layered upon this
interface are a GTK and Common Graphics (Windows-only) veneer.  Read
about them here in the files:

  * cggl/doc.txt
  * gtkgl/doc.txt

The files in the following directories were automatically generated:

  win32-1.1/	OpenGL 1.1 on Windows 2000
  linux-1.3/	OpenGL 1.3 on Red Hat 9.0
  macosx-2.0/	OpenGL 2.0 on Max OS X (ppc or x86)

The files were generated with SWIG 1.3.30 (from www.swig.org).  If you
want to know more about this see the `Makefile'.

To run the teapot demo on Windows, start up the IDE and evaluate:

        (load "sys:opengl;cggl;load.cl")
        (red-teapot)

   This is a wrappin of OpenGL version 1.1.

To run the GTK teapot demo on Linux/UNIX, start up Lisp and evaluate:

        (load "sys:opengl;gtkgl;load.cl")
	(red-teapot)

    This is a wrapping of OpenGL version 1.3.

To run the GLUT teapot demo on macosx (ppc or x86), start up Lisp and evaluate:

        (progn (load (compile-file "sys:opengl;macosx-2.0;opengl.cl"))
	       (load (compile-file "sys:opengl;glut-teapot.cl")))

	(teapot) ;; args ignored for this function.

    This is a wrapping of OpenGL version 2.0.

TROUBLESHOOTING

- On Linux, we've found that different distributions place libraries in
  different locations. In Fedora Core 5, for intance, libXi.so and
  libXmu.so are located in /usr/lib rather than /usr/X11R6/lib.

  Users should make sure that the required directories are part of the
  LD_LIBRARY_PATH environment variable at the time lisp is started or
  you will see errors on load of this package.

