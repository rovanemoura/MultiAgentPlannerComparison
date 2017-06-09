This information also appears in
   http://franz.com/products/express/modern-mode.lhtml

Thank you for downloading and installing the Free Express Edition of
Allegro CL.

To make the downloadable file as small as possible, we have only
included one Allegro CL image, alisp/alisp.dxl. That is the
International "ANSI" mode (case insensitive upper, as opposed to case
sensitive lower of "Modern" images) executable/image.

The other images which are commonly found in an Allegro CL
installation on UNIX are:

    mlisp       Modern base Lisp image, international (16-bit characters)

To build an mlisp (Modern, international) image, in this directory
startup alisp and evaluate the following form at a prompt:

  ;; mlisp:
  (progn
    (build-lisp-image "sys:mlisp.dxl" :case-mode :case-sensitive-lower
                      :include-ide nil :restart-app-function nil)
    (when (probe-file "sys:mlisp") (delete-file "sys:mlisp"))
    (sys:copy-file "sys:alisp" "sys:mlisp"))
