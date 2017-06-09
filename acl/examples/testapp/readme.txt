
The files in this directory make up a test, or skeleton, application.
You make use this code as a starting point to package your
application.

The application works on Windows, as well as UNIX.  On Windows,
however, Redhat's cygwin tools are needed for building
(http://sources.redhat.com/cygwin/) due to the use of GNU `make'.

By default, on Windows, the test app throws up dialog boxes.  If
instead, you want to use `stdout', like UNIX apps do, change the
makefile variable WINDOWS_CONSOLE_APP to `t' (from `nil' the
default).

This will build it:
   % make
   ...lots of output...

Now, we run it:

% dist/test -V
testapp: $Revision: 1.2 $
% dist/test
Usage: test [-D] [-V] [-a] [-b] [-c] [-d number | -e file]

One (and only one) of the following must be given:
  -d number :: frob number
  -e file   :: frob file

The following are optional:
  -D :: debug mode
  -V :: print version number and exit
  -a :: a frob
  -b :: b frob
  -c :: c frob

% dist/test -x
-x is not a possible option
% dist/test -a -b
Usage: test [-D] [-V] [-a] [-b] [-c] [-d number | -e file]

One (and only one) of the following must be given:
  -d number :: frob number
  -e file   :: frob file

The following are optional:
  -D :: debug mode
  -V :: print version number and exit
  -a :: a frob
  -b :: b frob
  -c :: c frob

% dist/test -a -d 10
number is 10, -a
% dist/test -a -b -d 10
number is 10, -a, -b
% dist/test -D
Break: debug me!

Restart actions (select using :continue):
 0: return from break.
 1: Abort entirely from this process.
// (exit)
; Exiting Lisp
% dist/test -a -b -e foo
File foo does not exist.
% dist/test -a -b -e makefile
file is "makefile", -a, -b
% 
