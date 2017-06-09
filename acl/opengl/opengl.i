%module opengl

#pragma SWIG nowarn=504,401

%insert("lisphead")  %{
;; typical locations are /usr/lib and /usr/X11R6/lib.
#+linux
(progn
  (load "libGL.so")
  (load "libGLU.so")
  (load "libXi.so")
  (load "libXmu.so"))
#+mswindows
(progn
  (load "opengl32.dll" :system-library t)
  (load "glu32.dll" :system-library t))
#+macosx
(progn
  (load "/System/Library/Frameworks/OpenGL.framework/OpenGL" :foreign t)
  (load "/System/Library/Frameworks/GLUT.framework/GLUT" :foreign t)
 )

%}


%typemap(lout) const GLubyte *
%{  (cl:let* ((address $body))
    (cl:setq ACL_ffresult (cl:if (cl:eq address 0) 0
      	     		  	 (excl:native-to-string address))))  %}

#ifdef ACL_WINDOWS
#define WINGDIAPI
#define APIENTRY
%include "c:/Program Files/Microsoft Platform SDK/Include/gl/GL.h"
#define CALLBACK
%include "c:/Program Files/Microsoft Platform SDK/Include/gl/GLU.h"
#else

#ifdef ACL_LINUX
%include "/usr/include/GL/gl.h"
%include "/usr/include/GL/glu.h"
%include "/usr/include/GL/glut.h"
#else
%include "/System/Library/Frameworks/OpenGL.framework/Headers/gl.h"
%include "/System/Library/Frameworks/OpenGL.framework/Headers/glu.h"
%include "/net/bronx/System/Library/Frameworks/GLUT.framework/Headers/glut.h"
#endif
#endif
