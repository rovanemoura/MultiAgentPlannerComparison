@echo off

rem $Id: prep-inet.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

goto notready

rem insert the Allegro CL home directory path here:
set aclroot=???

rem insert the Java home directory path here:
path %path%;???\bin;

if not exist jlinker.jar  copy /b %aclroot%\jlinker\jlinker.jar  .
if not exist jl-config.cl copy /b %aclroot%\jlinker\jl-config.cl .

set CLASSPATH=.;jlinker.jar
echo on
javac JLTextListener.java
javac TrekAppletLocal.java
javac TrekAppletInet.java
javac AnnotatorPanel.java
echo off
if not exist userdata mkdir userdata

pause

goto done


:notready
echo off
echo ===================================================
echo =
echo = This file ( %0 ) 
echo = has not been customized for the current location.
echo =
echo = Edit this file and replace ??? with appropriate 
echo = values. 
echo = 
echo = Then comment out the "goto notready" statement
echo = at the beginning of he file.
echo =
echo ===================================================
pause
exit
:done

