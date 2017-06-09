@echo off

rem $Id: doappv.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

rem Run the Applet viewer

call setjava

echo First step: start ACL
rem pause
start call-acl -L one-applet.cl

echo Next step: start Appler Viewer
pause

appletviewer applet.htm

pause

