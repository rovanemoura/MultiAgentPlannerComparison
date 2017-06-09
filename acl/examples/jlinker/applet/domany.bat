@echo off

rem $Id: domany.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

call setjava

echo Next step: start ACL
rem pause
start call-acl -L many-applets.cl

echo Next step: start Appler Viewer
pause
start appletviewer applet.htm

echo Next step: start a browser and throw in embed.htm
pause
call call-brw  embed.htm

pause
