@echo off

rem $Id: dobrows.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

call setjava

echo First step: start ACL
rem pause
start call-acl -L one-applet.cl

echo Next step: start browser on:   embed.htm
pause
call call-brw embed.htm

pause
