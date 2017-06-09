@echo off

rem $Id: run-local.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

echo start applet demo with user entry screen
pause
start run-acl -L run-local.cl

echo Next: start browser (after Lisp server is ready)
pause
start t-local.htm

