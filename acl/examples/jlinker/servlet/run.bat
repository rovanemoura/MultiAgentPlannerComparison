@echo off

rem $Id: run.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

rem Run the servlet sample application.

rem This script will run without changes in most cases.
rem All the customizations are in setjv.bat.

echo on
call setjv
echo off

echo Next step is to start ACL
pause

%RUNACL% -L run.cl

echo Next step is to start Servlet engine
pause

cd %SVROOT%
rem this expects to run in the jswdk directory

set classpath=%JLROOT%;%JLROOT%\jlinker.jar

call startserver.bat

echo off
echo Next step is to start browser at:   http://localhost:8080/LispSv.htm
echo If this does not start a fresh browser, the browser may see
echo the wrong environment variable values.
pause
start http://localhost:8080/LispSv.htm

echo.
echo.
echo Continue to STOP the Server
pause
echo Continue to STOP the Server -- For sure???
pause


call stopserver.bat

pause
