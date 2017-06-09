@echo off

rem $Id: run-tomcat.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

echo run with Jakarta-Tomcat-4

rem Run the servlet sample application.

rem This script will run without changes in most cases.
rem All the customizations are in set-tomcat.bat.

echo on
call set-tomcat
echo off

echo Next step is to start ACL
pause

%RUNACL% -L run.cl

echo Next step is to start Servlet engine
pause

cd %SVROOT%
rem this expects to run in the tomcat directory

rem set classpath=%JLROOT%;%JLROOT%\jlinker.jar
cd bin
call startup.bat

echo off
echo Next step is to start browser at:   http://localhost:8080/jlex/LispSv.htm
echo If this does not start a fresh browser, the browser may see
echo the wrong environment variable values.
pause
start http://localhost:8080/jlex/LispSv.htm

echo.
echo.
echo Continue to STOP the Server
pause
echo Continue to STOP the Server -- For sure???
pause


call %SVROOT%\bin\shutdown.bat

pause
