echo on
rem $Id: setjv.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

rem remove the following statement when this file is ready
exit

rem Modify the following to point to the Java home directory
if "%JAVA_HOME%"=="" set JAVA_HOME=c:\java\jdk1.2

rem Modify the following to point to the JSWDK directory
set SVROOT=c:\java\Servlets\jswdk-1.0.1

rem Modify the following to a command that starts ACL
rem  (and accepts command-line arguments)
set RUNACL=call runacl

rem Modify the following to point to the jlinker.jar directory
set JLINKER=c:\acl\jlinker

rem Modify the following to point to the current directory
set JLROOT=c:\users\self

path | find "%JAVA_HOME%" >NUL
if errorlevel 1 goto jset
goto done
:jset
path %path%;%JAVA_HOME%\bin;

:done
