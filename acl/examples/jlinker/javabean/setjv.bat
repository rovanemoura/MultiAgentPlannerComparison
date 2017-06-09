
rem $Id: setjv.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

rem Modify the following to point to the Java home directory
if "%JAVA_HOME%"=="" set JAVA_HOME=c:\java\jdk1.2

rem Modify the following to a command that starts ACL
rem  (and accepts command-line arguments)
set RUNACL=call runacl

rem Modify the following to point to the jlinker.jar directory
set JLINKER=c:\acl\jlinker

rem Modify the following to point to the BDK directory
set BDKHOME=c:\BDK1.1

path | find "%JAVA_HOME%" >NUL
if errorlevel 1 goto jset
goto done
:jset
path %path%;%JAVA_HOME%\bin;

:done
