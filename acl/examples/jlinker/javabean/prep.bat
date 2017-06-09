@echo off

rem $Id: prep.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

rem This script will run without changes in most cases.
rem All the customizations are in setjv.bat.
rem A similar script can be prepared for Unix.

echo on
if "x%1"=="" call setjv
if not "x%1"=="" call %1
echo on

rem Compile Lisp files and generate Java files
%RUNACL% -L prep.cl
echo Wait for Lisp to finish
pause
echo on

copy /b %JLINKER%\jlinker.jar . 

if "x%classpath%"=="x" goto empty
set classpath=.;.\jlinker.jar;%classpath%
goto append
:empty
set classpath=.;.\jlinker.jar
:append
set classpath=%classpath%;%SVROOT%\lib\servlet.jar

for %%x in ( *.java ) do javac %%x

echo off
echo Next step is to make jar file with manifest
pause

echo Name: LispButton.class >manifest.tmp
echo Java-Bean: True >>manifest.tmp
echo.>>manifest.tmp
echo Name: LispBean.class >>manifest.tmp
echo Java-Bean: True >>manifest.tmp

jar cfm clbtn.jar manifest.tmp *.class

echo off
echo Next step is to copy jar files to BeanBox
pause
echo on

copy /b clbtn.jar    %BDKHOME%\jars\
copy /b jlinker.jar  %BDKHOME%\

pause
