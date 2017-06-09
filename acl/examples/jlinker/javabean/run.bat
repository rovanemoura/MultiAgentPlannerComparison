@echo off

rem $Id: run.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

rem Run the JavaBean sample application.

rem This script will run without changes in most cases.
rem All the customizations are in setjv.bat.

echo on
if "x%1"=="" call setjv
if not "x%1"=="" call %1
echo off

echo Next step is to start ACL
pause

%RUNACL% -L run.cl

echo Next step is to start BeanBox
pause

start /d%BDKHOME%\beanbox run.bat

pause
