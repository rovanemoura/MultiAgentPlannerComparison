@echo off

rem $Id: setjava.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

rem set PATH appropriately for local Java version
rem (in this case we call some script setjv12 - maybe to set the
rem     variables for a Java 1.2 installation)
rem CHANGE THIS TO SOMETHING THAT WILL WORK
call setjv12

rem CHANGE the source folder to point to the installed location
rem    of the file jlinker.jar
copy /b ..\..\jlinker.jar . 

rem The following should work without change if CLASSPATH is
rem     normally null (the Java 1.2 default).
rem     If CLASSPATH already is set, you will need something like this:
rem     set classpath=.;.\jlinker.jar;%classpath%
set classpath=.;.\jlinker.jar

