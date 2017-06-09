@echo off

rem $Id: prep.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

rem Compile Java files and copy to expected folders

rem This script will run without changes in most cases.
rem All the customizations are in setjv.bat.
rem A similar script can be prepared for Unix.

echo on
call setjv
echo on

copy /b %JLINKER%\jlinker.jar . 

if "x%classpath%"=="x" goto empty
set classpath=.;.\jlinker.jar;%classpath%
goto append
:empty
set classpath=.;.\jlinker.jar
:append
set classpath=%classpath%;%SVROOT%\lib\servlet.jar

javac LispHttpServlet.java
javac LispAsyncHttpServlet.java
javac User1Servlet.java
javac User1ServletA.java
javac User2Servlet.java
javac User2ServletA.java
javac User2ServletB.java

copy /b User*.class %SVROOT%\examples\WEB-INF\servlets\

copy /a LispSv.htm %SVROOT%\webpages\

pause



