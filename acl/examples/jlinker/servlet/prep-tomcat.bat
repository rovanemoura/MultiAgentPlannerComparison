@echo off

rem $Id: prep-tomcat.bat,v 5.0 2004/01/14 18:31:35 layer Exp $

rem Compile Java files and copy to expected folders

rem This script will run without changes in most cases.
rem All the customizations are in set-tomcat.bat.
rem A similar script can be prepared for Unix.

echo on
call set-tomcat
echo on

copy /b %JLINKER%\jlinker.jar . 

if "x%classpath%"=="x" goto empty
set classpath=.;.\jlinker.jar;%classpath%
goto append
:empty
set classpath=.;.\jlinker.jar
:append
set classpath=%classpath%;%SVROOT%\common\lib\servlet.jar

javac LispHttpServlet.java
javac LispAsyncHttpServlet.java
javac User1Servlet.java
javac User1ServletA.java
javac User2Servlet.java
javac User2ServletA.java
javac User2ServletB.java

set jlexroot=%SVROOT%\webapps\jlex
if exist %jlexroot%  rmdir %jlexroot% /s /q

mkdir %jlexroot%
mkdir %jlexroot%\WEB-INF
mkdir %jlexroot%\WEB-INF\classes
mkdir %jlexroot%\WEB-INF\lib
copy /b *.class %jlexroot%\WEB-INF\classes\
copy /a LispSvTC.htm  %jlexroot%\LispSv.htm
copy /b jlinker.jar %jlexroot%\WEB-INF\lib

pause



