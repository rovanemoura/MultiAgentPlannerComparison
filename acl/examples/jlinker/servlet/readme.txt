
;; $Id: readme.txt,v 5.0 2004/01/14 18:31:35 layer Exp $


This file contains a simple example of how a servlet can be
implemented in Allegro CL using jLinker to communicate between the ACL
application and the Java servlet environment,

NOTE: The sample application opens a web server at port 8080 and
allows arbitrary evaluations from a web page.  This example should
only be run behind an appropriate firewall.

These examples are configured to run with two different servlet
containers.  We show instructions for Windows users.  To run the
example on Unix, you will need to get the appropriate servlet
container and you will need to modify the .bat files to corresponding
shell scripts in your favorite shell language.

We have chosen specific values for many parameters in order to
simplify the preparation needed to run the examples.  Most of these
values (port numbers, directory names, ...) may be modified to suit
the requirements of a particular installation.  When possible, we have
commented arbitrary choices as such.

-------------------------------------------
Container 1: JavaServer Web Development Kit
             (this container is no longer available)

	JavaServer Web Development Kit (JSWDK) 1.0.1
	JSWDK released: October, 1999 

Was obtainable from Sun Microsystems at
<http://java.sun.com/products/servlet/>.

To run the example on Windows, copy this entire directory to a
temporary directory and follow the instructions below.

	1. modify the file setjv.bat
	2. run prep.bat to compile the Java files
	3. run run.bat to run the sample application:
		- starts ACL with jLinker and servlet code
		- starts servlet simulator in the JSWDK
		- starts a web browser on the application page
	4. click on links in the application page to see the
		application.


-------------------------------------------
Container 2: Tomcat 4 Servlet/JSP container

Obtained from The Jakarta Project at <http://jakarta.apache.org/>.
The version of Tomcat-4 that we tested required
	JDK 1.2.2 (or later) and
	Java Naming and Directory Interface(TM) (JNDI) 1.2.1 (or later)

To run the example on Windows, copy this entire directory to a
temporary directory and follow the instructions below.

	1. modify the file set-tomcat.bat
	2. run prep-tomcat.bat to compile the Java files
	3. run run-tomcat.bat to run the sample application:
		- starts ACL with jLinker and servlet code
		- starts the Tomcat servlet container
		- starts a web browser on the application page
	4. click on links in the application page to see the
		application.



--------------------------------------------------------------------

readme.txt	this file


LispHttpServlet.java
LispAsyncHttpServlet.java

		Prototype classes to communicate with Lisp.
		These classes may be sub-classed or modified
		as required by the application.


User1Servlet.java
User1ServletA.java
User2Servlet.java
User2ServletA.java
User2ServletB.java

		The Java code for the sample servlet applications.


user1.cl	The Lisp code called form User1Servlet and User1ServletA.
user2.cl	The Lisp code called form User2Servlet, User2ServletA
		and User2ServletB.


LispSv.htm	The HTML file that initiates the sample applications
LispSvTC.htm


setjv.bat	A sample Windows script to set environment variables
set-tomcat.bat	used by the other scripts.  MUST be modified to fit
		the site where it is used.


prep.bat	A Windows script to prepare the sample application to 
		run with an installed JSWDK.


prep-tomcat.bat	A Windows script to prepare the sample application to 
		run with an installed Tomcat-4 container.


run.bat		A Windows script to run the sample application.
run-tomcat.bat	


run.cl		A Lisp file used by the above script.


