
;; $Id: readme.txt,v 5.0 2004/01/14 18:31:35 layer Exp $


This file contains a simple example of how Java Beans can be implemented
in Allegro CL using jLinker to communicate between the ACL application
and the Java Bean environment,

The examples use

	the Beans Development Kit (BDK) Version 1.1

obtainable from Sun Microsystems at <http://java.sun.com/>.

To run the example on Windows, copy this entire directory to a temporary
directory and follow the instructions below.
To run the example on Unix, you will need to get the appropriate BDK
and you will need to modify the .bat files to corresponding shell scripts
in your favorite shell language.

	1. modify the file setjv.bat
	2. run prep.bat to compile the Lisp and Java files
	3. run run.bat to run the sample application:
		- starts ACL with jLinker and Bean code
		- starts the BDK BeanBox application
	4. modify run.bat in the <BDK>/beanbox/ directory
	    to set path and classpath appropriately

On Unix, the .bat files must naturally be modified to corresponding
shell scripts.


In the BeanBox:

	LispButton example

	1. Create an instance of the Juggler
	2. Create an instance of LispButton
		- change the label and color
		- set debug=True to see messages
		  appear in the Lisp console window when
		  the button is pressed
		- select edit/events/action/actionPerformed
		  and connect the event to the stopJuggling
		  method of the Juggler
	3. Create another instance of LispButton
		- change the label and color
		- set debug=True to see messages
		  appear in the Lisp console window when
		  the button is pressed
		- select edit/events/action/actionPerformed
		  and connect the event to the startJuggling
		  method of the Juggler
	4. Press the start and stop buttons and observe the behavior
		in Lisp.
	   Trace the Lisp methods of LispButton to observe the
		interaction between Java and Lisp.


	LispBean example

	1. create an instance of LispBean
	2. create 2 buttons (but dont use LispButton!)
	3. connect one button to startAction in LispBean
	4. connect the other button to stopAction in LispBean
	5. start and stop the Lisp bean, observe the behavior
	   as the properties of LispBean are modified

	It is not possible to use LispButton to start the 
	LispBean because that may create asynchronous and
	simultaneous calls between Lisp and Java and cause
	deadlocks in the jLinker communication channels.
	It is the resposibility of the application (programmer)
	to ensure that the re-entrancy rules are not violated.


--------------------------------------------------------------------

readme.txt	this file

MyConnector.java
		An example of a sub-class of LispConnector.java.

LispButtonBase.java
		The Java code (as a super-class) for the LispButton
		Java Bean example.  Some of the methods make more 
		sense in Java since writing them in Lisp would
		add nothing to the example.

bean1gen.cl	The Lisp code that generates the classes LispButton,
		LispBean, and LispBeanBeanInfo.

bean1.cl	The lisp implementation of the methods in LispButton,
		LispBean, and LispBeanBeanInfo.


setjv.bat	A sample Windows script to set environment variables
		used by the other scripts.  MUST be modified to fit
		the site where it is used.


prep.bat	A Windows script to prepare the sample application to 
		run with an installed BDK.


run.bat		A Windows script to run the sample application.


run.cl		A Lisp file used by the above script.


