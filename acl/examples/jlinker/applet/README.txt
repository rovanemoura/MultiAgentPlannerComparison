
;; $Id: README.txt,v 5.0 2004/01/14 18:31:35 layer Exp $

Contents:

* What is this?
* TO RUN THE APPLET DEMO (on Windows):
* TO RUN THE APPLET DEMO (on Unix):
* FILES THAT DEFINITELY NEED TO BE CUSTOMIZED BEFORE USE:
* Files used to run the applet examples:

-----------------------------------------------------------

* What is this?

   The files in this directory define several examples of Java applets 
   that communicate with Lisp through the jLinker interface.



* TO RUN THE APPLET DEMO (on Windows):

	1. Customize the three .bat files listed below.

	2. Run compjava.bat to compile the Java files.

	3. Run doappv.bat to run the applet in the Java applet viewer.

		3.1. The Lisp window will open first with messages
			indicating Lisp is waiting for a connection
		3.2. Press a key in the command window at the pause
		3.3. The applet viewer window will open and start showing
			some travelling circles
		3.4. In the Lisp window:
			Call (dd) to see the current circle parameters
			Call (dd width height) to change the circle to
				an ellipse
		3.5. Close the Applet viewer window
		3.6. Close the Lisp window.

	4. Run dobrows.bat to run the same applet in a browser

	   TO RUN THE APPLET DEMO IN A BROWSER, YOU WILL NEED THE JAVA
	   browser plugin (obtained from sun.com or from the browser site).

	   The command window will pause before starting the browser.


	5. Run domany.bat to run two applets simultaneously. 

	   The command window will pause before starting the applet viewer
	   and again before starting the browser.


* TO RUN THE APPLET DEMO (on Unix):

	1. Customize the three .csh files listed below 
	    and make sure they  and the other .csh files have execute permission.

	2. Run compjava.csh to compile the Java files.

	3. Run doappv.csh to run the applet in the Java applet viewer.

		3.1. The applet viewer window will open and wait for Lisp
		3.2. The Lisp window will open next with messages
			indicating Lisp is waiting for a connection
		3.3. The applet viewer window will start showing
			some travelling circles
		3.4. In the Lisp window:
			Call (dd) to see the current circle parameters
			Call (dd width height) to change the circle to
				an ellipse
		3.5. Close the Applet viewer window
		3.6. Close the Lisp window.

	4. Run dobrows.csh to run the same applet in a browser

	   TO RUN THE APPLET DEMO IN A BROWSER, YOU WILL NEED THE JAVA
	   browser plugin (obtained from sun.com or from the browser site).


	5. Run domany.csh to run two applets simultaneously. 

	   The command shell will pause before starting the applet viewer
	   and again before starting the browser.




* FILES THAT DEFINITELY NEED TO BE CUSTOMIZED BEFORE USE:

	These files in the distribution are just sample files - if they
	happen to work in some installation, it is only by accident.

	setjava.bat	- called by other scripts to set PATH and CLASSPATH
	setjava.csh

	call-acl.bat	- called by other scripts to start ACL
	call-acl.csh

	call-brw.bat	- called by other scripts to start a web browser
	call-brw.csh		  on a given file




* Files used to run the applet examples:

	LispApplet.java	- Java class: 

	public class LispApplet extends java.applet.Applet;

	This class implements an iterface between an applet instance 
	and a Lisp jLinker peer. 

	Before using this class, it may be necessary to modify some of 
	the connection parameters set in static members of the
	LispConnector class.


	LispApplet2.java - Java class: 

	A second LispApplet class with a different name.


	one-applet.cl	- Lisp code to run one Applet from the
			  Applet Viewer or a browser.
		
			The function (dd [w [h]]) may be called to
			change the width and height parameters for the
			circles - to show that Lisp is in control of the
			picture drawn by the applet.


	many-applets.cl	- Lisp code to run many applets and many
			  invocations of the same applet from many
			  sources.


	compjava.bat	- compiles the sample applet java files
	compjava.csh

	doappv.bat	- run the sample applet in Applet Viewer
	doappv.csh	  where it will be used.

	dobrows.bat	- run the sample applet in a browser
	dobrows.csh

	domany.bat	- run Applet Viewer and browser simultaneously
	domany.csh		  to show multiple applets connected to one Lisp


	applet.htm	Invoke LispApplet from an <applet> tag

	embed.htm	Invoke LispApplet from an <embed> or an <object> tag

	embed2.htm	Invoke LispApplet2 from an <embed> or an <object> tag

	embed3.htm	Try to invoke both LispApplet and LispApplet2 from the
			same browser.


