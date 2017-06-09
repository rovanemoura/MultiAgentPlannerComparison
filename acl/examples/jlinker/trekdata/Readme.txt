

;; $Id: Readme.txt,v 5.0 2004/01/14 18:31:35 layer Exp $

Contents:

* What is this?
* What kind of preparation is needed?
* Run one user in a dedicated Java VM
* Run one or more intranet users from web browsers
* Run ons or more internet users with optimized code
* What are all the files? 
* Prerequisites

-----------------------------------------------------------

* What is this?

This directory contains a sample jLinker application that may be 
run as a stand-alone application, or as an applet in a web browser.
The applet version can support multiple simultaneous users.

The main screen shows a scrollable and searchable database of trivia
about the original Star Trek episodes.  The input fields allow you to
add comments to the database.  The comments may be saved (at the
server) from one session to the next.

As the mouse moves around the window, the status area at the bottom
edge is updated by local Java code.  Button presses are sent back to
Lisp for handling there, hence the greater delay.

The function (verbose [boolean]) turns messages on or off. 
The messages show stuff happening in Lisp while buttons are
pressed in the Java window.

The function (colors [name [r g b]]) changes the color values
used for several window components.  It shows how Lisp can 
modify the parameters of the Java application.  New colors
take effect when a new session is started in the browser or
by calling (run).

The application has been tested with Netscape 4.7 and MSIE 5.0.


FEATURES:

	- Lisp handles the data base
	- Java handles the GUI
	- can run as an Applet 
	- can run directly     
	- Lisp is portable
	- Java is portable

	- Can modify Lisp code while window is up
	  to change behavior of window



-------------------------------------------------------------------------------
SIDEBAR: What does it mean "start ACL in folder foo"?

How to "start ACL in folder foo"?

The goal of this step is to have ACL running with the current default
folder (directory )in ACL being the folder (directory) foo.  This kind
of operation is useful because after this step, files in folder foo
can be manipulated without any folder (directory) prefixes that make
examples long and hard to read.



Windows:

There are several ways to reach this goal:

 1. Select one of the ACL executables from the Start Menu
    AND THEN, in the Lisp console window or the Lisp debug
    window, enter the command

          :cd full-path-to-folder

 2. Open a DOS Command Prompt Window and then enter the commands

          cd full-path-to-folder
          full-path-to-ACL-executable

 3. Create a shortcut to ACL in the working folder (foo) and
    make sure that the "Start in" line on the shortcut Properties
    window is blank.

    Double-click on the shortcut.

 4. Create a shortcut to ACL in some convenient place, and
    make sure that the "Start in" line on the shortcut Properties
    window points to the folder foo.

    Double-click on the shortcut.


Unix:

 1. Select one of the ACL executables and start it from a
    command shell,  AND THEN, at the Lisp console prompt,
    enter the command

          :cd full-path-to-folder

 2. In a suitable shell window or buffer, enter the commands

          cd full-path-to-folder
          full-path-to-ACL-executable

-------------------------------------------------------------------------------



* Run one user in a dedicated Java VM

   Prep Work:

	- customize prep-slave.bat (point to Java and to ACL)
	- run prep-slave.bat to copy and compile files
	- customize jl-config.cl (point to Java)
	  It needs to point to the Java home directory.


   To run the demo directly from Lisp:

	- call run-slave.bat

	OR

	- start ACL in this folder
	- :cl t-java
	- (run)




* Run one or more intranet users from web browsers

Prep Work:

	- customize prep-local.bat (point to Java and to ACL)
	- run prep-local.bat to copy and compile files
	- customize jl-config.cl (point to Java)


To run the demo from a browser(run.bat):

	- call run-local.bat

	OR

	- start ACL in this folder
	- :cl t-local
	- (enable-applets)

	- start a browser on file t-local.htm









* Run one or more internet users with optimized code

  OPTIMIZED for internet version - shift work to Java, reduce net traffic

  This program was initially created to demonstrate jLinker in a
  closely-coupled situation where Lisp and Java are running on the same
  machine.  The Lisp code naively used the awt library as if it were a
  local Lisp library.  In the one-host situation, screen creation and
  scrolling response are nominally instantaneous.

  In this internet demonstration, the round-trip delay for even a single
  Lisp-Java interaction is the overwhelming component.  This is a
  modified version of the original application to reduce the number of
  calls form Lisp to Java by adding some Java code that performs
  commonly used sequences of calls.  The main screen creation still uses
  about 200 calls from Lisp to Java, while a scrolling operation takes
  only 15 calls.  Any practical internet applet should be mostly
  self-contained; communication with the Lisp host should be limited to
  a few round-trips per user interaction.

  Prep Work:

	- customize prep-inet.bat (point to Java and to ACL)
	- run prep-inet.bat to copy and compile files
	- customize jl-config.cl (point to Java)

	- to test the applet on one machine, use the file t-inet.htm
	  unchanged;  but to run the applets on machines other than
	  the Lisp server, modify the file t-inet.htm:

			replace "localhost" with "xxx" twice
			replace "4321" with "yyy" twice
			replace "-4331 with "-zzz" twice

	  where xxx is the hostname of the machine running the Lisp server,
	  yyy and zzz are the ports where Lisp is listening.

	  If ports 4321 and 4331 are not suitable, use the form

		(enable-applets :lisp-port yyy :java-port -zzz)

	  or modify the defaults in file t-inet.cl.

	- Files that the browser needs to see
	  (copy all to the same directory as t-inet.htm):

                t-inet.htm
                jlinker.jar     
                AnnotatorPanel.class
                JLTextListener.class
                TrekAppletInet.class
                TrekAppletInet$1$MAplus.class

  To run the demo from a browser(run.bat):

	- call run-inet.bat

	OR

	- start ACL in this folder
	- :cl t-inet
	- (enable-applets)

	- start a browser on file t-inet.htm



  To run the demo directly from Lisp:

	- CUSTOMIZE the file jl-config.cl (in this folder).
	  It needs to point to the Java home directory.

	- start ACL in this folder
	- :cl t-inet
	- (run)


* What are all the files? 

      Readme.txt

      run-acl.bat

      prep-slave.bat
      run-slave.bat
      run-slave.cl

      prep-local.bat
      run-local.bat
      run-local.cl

      prep-inet.bat
      run-inet.bat
      run-inet.cl
      run-prep-inet.cl

      AnnotatorPanel.java
      JLTextListener.java
      TrekAppletInet.java
      TrekAppletLocal.java
      java.policy

      t-data.cl
      t-inet.cl
      t-local.cl
      trek.cl

      t-local.htm
      t-inet.htm

      userdata
      webpart


      AnnotatorPanel.class
      JLTextListener.class
      TrekAppletLocal.class

      TrekAppletInet.class
      TrekAppletInet$1$MAplus.class





* Prerequisites

  You need the Java 1.2 PlugIn available at the Sun MicroSystems Java site:

    http://java.sun.com/products/plugin/1.2/plugin-install.html

  To run the applet accross the internet, the Lisp server must listen at sockets
  that may be connected through a firewall.  The applet security environment 
  must be modified to allow connection to sockets on the internet.

    o This means that you must allow an applet to connect
        to a pair of sockets on the Lisp server machine.

    o There is a file "java.policy" that needs to be updated with an
        appropriate permission entry.  
	If there is only one file, that is the one to update.
	If there are more than one, the one in a JRE subtree may
	be the likely target, but some trial and error may be required.

    o The file may be edited with Notepad or Emacs to add the following 3 lines


   grant {
    permission java.net.SocketPermission "spider.franz.com:9385-9386", 
    "connect, resolve";
    };

      where host name and socket ports are renamed appropriately.


    o It is also possible to use the JDK (Java Develper ToolKit)     
      policytool  program to modify the policy file.

      Use the tool to
      add a  SocketPermission   entry with components: 

             target:  spider.franz.com:9385-9386
             actions: "connect,resolve"

    o It may be necessary to reboot to make the permissions effective.

    o On one Windows machine, Java 1.2 JRE is installed at

      c:\mmProgs\JRE\1.2\   and the corresponding policy

      file was

      c:\mmProgs\JRE\1.2\lib\security\java.policy


