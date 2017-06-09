


;; $Id: Readme.txt,v 5.0 2004/01/14 18:31:35 layer Exp $


Some very simple and basic examples of jLinker programming
==========================================================

 o Calling Lisp from Java
 o Calling Java from Lisp - the StringTokenizer Example - Funcall Model
 o Calling Java from Lisp - the StringTokenizer Example - Class Model
 o Calling Java from Lisp, but Java connects independently


==========================================================

 o Calling Lisp from Java

	TryJavaToLisp.java
	tryjtol.cl

	1. make sure Java can see jlinker.jar (CLASSPATH)
	2. compile TryJavaToLisp.java with javac
	3. start ACL in this folder
	4. in ACL:	:ld tryjtol
	5. in ACL:	(advertise)
	6. start Java:	java TryJavaToLisp
	
	Java will call Lisp, print the answer, and exit.

	You can stay in Lisp and repeat steps 5 and 6.
	Variations: :tr test-work   to see that Lisp gets called


==========================================================

 o Calling Java from Lisp - the StringTokenizer Example - Funcall Model

	token1.cl

	1. copy and customize the file jl-config.cl
	2. copy jlinker.jar
	3. start ACL in this folder
	4. in ACL:	:ld token1
	5. in ACL:      (run-tokenizer)

==========================================================

 o Calling Java from Lisp - the StringTokenizer Example - Class Model

	token2.cl

	1. copy and customize the file jl-config.cl
	2. copy jlinker.jar
	3. start ACL in this folder
	4. in ACL:	:ld token2
	5. in ACL:      (run-tokenizer)

==========================================================

 o Calling Java from Lisp, but Java connects independently

	token3.cl
	ConnectToLisp.java

	1. make sure Java can see jlinker.jar (CLASSPATH)
	2. compile ConnectToLisp.java with javac
	3. start ACL in this folder
	4. in ACL:	:ld token3
	5. in ACL:      (run-tokenizer)
			;; Lisp will hang until Java gets started
	6. start Java:	java ConnectToLisp
	7. in ACL:	;; result is printed after Java connects
	8. in ACL:      ;; call (run-tokenizer) with other arguments
	9. in ACL:	;; after five minutes, Java disconnects
			;; and further calls to Java fail.

==========================================================

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

