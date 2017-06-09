#!/bin/csh

# $Id: setjava.csh,v 5.0 2004/01/14 18:31:35 layer Exp $

# set PATH appropriately for local Java version
# (in this case we call some script setjv12 - maybe to set the
#     variables for a Java 1.2 installation)
# CHANGE THIS TO SOMETHING THAT WILL WORK
#setjv12

# CHANGE the source folder to point to the installed location
#    of the file jlinker.jar
cp  /bg/acl60/acl60/jlinker/jlinker.jar . 

# The following should work without change if CLASSPATH is
#     normally null (the Java 1.2 default).
#     If CLASSPATH already is set, you will need something like this:
#     set CLASSPATH=.:./jlinker.jar:$CLASSPATH
setenv CLASSPATH .:./jlinker.jar

