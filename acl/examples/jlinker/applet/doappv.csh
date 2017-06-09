#!/bin/csh

# $Id: doappv.csh,v 5.0 2004/01/14 18:31:35 layer Exp $

# Run the Applet viewer

source ./setjava.csh

echo 'First step: start Appler Viewer (in background)'
appletviewer applet.htm&

echo Next step: start ACL
./call-acl.csh -L one-applet.cl

