#!/bin/csh

# $Id: domany.csh,v 5.0 2004/01/14 18:31:35 layer Exp $

./setjava.csh

echo Next step: start ACL

./call-acl.csh -L many-applets.cl&

echo Next step: start Appler Viewer
set x=$<
appletviewer applet.htm&

echo Next step: start a browser and throw in embed.htm
set x=$<
./call-brw.csh  embed.htm

