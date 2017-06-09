#!/bin/csh

# $Id: dobrows.csh,v 5.0 2004/01/14 18:31:35 layer Exp $

source ./setjava.csh

echo First step: start browser on:   embed.htm
./call-brw.csh embed.htm&


echo Next step: start ACL
./call-acl.csh -L one-applet.cl


