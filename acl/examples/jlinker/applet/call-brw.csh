#!/bin/csh

# $Id: call-brw.csh,v 5.0 2004/01/14 18:31:35 layer Exp $

# Start a web browser on a file

# THE FOLLOWING STATEMENT MUST BE MODIFIED
#     to call a wb browser on the relative pathname in $1
netscape file:`pwd`/$1

