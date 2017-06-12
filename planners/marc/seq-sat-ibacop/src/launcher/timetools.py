#!/usr/bin/python2.7
#
# timetools.py
# Description: time management
# -----------------------------------------------------------------------------
#
# Started on  <Tue Mar  8 11:54:22 2011 Carlos Linares Lopez>
# Last update <Thursday, 06 October 2011 00:34:54 Carlos Linares Lopez (clinares)>
# -----------------------------------------------------------------------------
#
# $Id:: timetools.py 296 2011-10-05 23:16:57Z clinares                       $
# $Date:: 2011-10-06 01:16:57 +0200 (jue 06 de oct de 2011)                  $
# $Revision:: 296                                                            $
# -----------------------------------------------------------------------------
#
# Made by Carlos Linares Lopez
# Login   <clinares@korf.plg.inf.uc3m.es>
#

# -----------------------------------------------------------------------------
#     This file is part of IPCData
#
#     IPCData is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     IPCData is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with IPCData.  If not, see <http://www.gnu.org/licenses/>.
#
#     Copyright Carlos Linares Lopez, 2011
# -----------------------------------------------------------------------------

"""
time management
"""

from __future__ import with_statement

__version__  = '1.1'
__revision__ = '$Revision: 296 $'

# imports
# -----------------------------------------------------------------------------
import time                     # time management

# -----------------------------------------------------------------------------
# Timer
#
# this class creates a block to be used within a with statement. It
# exactly measures the time between the entry and exit points of the
# with block
# -----------------------------------------------------------------------------
class Timer(object):

    """
    this class creates a block to be used within a with statement. It exactly
    measures the time between the entry and exit points of the with block
    """

    def __enter__(self):
        self.__start = time.time()

    def __exit__(self, type, value, traceback):
        self.__finish = time.time()

    def elapsed (self):
        return self.__finish - self.__start


# Local Variables:
# mode:python2.7
# fill-column:80
# End:
