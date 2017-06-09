What is this?

The files in this directory demonstrate how a large computation can be
subdivided into a number of tasks that can run in parallel on several
processors on a network or a cluster computer.

The application code was contributed by Dr. Larry Hunter at the
University of Colorado.

=========================================================================
CONTENTS:

 * List of Files
 * The Application
 * The Task Manager
 * How to Run the Application on One Processor
 * How to Run the Application on a Network
 * How to Run the Application on a Cluster

=========================================================================

* List of Files

   load.cl           - Load this file to start the application.
   run               - A script used to start sub-tasks.
		       THIS SCRIPT MUST BE MODIFIED BEFORE it may be used.
   task-level.cl     - Task management functions that may be useful
                       with more than one application.
   top-level.cl      - Some sample top-level calls to run the
		       application in various modes.

   parallel5.cl      - The Application.

   lex-string.cl     - Utility functions called by the application.

   gene2pubmed.data  - This file is not included. It is downloaded
                       by the application.  Since the data at the source 
		       changes over time, it is useful to run with a local 
		       copy when it is important to use the same data
		       each time.

=========================================================================

 * The Application

This application calculates all the pairwise distances between a list
of genes, based on co-mentions in articles in medline.  Medline is a
database that can be downloaded from a public ftp site at
ftp.ncbi.nlm.nih.gov.

For human genes (about 25000), this involves testing over 75,000,000
pairs.  On a 3MHz processor, this takes over 10 minutes.

The task is partitioned into subtasks by subdividing the triangular
search space into smaller non-overlapping triangles and rectangles of
approximately equal area.  Each sub-task scans the entire database to
prepare for the scan but this part only takes one minute on the 3MHz
processor.  Thus it may possible to do the 10 minute task in 3 minutes
on 11 3MHz processors:

   1 minute on the control processor to scan the database
     and prepare the sub-tasks
   1 minute on each of the sub-processors to scan the
     database again
   1 minute on each of the sub-processors to scan 1/10 of
     the pair space

(We actually have not tested this since we do not have access to
 a network of 11 comparable speed processors.)


=========================================================================

 * The Task Manager

=========================================================================

 * How to Run the Application on One Processor

Start by downloading a copy of the database with
  
  (download [max])

Since the entire database is over 30Mb, it may make sense to limit the
number of records downloaded by specifying a max argument.  If max is
less than 250000, there seems to be very little for the algorithms to
work with.


Run the application on a single processor:

  (run [:max max])


=========================================================================

 * How to Run the Application on a Network

Download the database to a local file.  Since the database changes
dynamically, all the parallel sub-tasks must use a stable copy.  The
local data file must be in a shared filesystem visible to all the
sub-processors, or it must be copied to each separate filesystem.

Prepare a "run" script for each sub-processor.  The "run" script must
establish an appropiate directory default and start the ACL executable
with the arguments supplied to the script.  The task manager assumes
that the required program and data files are all visible in the
directory where the ACL executable is started.

The sub-tasks do not write any data to files; therefore, in a shared
filesystem, all the sub-tasks may use the same starting directory.
However, if the sub-processors are not all of the same type, separate
directories are needed to hold distinct fasl files.

Update the *remote-hosts* value in top-level.cl.

  (prun [:max max])

=========================================================================

 * How to Run the Application on a Cluster

We do not have access to a cluster.

Dr. Hunter tells us that all that is needed is to supply the :host
argument in the call to prun because (short-site-name) does not return
the correct value.  The :host argument must be the host name that
sub-tasks must use in order to reach the controlling process.


=========================================================================
