[This example software in this directory is provided "as is" by a third party 
without warranty of any kind].


		DIRET 1.0 DOCUMENTATION
		Vladimir Kulyukin
		Department of Computer Science
		DePaul University
		vkulyukin@cs.depaul.edu
		=======================

This document contains a brief outline of DIRET 1.0. The 
name "DIRET" stands for "Distributed Information RETrieval." 
The objective of the document is to give the reader an 
understanding of how DIRET works and explain how to run 
the implemented demos. 

The document is not meant to serve as a comprehensive guide 
to the ideas and techniques underlying the system. The reader 
is encouraged to consult the references given at the end of 
the document.

Credits
-------

The current version of the system was designed and developed
by Vladimir Kulyukin of DePaul University. Dr. Kulyukin can
be reached at vkulyukin@cs.depaul.edu. Alex Lifshits deserves 
credit for testing several Common Graphics GUI's. Other 
acknowledgements and copyrights can be found in the source 
files.

Copyright
---------

Copyright (c) 1999 by Vladimir Kulyukin

Permission to use, copy, modify, and distribute this software and 
its documentation without fee for any purpose involving research and/or 
education is hereby granted, provided that this copyright and permission 
notice appear in all copies and supporting documentation. Permission to
use, copy, modify, and distribute this software and its documentation
for any commercial purpose requires explicit written permission of the 
author.

The author makes no representations about the suitability of this 
software for any purpose.  It is provided "as is" without express or 
implied warranty. All warranties, including all implied warranties 
of merchantability and fitness, are hereby disclaimed. In no event, 
shall the author be liable for any special, indirect or consequential 
damages or any damages whatsoever resulting from loss of use, data 
or profits, whether in an action of contract, negligence or other 
tortious action, arising out of or in connection with the use or 
performance of this software.

Compiling
---------

To compile the software: 

We assume you have write permission in this directory. If not, copy the
entire diret1.0 directory to a safe directory in which you have write permission.

Start the ACL 5.0 Enterprise Edition/Windows "lisp only" prompt, and :cd to this directory .

:ld compile-diret.lisp
(compile-diret) ; A number of warning messages will be printed; these may be ignored.
:exit

The system is now compiled.



Required Files
--------------

The system is implemented in ACL5.0 Enterprise Edition/Windows 
and requires the Allegro ORBLink 1.0.1 addon. The source code
is written in standard CLOS.

The demos discribed below have been run on Windows NT 4.0 (Service 
Pack 3) and Windows 98. The system has not been tested on any other 
platform.

The complete list of files for the system is as follows:

1)  diret.idl
2)  diret-server.lisp
3)  diret-server.fasl
4)  diret.lisp
5)  dmapvs-loader.fasl
6)  dmapvs-loader.lisp
7)  utils.fasl
8)  utils.lisp
9)  deftable.fasl
10) deftable.lisp
11) frame-manager.fasl
12) frame-manager.lisp
13) dmap.fasl
14) dmap.lisp
15) dmaprr.fasl
16) dmaprr.lisp
17) dmapvs.fasl
18) dmapvs.lisp
19) diret-client.lisp
20) diret-client.fasl
21) diret-control-panel.lisp
22) diret-control-panel.fasl
21) diret-control-panel-functions.lisp
22) diret-control-panel-functions.fasl
22) diret-subscription.lisp
23) diret-subscription.fasl
24) diret-subscription-functions.lisp
25) diret-subscription-functions.fasl
26) diret-retrieval-dialog.lisp
27) diret-retrieval-dialog.fasl
28) diret-retrieval-functions.lisp
29) diret-retrieval-functions.fasl
30) dmapvs-loader.lisp
31) dmapvs-loader.fasl
32) diret-gui-loader.lisp
33) diret-gui-loader.fasl

The data for the system is in the data subdirectory and contains the 
following free-text files:

1)  common-stock-funds.txt
2)  growth-and-income-funds.txt
3)  small-company-funds.txt
4)  equity-income-funds.txt
5)  growth-funds.txt
6)  emerging-market-funds.txt
7)  international-mutual-funds.txt
8)  myths-about-indexing.txt
9)  investment-grade-corporate-bond-funds.txt
10) municipal-tax-free-bond-funds.txt
11) us-treasure-and-government-bond-funds.txt
12) bond-funds.txt
13) basics-of-bonds.txt
14) morgage-backed-securities-funds.txt
15) bond-funds-risks.txt
16) investing-in-individual-bonds.txt
17) money-markets.txt
18) bear-market-causes.txt
19) bear-market-survival.txt
20) bear-markets.txt
21) dollar-cost-averaging.txt
22) financing-college.txt
23) mutual-funds-costs.txt
24) mutual-funds-and-taxes.txt
25) past-bear-markets.txt
26) readiness-for-bear-markets.txt
27) sample-document1.txt
28) sample-document2.txt
29) sample-document3.txt

Introduction
------------

The system consists of two components: the client and the server.
The current implementation requires that each Lisp image have at 
most one client or one server. The "or" is exclusive.

A DIRET server is a CORBA-based search engine that manages a collection 
of free-text documents and answers free-text queries pertaining to 
their content. The server publishes its retrieval interface via IDL 
and the files of interoperable object references (IOR's). DIRET servers 
are referred to as information sources.

A DIRET client is embedded in a text processor. The client collects 
background samples of what the user is typing. The samples are
transformed into queries. When a query is obtained, the client 
decides which remote information sources are relevant to it. The
query is sent to those sources, and the received retrievals, if there 
are any, are stored locally. The user can inspect the retrievals at 
his convenience. Thus, the retrieval of pertinent information occurs 
as a by-product of routine activities. The current domain of DIRET 1.0 
is mutual fund investment.

To enable the DIRET client to communicate with information sources, 
the user goes through the source subscription when the system is made 
operational for the first time. During the source subscription, the user 
specifies which information sources he user wants to be in touch with.

How to Make DIRET 1.0 Operational
---------------------------------

The file diret.lisp contains the global parameters used by the system. 
The explanations below assume that the system is stored in the
directory "d:\\programming\\lisp\\diret1.0\\. 

The parameter *diret-dir* in diret.lisp specifies the directory with
all of the system's source and data files. If necessary, the parameter 
can be explicitly set to the required directory. If the parameter is 
modified, diret.lisp should be recompiled.

To run the system's demo requires four Common Lisp images. Three 
of those images run DIRET servers. The fourth image runs a Diret 
client.

Starting The Stocks DIRET Server
--------------------------------

To start the Stocks Diret server, start the Allegro CL 5.0 Enterprise
Edition (lisp only) image. The IDE image can also be used. However,
since the server class does not have any GUI's associated with it, the 
lisp only image is more appropriate. When the Listener is up, follow 
these interactions:

USER(1): :cd d:/programming/lisp/diret1.0/
d:\programming\lisp\diret1.0\

USER(2): :ld diret-server
; Fast loading d:\programming\lisp\diret1.0\diret-server.fasl
;   Fast loading d:\programming\lisp\diret1.0\diret.fasl
;   Fast loading d:\programming\lisp\diret1.0\dmapvs-loader.fasl
;     Fast loading d:\programming\lisp\diret1.0\utils.fasl
;     Fast loading d:\programming\lisp\diret1.0\deftable.fasl
;     Fast loading d:\programming\lisp\diret1.0\frame-manager.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmap.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmaprr.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmapvs.fasl
;   Fast loading C:\Program Files\acl50\code\ORBLINK.fasl
;     Loading C:\Program Files\acl50\code\orblink-configure.cl

USER(3): (start-server :stocks)
Wrote ior to file: stocks-dir-inx.ior
Wrote ior to file: stocks-dir-ret.ior
#<DIRET-SERVER @ #x205a201a>

The files "stocks-dir-inx.ior" and "stocks-dir-ret.ior"  are the two IOR 
files each DIRET client uses to communicate with the server. The function
returns a DIRET server object which becomes the value of the variable
*diret-server*.

This DIRET server answers queries about mutual funds that invest in 
common stocks. To make sure that the server is properly running in 
the Lisp image, evaluate (diret-server-active-p), which should return 
true. 

One can inquire what server is running in the current Lisp image by
evaluating (respond-to-describeSelf *diret-server*). The call returns a 
string describing the document collection of the server, i.e., 
"Collection of documents on stock mutual funds." 

Starting the Bonds Diret Server
-------------------------------

To start the Bonds Diret server, start another ACL5.0 Enterprise 
Edition (lisp only) image and follow the the following interactions 
with the Listener:

USER(1): :cd d:/programming/lisp/diret1.0/
d:\programming\lisp\diret1.0\

USER(2): :ld diret-server
; Fast loading d:\programming\lisp\diret1.0\diret-server.fasl
;   Fast loading d:\programming\lisp\diret1.0\diret.fasl
;   Fast loading d:\programming\lisp\diret1.0\dmapvs-loader.fasl
;     Fast loading d:\programming\lisp\diret1.0\utils.fasl
;     Fast loading d:\programming\lisp\diret1.0\deftable.fasl
;     Fast loading d:\programming\lisp\diret1.0\frame-manager.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmap.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmaprr.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmapvs.fasl
;   Fast loading C:\Program Files\acl50\code\ORBLINK.fasl
;     Loading C:\Program Files\acl50\code\orblink-configure.cl

USER(3): (start-server :bonds)
Wrote ior to file: bonds-dir-inx.ior
Wrote ior to file: bonds-dir-ret.ior
#<DIRET-SERVER @ #x205a0efa>

USER(4): (diret-server-active-p)
T

This server answers free-text queries on mutual funds that invest 
in bonds.

Starting the Cash Management DIRET Server
-----------------------------------------

After the Bonds DIRET server is running, start the third Lisp 
image and follow these interactions in the Listener:

USER(1): :cd d:/programming/lisp/diret1.0/
d:\programming\lisp\diret1.0\

USER(2): :ld diret-server
; Fast loading d:\programming\lisp\diret1.0\diret-server.fasl
;   Fast loading d:\programming\lisp\diret1.0\diret.fasl
;   Fast loading d:\programming\lisp\diret1.0\dmapvs-loader.fasl
;     Fast loading d:\programming\lisp\diret1.0\utils.fasl
;     Fast loading d:\programming\lisp\diret1.0\deftable.fasl
;     Fast loading d:\programming\lisp\diret1.0\frame-manager.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmap.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmaprr.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmapvs.fasl
;   Fast loading C:\Program Files\acl50\code\ORBLINK.fasl
;     Loading C:\Program Files\acl50\code\orblink-configure.cl

USER(3): (start-server :cash)
Wrote ior to file: cash-dir-inx.ior
Wrote ior to file: cash-dir-ret.ior
#<DIRET-SERVER @ #x205a376a>
USER(4): (diret-server-active-p)
T

This server answers queries on mutual funds that invest in
various cash management instruments.

II.d) Starting the DIRET Client
-------------------------------

To start the Diret Client, start the ACL5.0 Enterprise Edition 
(with IDE). The IDE is needed because the client uses Common Graphics. 
If the servers are not running on the same machine with the client,
the client must have access to each server's IORs. Follow these 
interactions in the Debug Window:

> :cd d:/programming/lisp/diret1.0/
d:\programming\lisp\diret1.0\

> :ld diret-client
; Fast loading d:\programming\lisp\diret1.0\diret-client.fasl
;   Fast loading d:\programming\lisp\diret1.0\diret.fasl
;   Fast loading d:\programming\lisp\diret1.0\dmapvs-loader.fasl
;     Fast loading d:\programming\lisp\diret1.0\utils.fasl
;     Fast loading d:\programming\lisp\diret1.0\deftable.fasl
;     Fast loading d:\programming\lisp\diret1.0\frame-manager.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmap.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmaprr.fasl
;     Fast loading d:\programming\lisp\diret1.0\dmapvs.fasl
;   Fast loading C:\Program Files\acl50\code\ORBLINK.fasl
;     Loading C:\Program Files\acl50\code\orblink-configure.cl
;   Fast loading d:\programming\lisp\diret1.0\diret-gui-loader.fasl
;    Fast loading d:\programming\lisp\diret1.0\diret-control-panel.fasl
;    Fast loading d:\programming\lisp\diret1.0\diret-control-panel-functions.fasl
;     Fast loading d:\programming\lisp\diret1.0\diret-subscription.fasl
;     Fast loading d:\programming\lisp\diret1.0\diret-subscription-functions.fasl
;     Fast loading d:\programming\lisp\diret1.0\diret-retrieval-dialog.fasl
;     Fast loading d:\programming\lisp\diret1.0\diret-retrieval-functions.fasl

> (diret)
t

The DIRET Control panel appears. Since there are no subscriptions currently 
available to the client, click on subscriptions, and subscribe to the three 
information sources, i.e., the three Diret servers that brought up previously. 
Subscribe to all three: stock mutual funds, bond mutual funds, and cash mutual 
funds. 

Subscription to a source may take a few seconds, especially when the servers
are running on three different machines, since the client must contact the 
appropriate server and receive all it needs to know about it. Wait for the 
message "Subscription has been accepted" in the lower message window.

After all three sources are subscribed to, delete the subscription window.
Take a look at the buttons in the Diret control panel. The editor button
brings up a text window, where the user types documents. Do not type anything 
there yet. The retrievals button brings out a window displaying current 
retrievals. Since nothing has been retrieved yet, no retrievals are displayed. 

The DocProcessor button is the one needed for the demos. Click on it. You should 
see the three sample documents, sample-document1.txt, sample-document2.txt, and 
sample-document3.txt. The chosen document is being displayed in the Editor as if 
typed by the user. Every so often the system asks the user if he wants to inspect 
the retrievals obtained so far. This simulates what the user is expected to do, 
as he works on the document: write some text, inspect the documents found so far, 
write some more, etc.

To make sure that the client is, indeed, running in the background, take a look 
at the value of sys:*all-processes*. One of the processes is #<multiprocessing:process 
Diret1.0 Query Engine(2dd) Sleeping @ #x20bc02ea>. The Diret query engine is 
sampling the user's text from the editor's window and turns them into queries 
sent to information sources.

Choose sample-document1.txt and watch the text of that document being 
displayed in the editor window. When the system asks you for retrievals, 
answer yes or no. If you answer yes, the retrievals dialog is brought up 
with the relevant documents found so far displayed in it. When you click 
on Describe, a brief description of that document appears in the lower 
window. When you click on Examine, the full text of the document is 
displayed in a separate window. Another strategy is to answer no until
the system is done with the document and then click on Retrievals.

Neither the description nor the text of the document are stored locally. The 
client request them from the appropriate information source and, after their 
reception, unmarshalls and displays them locally.

The retrievals from the remote sources are stored locally in the file user-retrievals.txt.
Each entry in that file is an s-expression. The first element of the s-expression is 
the name of the source from which the retrieval was received. For example, :BONDS 
means that the retrieval was received from the bond collection. The second element is 
the name of the string containing the text of the query in response to which the retrieval 
was made. The third element is another s-expression that has the id of the document on 
the remote server retrieved in respond to the query, the similarity coefficient, and a 
brief description of that document. Here are two sample entries:

(:BONDS "Of course, prices don't always rise. In a period of deflation (in 
         other words, a time of falling prices), the principal amount of a 
         Treasury inflation-indexed security might be reduced. However, when 
         the principal amount is repaid at maturity, the investor will receive 
         the larger of the inflation-adjusted principal"
 ("6" 0.39909714
  "Risks of Investing in Bond Funds"
  )
)
(:CASH 
  "of investment-grade corporate bond funds, they do not match the 
   quality level of  U.S. government bond funds. As a result, corporate 
   bond funds generally pay higher  yields than U.S. government bond 
   funds. Some bond funds also may be exposed to event risk, the possibility 
   that some "
  ("0" 0.43343994
   "Basics of Money Markets"
  )
)

III) Contact Information
-------------------------

Send questions, comments, and bugs to vkulyukin@cs.depaul.edu.

IV) References
--------------

For more details, you can consult the following references:

Kulyukin, V. 1999. Application-Embedded Retrieval from Distributed
Free-Text Collections. In Proceedings of AAAI-99.

Kulyukin, V. 1998. FAQ Finder: A Gateway to Newsgroups' Expertise. 
In Proceedings of the 40th Conference of Lisp Users.

Kulyukin, V. 1998. Question-Driven Information Retrieval Systems. 
Ph.D. diss., Dept. of Computer Science, The University of Chicago.

Kulyukin, V.; Hammond, K.; and Burke, R. 1998. Answering Questions 
for an Organization Online. In Proceedings of AAAI-98.

Kulyukin, V.; Hammond, K.; and Burke, R. 1996. Automated Analysis 
of Structured Online Documents. In Proceedings of the AAAI-96 
Workshop on Internet-Based Information Systems.


