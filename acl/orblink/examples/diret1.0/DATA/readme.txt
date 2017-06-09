DIRET 1.0 README
================

I) Required Files
-----------------

This file contains the list of files for the Diret1.0 system and 
instructions on how to make the system operational. "Diret" stands 
for "Distributed Information Retrieval." The system is implemented
in ACL5.0 Enterprise Edition/Windows and requires the Allegro ORBLink
1.0.1 addon. The demo discribed below has been run on Windows NT 4.0 
(Service Pack 3) and Windows 98. The complete list of files for the 
system is as follows:

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

The data for the system is in the data subdirectory and contains 
the following free-text files:

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

II) Introduction
----------------

The system consists of two components: the client and the server.
The current implementation requires that each Lisp image have at 
most one client or at most one server. The "or" in the previous 
sentence is exclusive.

Each server is a CORBA-based search engine that sits on top of a 
collection of text documents and fields free-text queries pertaining 
to their content. The server publishes its retrieval interface via
IDL and the files with its interoperable object references (IOR's).
Each such server is called an information source.

A Diret client is embedded in a simple text processor. The client 
collects background samples of what the user is typing, transforms 
the samples into queries, sends the queries to the appropriate
information sources, and stores the received retrievals locally.
The user can inspect the retrievals at his or her convenience. Thus, 
the retrieval of pertinent information occurs as a by-product of 
routine activities. The current domain of Diret is mutual fund 
investment.

To enable the Diret client to communicate with the appropriate 
resources, the user goes through the resource subscription when
the system is made operational for the first time. During the
resource subscription, the user specifies which information sources
the user wants the client to be in touch with.

III) Making Diret1.0 Operational
--------------------------------

The file diret.lisp contains the global parameters used by the system. 
To make the system operational, *diret-dir* must be set to a string 
specifying, in the windows style, the directory where all of the system's 
source and data are residing. This is done by modifying the following 
line in diret.lisp:

(defparameter *diret-dir* "d:\\programming\\lisp\\dmapvs2\\").

After this line is modified, diret.lisp should be recompiled.

To run the demo of the system requires four Lisp images. Three of those 
images run Diret servers. The fourth image runs a Diret client.

II.a) Starting First Diret Server
---------------------------------

To start the first Diret server, start the Allegro CL 5.0 Enterprise
Edition (lisp only) image. In the image, cd to the directory specified 
in *diret-dir*. Once in that directory, the following interaction 
should take place in the Listener:

USER(3): (require :orblink)
; Fast loading C:\Program Files\acl50\code\ORBLINK.fasl
;   Loading C:\Program Files\acl50\code\orblink-configure.cl
T
USER(4): :ld diret-server
; Fast loading d:\programming\lisp\dmapvs2\diret-server.fasl
;   Fast loading d:\programming\lisp\dmapvs2\diret.fasl
;   Fast loading d:\programming\lisp\dmapvs2\dmapvs-loader.fasl
;     Fast loading d:\programming\lisp\dmapvs2\utils.fasl
;     Fast loading d:\programming\lisp\dmapvs2\deftable.fasl
;     Fast loading d:\programming\lisp\dmapvs2\frame-manager.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmap.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmaprr.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmapvs.fasl
USER(5): (setup-server :stocks)
Wrote ior to file: stocks-dir-inx.ior
Wrote ior to file: stocks-dir-ret.ior
#<DIRET-SERVER @ #x20564992>

This Diret server fields queries about mutual funds that invest in 
common stocks. To make sure that the server is properly running in 
the Lisp image, evaluate (diret-server-active-p), which should return 
true. You can also inspect the files "stocks-dir-inx.ior" and 
"stocks-dir-ret.ior." These are the two ior files which each diret 
client needs to have access to in order to communicate with the 
server.

II.b) Starting the Second Diret Server
--------------------------------------

To start the second Diret server, start another ACL5.0 Enterprise 
Edition (lisp only) image and follow the the following interactions 
with the Listener:

USER(1): (require :orblink)
; Fast loading C:\Program Files\acl50\code\ORBLINK.fasl
;   Loading C:\Program Files\acl50\code\orblink-configure.cl
T
;;; CD to the directory specified in *diret-dir*.
USER(2): :cd d:/programming/lisp/dmapvs2/
d:\programming\lisp\dmapvs2\

USER(3): :ld diret-server
; Fast loading d:\programming\lisp\dmapvs2\diret-server.fasl
;   Fast loading d:\programming\lisp\dmapvs2\diret.fasl
;   Fast loading d:\programming\lisp\dmapvs2\dmapvs-loader.fasl
;     Fast loading d:\programming\lisp\dmapvs2\utils.fasl
;     Fast loading d:\programming\lisp\dmapvs2\deftable.fasl
;     Fast loading d:\programming\lisp\dmapvs2\frame-manager.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmap.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmaprr.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmapvs.fasl

USER(4): (setup-server :bonds)
Wrote ior to file: bonds-dir-inx.ior
Wrote ior to file: bonds-dir-ret.ior
#<DIRET-SERVER @ #x205809d2>

USER(5): (diret-server-active-p)
T

II.c) Starting the Third Diret Server
-------------------------------------

After the second Diret server is running, start the third Lisp 
image and follow these interactions in the Listener:

USER(1): :cd d:/programming/lisp/dmapvs2/
d:\programming\lisp\dmapvs2\

USER(2): :ld diret-server
; Fast loading d:\programming\lisp\dmapvs2\diret-server.fasl
;   Fast loading d:\programming\lisp\dmapvs2\diret.fasl
;   Fast loading d:\programming\lisp\dmapvs2\dmapvs-loader.fasl
;     Fast loading d:\programming\lisp\dmapvs2\utils.fasl
;     Fast loading d:\programming\lisp\dmapvs2\deftable.fasl
;     Fast loading d:\programming\lisp\dmapvs2\frame-manager.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmap.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmaprr.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmapvs.fasl
;   Fast loading C:\Program Files\acl50\code\ORBLINK.fasl
;     Loading C:\Program Files\acl50\code\orblink-configure.cl
USER(3): (setup-server :cash)
Wrote ior to file: cash-dir-inx.ior
Wrote ior to file: cash-dir-ret.ior
#<DIRET-SERVER @ #x205a1b62>
USER(4): (diret-server-active-p)
T

II.d) Starting the Diret Client
-------------------------------

To start the Diret Client, start the ACL5.0 Enterprise Edition 
(with IDE). Have the following interactions in the Debug Window:

> :cd d:/programming/lisp/dmapvs2/
d:\programming\lisp\dmapvs2\

> :ld diret-client
; Fast loading d:\programming\lisp\dmapvs2\diret-client.fasl
;   Fast loading d:\programming\lisp\dmapvs2\diret.fasl
;   Fast loading d:\programming\lisp\dmapvs2\dmapvs-loader.fasl
;     Fast loading d:\programming\lisp\dmapvs2\utils.fasl
;     Fast loading d:\programming\lisp\dmapvs2\deftable.fasl
;     Fast loading d:\programming\lisp\dmapvs2\frame-manager.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmap.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmaprr.fasl
;     Fast loading d:\programming\lisp\dmapvs2\dmapvs.fasl
;   Fast loading C:\Program Files\acl50\code\ORBLINK.fasl
;     Loading C:\Program Files\acl50\code\orblink-configure.cl
;   Fast loading d:\programming\lisp\dmapvs2\diret-gui-loader.fasl
;     Fast loading d:\programming\lisp\dmapvs2\diret-control-panel.fasl
;     Fast loading d:\programming\lisp\dmapvs2\diret-control-panel-functions.fasl
;     Fast loading d:\programming\lisp\dmapvs2\diret-subscription.fasl
;     Fast loading d:\programming\lisp\dmapvs2\diret-subscription-functions.fasl
;     Fast loading d:\programming\lisp\dmapvs2\diret-retrieval-dialog.fasl
;     Fast loading d:\programming\lisp\dmapvs2\diret-retrieval-functions.fasl

> (diret)
nil

You should see the Diret1.0 Control panel come up. Since there are no 
subscriptions currently available to the client, click on subscriptions,
to subscribe to the three information sources. Subscribe to all three
information sources: stock mutual funds, bond mutual funds, and cash 
mutual funds. Subscription to the source may take a few seconds, since 
the client must contact the appropriate server and receive all it needs 
to know about it. Wait for the message "Subscription has been accepted" 
in the lower text window.

After all three sources are subscribed to, delete the subscription window.
Take a look at the buttons in the Diret control panel. The editor button
brings out a simple text window, where the user types documents. Do not
type anything there yet. The retrievals button brings out a window with
displaying current retrievals. Since nothing has been retrieved yet, no 
retrievals are displayed. 

The DocProcessor button is the one needed for the demo, click on it. You will 
see the three sample documents, sample-document1.txt, sample-document2.txt, 
and sample-document3.txt. The demo is as follows. The chosen document is being 
displayed in the Editor as if typed by the user. Every so often the system 
asks you if you want to inspect the retrievals obtained so far. This simulates
what the user is expected to do, as he works on the document: write some text, 
go inspect the relevant documents found by the client running in the background.

To make sure that the client is, indeed, running in the background, you can 
take a look at the value of sys:*all-processes*. One of the processes is
#<multiprocessing:process Diret1.0 Query Engine(2dd) Sleeping @ #x20bc02ea>.
The query engine is sampling the user's text from the editor's window and 
turns them into queries sent to information sources.

Choose sample-document1.txt and watch the text of that document being displayed 
in the editor window. When the system asks you for retrievals, answer yes or no. 
If you answer yes, the retrievals dialog is brought up with the relevant documents 
found so far displayed in it. When you click on Describe, a brief description of 
that document appears in the lower window. When you click on Examine, the full
text of the document is displayed in a separate window. 

Important: Neither the description nor the text of the document are stored 
locally. The client request them from the appropriate information source and, 
after their reception, unmarshalls and displays them locally.


III) Contact Information
-------------------------

Send questions, comments, and bugs to vkulyukin@cs.depaul.edu.






