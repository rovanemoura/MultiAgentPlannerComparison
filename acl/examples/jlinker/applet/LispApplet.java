
// $Id: LispApplet.java,v 5.0 2004/01/14 18:31:35 layer Exp $


import com.franz.jlinker.JavaLinkDist;
import com.franz.jlinker.TranStruct;
import com.franz.jlinker.LispConnector;

public class LispApplet extends java.applet.Applet {

  // A simple applet that forwards its main methods to Lisp
  // init() make a connection, then forwards to lisp
  // start()  stop()  destroy() fwd to Lisp

  // The following static values in LispConnector can be changed to
  //  suit a particular installation.  The default calues below
  //  expect Lisp to advertise at localhost:4321,
  //  Java will try to connect every second for 5 minutes.
  // LispConnector.lispAdvertises = true;
  // LispConnector.advertInFile   = false;
  // LispConnector.lispFile  = "";
  // LispConnector.lispHost  = "";
  // LispConnector.lispPort  = 4321;
  // LispConnector.pollInterval = 1000;
  // LispConnector.pollCount    = 300;
  // LispConnector.javaTimeout    = -1;
  // LispConnector.javaFile = "";
  // LispConnector.javaHost = "";
  // LispConnector.javaPort = 0;
  // LispConnector.debug = true;


  static Object lock = new Object();
  static boolean connected = false;
    
  // keep track of the number of instances created 
  static int cons = 0;

  // keep track of the number of calls to init
  int call = 0;

  String thisName = "";

  public LispApplet() {
    synchronized (lock) { cons++; }
    thisName = getClass().getName() + cons + ".";
  }
  

  void showThisStatus(String s){
    if ( LispConnector.debug ) 
      {
	showStatus( thisName + call + ": " + s);
	System.out.println( thisName + call + ": " + s );
      }
  }

  void callLisp (String lispFunc) {
    if ( connected && JavaLinkDist.query(false) ) 
      {
	showThisStatus( "Calling Lisp function " + lispFunc );

	// We make a one-way call to Lisp to keep the interface open
	//  for a call from Lisp and a possible callback to Lisp.
	JavaLinkDist.invokeInLisp(-1, JavaLinkDist.newDistOb(lispFunc),
				  new TranStruct[]
				  { JavaLinkDist.newDistOb(this),
				      JavaLinkDist.newDistOb(cons),
				      JavaLinkDist.newDistOb(call)
				      });
      }
    else
      showThisStatus( "Not connected to Lisp when calling " + lispFunc );

  }

  public void init () {

    synchronized (this) { call++; }
    showThisStatus("init() called");

    String[] err = new String[]{"unknown reason"};
    if ( LispConnector.go(true, err) )
      {
	connected = true;
	callLisp("init-applet");
      }
    else
      {
	connected = false;
	showThisStatus("Failed to connect to Lisp: " + err[0]);
      }

  }

  public void destroy () {
    callLisp("destroy-applet");
  }

  public void start () {
    callLisp("start-applet");
  }

  public void stop () {
    callLisp("stop-applet");
  }

}
