
// $Id: TrekAppletLocal.java,v 5.0 2004/01/14 18:31:35 layer Exp $

// This code is derived from LispApplet.java

import com.franz.jlinker.JavaLinkDist;
import com.franz.jlinker.TranStruct;
import com.franz.jlinker.LispConnector;

public class TrekAppletLocal extends java.applet.Applet {

  static Object lock = new Object();
  String user = "";
  boolean connected = false;
  
  // keep track of the number of instances created 
  static int cons = 0;

  // keep track of the number of calls to init
  int call = 0;

  String thisName = "";

  public TrekAppletLocal() {
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

  public void init () {

    String host=getParameter("lisp_host");
    String s_port;
    user=getParameter("user_name");
    int lport = 0;
    int jport = 0;

    try { 
      s_port=getParameter("lisp_port");
      lport=Integer.parseInt(s_port);
      s_port=getParameter("java_port");
      jport=Integer.parseInt(s_port);
    } catch (Exception e) { }

    synchronized (this) { call++; }
    showThisStatus("Starting");
    // System.out.println(thisName + "Starting");

    LispConnector.lispHost = host;
    LispConnector.lispPort = lport;
    LispConnector.javaPort = jport;
    LispConnector.pollCount = 5;
    int aport = jport;
    if ( jport<0 ) 
      {
	LispConnector.javaHost = host;
	aport = -jport;
      }
    String[] err = new String[]{""};

    if ( LispConnector.go(true, err) )
      {
	showThisStatus("Connected to Lisp");
	// System.out.println(thisName + "Connected to Lisp");
	connected = true;
      }
    else 
      {
	showThisStatus("Failed to connect to Lisp");
	// System.out.println(thisName + "Failed to connect to Lisp");
	showThisStatus(err[0]);
	// System.out.println(thisName + err[0]);
	connected = false;
      }

  }

  public void destroy () {
    showThisStatus("Destroy called");

    if (connected)
      JavaLinkDist.invokeInLisp(-1, JavaLinkDist.newDistOb("destroy-applet"),
				new TranStruct[]
				{ JavaLinkDist.newDistOb(this),
				    JavaLinkDist.newDistOb( thisName+call ),
				    JavaLinkDist.newDistOb( user )
				    }
				);
    else showThisStatus("Destroy called - not connected");
  }

  public void start () {
    showThisStatus("Start called");
    if (connected)
      {
	JavaLinkDist.invokeInLisp(-1, JavaLinkDist.newDistOb("start-applet"),
				  new TranStruct[]
				  { JavaLinkDist.newDistOb(this),
				      JavaLinkDist.newDistOb( thisName+call ),
				      JavaLinkDist.newDistOb( user )
				      });
      }
    else showThisStatus("Start called - not connected");
  }

  public void stop () {
    showThisStatus("Stop called");
    if (connected)
      JavaLinkDist.invokeInLisp(-1, JavaLinkDist.newDistOb("stop-applet"),
				new TranStruct[]
				{ JavaLinkDist.newDistOb(this),
				      JavaLinkDist.newDistOb( thisName+call ),
				      JavaLinkDist.newDistOb( user )
				    }
				);
    else showThisStatus("Stop called - not connected");
  }

}
