
// ;; $Id: ConnectToLisp.java,v 5.0 2004/01/14 18:31:35 layer Exp $

import com.franz.jlinker.JavaLinkDist;
import com.franz.jlinker.LispConnector;

public class ConnectToLisp {

  public static void main ( String[] argv ) {

    //
    // A simple Java main program 
    //

    String[] cerr = new String[1]; 
    LispConnector.lispPort = 7171;
    LispConnector.debug = false;

    if ( LispConnector.go(false, cerr) )
      {
	try { Thread.sleep(300000); }
	catch (InterruptedException e) {}

	System.out.println( "Disconnecting..." );
	JavaLinkDist.disconnect();

      }
    else
      {
	// connection failed, report in cerr
	System.out.println( "Connect failed: " + cerr[0] );
      }


  }

}
