
// ;; $Id: TryJavaToLisp.java,v 5.0 2004/01/14 18:31:35 layer Exp $

import com.franz.jlinker.JavaLinkDist;
import com.franz.jlinker.LispConnector;
import com.franz.jlinker.TranStruct;
import com.franz.jlinker.LispCall;

public class TryJavaToLisp {

  public static void main ( String[] argv )
       throws JavaLinkDist.JLinkerException
  {

    //
    // A simple Java main program to call Lisp and print the answer
    //

    String[] cerr = new String[1]; 
    LispConnector.lispPort = 4326;
    LispConnector.debug = false;

    if ( LispConnector.go(false, cerr) )
      {

	LispCall x = new LispCall("test-work");
	x.addArg(100);
	if ( 0<x.call() )
	  {
	    System.out.println( "Value came back = " + x.intValue() );
	  }
	else
	  System.out.println( "Did not get expected result from Lisp." );



	System.out.println( "Try a call the old way too." );

	TranStruct opToLisp;
	TranStruct[] argsToLisp = new TranStruct[1];
	
	// Setup the arguments in their transfer wrappers
	opToLisp = JavaLinkDist.newDistOb("test-work");
	argsToLisp[0] = JavaLinkDist.newDistOb(100);

	TranStruct[] res = JavaLinkDist.invokeInLisp(2, opToLisp, argsToLisp);

	if ( 1<res.length                           // returned at least one value
	     && JavaLinkDist.integerP( res[0] )     // number of values is specified
	     && 1==JavaLinkDist.intValue( res[0] )  // number of values is 1
	     && JavaLinkDist.integerP( res[1] )     // the one value is an integer
	     )
	  {
	    System.out.println( "Value came back = " +

				// fetch the actual value returned from Lisp
				JavaLinkDist.intValue( res[1] ) 

				);

	  }
	  
	else
	  System.out.println( "Did not get expected result from Lisp." );

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
