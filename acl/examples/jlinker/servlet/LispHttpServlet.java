
// $Id: LispHttpServlet.java,v 5.0 2004/01/14 18:31:35 layer Exp $


import com.franz.jlinker.JavaLinkDist;
import com.franz.jlinker.JavaLinkCommon;
import com.franz.jlinker.TranStruct;
import com.franz.jlinker.LispConnector;
import javax.servlet.http.*;
import javax.servlet.ServletException;

public class LispHttpServlet extends HttpServlet {

  // This class is a prototype class to be sub-classed or
  //  modified for a specific application.  It is supplied in
  //  source form to allow compilation with the appropriate
  //  Servlet toolkit and library.

  // The Java subclass constructor should call the default 
  //  constructor super() and then sets the config variables.

  // The lisp class name is required and must be a user-defined
  //  Lisp class that provides the required method implementations.
  //  The built-in class methods in "http-servlet" simply print a 
  //  status message and return to Java.
  //  
  // This class provides methods that propagate requests to Lisp
  //  synchronously:
  //      - request arrives in Java
  //      - Java method calls Lisp
  //      - Lisp works and returns to Java
  //      - Java method returns
  //
  // Because of the jLinker re-entrancy restrictions, the Lisp code may 
  //  call Java, but the called Java methods may not call back to Lisp
  //  because the channel from Java to Lisp is busy.

  public String lispClassName = "net.jlinker:http-servlet";


  // The following static values in LispConnector can be changed to
  //  suit a particular installation.  The default values below
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

  public LispHttpServlet() { 
  }

  TranStruct lispServlet;

  int lispValues( TranStruct[] res, String called, int min, int max, 
		  boolean firstRefP )
       throws ServletException 
  {
    Object[] r = JavaLinkCommon.lispValues(res, called, min, max, firstRefP);
    String err = (String)(r[1]);
    if ( err.length()>0 ) throw( new ServletException(err) );
    else return ( (Integer)(r[0]) ).intValue();
  }

  public void init()  throws ServletException 
  {
    String[] err = new String[]{"unknown error"};
    if ( !LispConnector.go(true, err) )
	 throw( new ServletException( err[0] ) );

    TranStruct[] args, res; int vct;
    args = new TranStruct[3];
    args[0] = JavaLinkDist.newDistOb(lispClassName);
    args[1] = JavaLinkDist.newDistOb(this);
    args[2] = JavaLinkDist.newDistOb( getServletConfig() );
    res = JavaLinkDist.invokeInLisp
            ( 2, 
	      JavaLinkDist.newDistOb("net.jlinker:new-servlet"), 
	      args );
    vct=lispValues(res, "new-servlet", 1, 1, true);

    lispServlet = res[1];
    
    return;
  }


  public void doRequest ( HttpServletRequest request,
			  HttpServletResponse response,
			  String lispFunc
			  ) 
       throws javax.servlet.ServletException 
  {
    TranStruct[] args, res; int vct;

    if ( !JavaLinkDist.query(true) )
      throw( new javax.servlet.ServletException("Lost connection to Lisp") );
    args = new TranStruct[3];
    args[0] = lispServlet;
    args[1] = JavaLinkDist.newDistOb(request);
    args[2] = JavaLinkDist.newDistOb(response);
    res = JavaLinkDist.invokeInLisp
             ( 2, JavaLinkDist.newDistOb(lispFunc), args );
    vct=lispValues(res, lispFunc, 0, 1, false);
    if ( vct==1 ) JavaLinkDist.discardInLisp(res[1]);

  }

  public void doDelete(HttpServletRequest request,
		       HttpServletResponse response) 
       throws javax.servlet.ServletException 
  {
    doRequest(request, response, "net.jlinker:do-delete");
  }

  public void doGet(HttpServletRequest request,
		    HttpServletResponse response) 
       throws javax.servlet.ServletException 
  {
    doRequest(request, response, "net.jlinker:do-get");
  }

  public void doHead(HttpServletRequest request,
		     HttpServletResponse response)
       throws javax.servlet.ServletException
  {
    doRequest(request, response, "net.jlinker:do-head");
  }

  public void doOptions(HttpServletRequest request,
			HttpServletResponse response)
       throws javax.servlet.ServletException
       //                Allow: GET,HEAD,TRACE,OPTIONS
  {
    doRequest(request, response, "net.jlinker:do-options");
  }

  public void doPost(HttpServletRequest request,
		     HttpServletResponse response)
       throws javax.servlet.ServletException
  {
    doRequest(request, response, "net.jlinker:do-post");
  }

  public void doPut(HttpServletRequest request,
		    HttpServletResponse response)
       throws javax.servlet.ServletException
  {
    doRequest(request, response, "net.jlinker:do-put");
  }

  public void doTrace(HttpServletRequest request,
		      HttpServletResponse response)
       throws javax.servlet.ServletException
  {
    doRequest(request, response, "net.jlinker:do-trace");
  }

  // protected long getLastModified(HttpServletRequest request);
  //   This one is not propagated to Lisp, but called _from_ Lisp.


  // protected void service(...)
  //   Let the Java superclass handle this one.

  public void destroy() 
  {
    TranStruct[] args, res; int vct;

    if ( JavaLinkDist.query() )
      {
	args = new TranStruct[1];
	args[0] = JavaLinkDist.lastUse(lispServlet);
	res = JavaLinkDist.invokeInLisp
	         ( 2, JavaLinkDist.newDistOb("net.jlinker:destroy-servlet"), 
		   args );
	lispServlet = null;
      }
  }

}


