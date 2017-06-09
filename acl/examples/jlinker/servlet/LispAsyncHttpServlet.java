
// $Id: LispAsyncHttpServlet.java,v 5.0 2004/01/14 18:31:35 layer Exp $

import com.franz.jlinker.JavaLinkDist;
import com.franz.jlinker.JavaLinkCommon;
import com.franz.jlinker.TranStruct;
import javax.servlet.http.*;

public class LispAsyncHttpServlet extends LispHttpServlet {

  // This class is a prototype class to be sub-classed or
  //  modified for a specific application.  It is supplied in
  //  source form to allow compilation with the appropriate
  //  Servlet toolkit and library.

  // The Java subclass constructor should call the default 
  //  constructor super() and then sets the config variables.

  // The lisp class name is required and must be a user-defined
  //  Lisp class that provides the required method implementations.
  //  The built-in class methods in "async-http-servlet" and
  //  "multi-async-http-servlet" simply print a 
  //  status message and return to Java.
  //  
  // This class provides methods that propagate requests to Lisp
  //  asynchronously:
  //      - request arrives in Java
  //      - Java method calls Lisp
  //      - Lisp shedules the work and returns to Java
  //      - Java method waits for work to be completed
  //         and then returns to the caller.
  //
  // Lisp code may call Java, and Java may call back to Lisp because
  //  the channel from Java to Lisp is free.
  // In Lisp, two implementation classes are available:
  //    async-http-servlet queues all work into a single Lisp thread
  //    multi-async-http-servlet starts a fresh Lisp thread for each
  //       request.


  public LispAsyncHttpServlet() { 
    super();
    lispClassName = "async-http-servlet"; 
  }

  // Inherited:
  //
  // TranStruct lispServlet;
  //
  // int lispValues( TranStruct[] res, String called, int min, int max, 
  //		  boolean firstRefP )
  //     throws javax.servlet.ServletException 
  //
  // public void init()  throws javax.servlet.ServletException 


  public void startRequest( HttpServletRequest request,
			    HttpServletResponse response,
			    String lispFunc ) 
       throws javax.servlet.ServletException 
  {
    TranStruct[] args, res; int vct;
    Object gate = JavaLinkCommon.newGate();
    if ( !JavaLinkDist.query(true) )
      throw( new javax.servlet.ServletException("Lost connection to Lisp") );
    args = new TranStruct[5];
    args[0] = lispServlet;
    args[1] = JavaLinkDist.newDistOb(lispFunc);
    args[2] = JavaLinkDist.newDistOb(request);
    args[3] = JavaLinkDist.newDistOb(response);
    args[4] = JavaLinkDist.newDistOb(gate);
    res = JavaLinkDist.invokeInLisp
             ( 2, JavaLinkDist.newDistOb("net.jlinker:start-work"), 
	       args );
    vct=lispValues(res, lispFunc, 0, 1, false);
    if ( vct==1 ) JavaLinkDist.discardInLisp(res[1]);

    String err = JavaLinkCommon.testGate(gate);
    if ( 0<err.length() )
      throw( new javax.servlet.ServletException
	     ( "LispServlet error in " + lispFunc + " = " + err ) );
  }
  

  public void doDelete(HttpServletRequest request,
		       HttpServletResponse response) 
       throws javax.servlet.ServletException 
  {
    startRequest(request, response, "net.jlinker:do-delete");
  }

  public void doGet(HttpServletRequest request,
		    HttpServletResponse response) 
       throws javax.servlet.ServletException 
  {
    startRequest(request, response, "net.jlinker:do-get");
  }

  public void doHead(HttpServletRequest request,
		     HttpServletResponse response)
       throws javax.servlet.ServletException
  {
    startRequest(request, response, "net.jlinker:do-head");
  }

  public void doOptions(HttpServletRequest request,
			HttpServletResponse response)
       throws javax.servlet.ServletException
       //                Allow: GET,HEAD,TRACE,OPTIONS
  {
    startRequest(request, response, "net.jlinker:do-options");
  }

  public void doPost(HttpServletRequest request,
		     HttpServletResponse response)
       throws javax.servlet.ServletException
  {
    startRequest(request, response, "net.jlinker:do-post");
  }

  public void doPut(HttpServletRequest request,
		    HttpServletResponse response)
       throws javax.servlet.ServletException
  {
    startRequest(request, response, "net.jlinker:do-put");
  }

  public void doTrace(HttpServletRequest request,
		      HttpServletResponse response)
       throws javax.servlet.ServletException
  {
    startRequest(request, response, "net.jlinker:do-trace");
  }

  // Inherited:
  // public void destroy() 
  

}


