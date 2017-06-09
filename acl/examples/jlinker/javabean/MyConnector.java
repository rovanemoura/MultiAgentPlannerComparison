
// $Id: MyConnector.java,v 5.0 2004/01/14 18:31:35 layer Exp $

import com.franz.jlinker.LispConnector;

public class MyConnector extends LispConnector {

  // This class illustrates how a custom connection sub-class
  // may be created.

  public static boolean go(boolean q, String[] t) {

    LispConnector.lispAdvertises = true;
    LispConnector.advertInFile   = false;
    LispConnector.lispHost  = "";
    LispConnector.lispPort  = 4323;

    return LispConnector.go(q, t);
  }

}
