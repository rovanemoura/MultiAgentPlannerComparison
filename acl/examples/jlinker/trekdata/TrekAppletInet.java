
// $Id: TrekAppletInet.java,v 5.0 2004/01/14 18:31:35 layer Exp $

// A web-optimzed subclass of TrekAppletLocal.
//   Repetitious tasks are done in utility methods in Java to
//   reduce the number of internet round-trips between Lisp
//   and Java.

import com.franz.jlinker.JavaLinkDist;
import com.franz.jlinker.TranStruct;
import com.franz.jlinker.LispConnector;

public class TrekAppletInet extends TrekAppletLocal {

  public TrekAppletInet() { super(); }

  public void destroy () {
    showThisStatus("Destroy called");
    System.out.println(thisName + "Destroy called");
    if ( connected && JavaLinkDist.query() )
      JavaLinkDist.invokeInLisp(-1, JavaLinkDist.newDistOb("destroy-applet"),
				new TranStruct[]
				{ JavaLinkDist.newDistOb(this),
				    JavaLinkDist.newDistOb( thisName+call ),
				    JavaLinkDist.newDistOb( user )
				    }
				);
    else 
      { 
	connected = false;
	showThisStatus("Destroy called - not connected");
	System.out.println(thisName + "Destroy called - not connected");
      }
  }

  public static TrekAppletInet thisApplet;
  public void start () 
  {
    thisApplet=this;
    showThisStatus("Start called");
    System.out.println(thisName + "Start called");
    if ( connected && JavaLinkDist.query() )
      {
	JavaLinkDist.invokeInLisp(-1, JavaLinkDist.newDistOb("start-applet"),
				  new TranStruct[]
				  { JavaLinkDist.newDistOb(this),
				      JavaLinkDist.newDistOb( thisName+call ),
				      JavaLinkDist.newDistOb( user )
				      });
      }
    else 
      {
	connected = false;
	showThisStatus("Start called - not connected");
	System.out.println(thisName + "Start called - not connected");
      }
  }

  public void stop () {
    showThisStatus("Stop called");
    System.out.println(thisName + "Stop called");
    if ( connected && JavaLinkDist.query() )
      JavaLinkDist.invokeInLisp(-1, JavaLinkDist.newDistOb("stop-applet"),
				new TranStruct[]
				{ JavaLinkDist.newDistOb(this),
				      JavaLinkDist.newDistOb( thisName+call ),
				      JavaLinkDist.newDistOb( user )
				    }
				);
    else 
      {
	connected = false;
	showThisStatus("Stop called - not connected");
	System.out.println(thisName + "Stop called - not connected");
      }
  }


  public static boolean modifyState = false;
  public static boolean busyState = false;
  public static java.awt.TextField statusArea = null;

  public static void cellModified( int state, java.awt.Button ref,
				   java.awt.Button conf ) {
    if (state==0) modifyState = false;
    else modifyState = true;
    String l;
    if (modifyState) l="Reset"; else l="Refresh";
    ref.setLabel(l);
    if (modifyState) l="Confirm"; else l="Refresh";
    conf.setLabel(l);
  }

  public static void setBusy() { 
    if ( statusArea!=null ) statusArea.setText("Working...");
    busyState = true; 
  }
  public static void setReady() {
    if ( statusArea!=null ) statusArea.setText("Ready!");
    busyState = false;
  }

  public static void addStatusLine( java.awt.Component comp,
				    String modtext, String unmod ) {

    class MAplus extends java.awt.event.MouseAdapter {
      String mod, un;
      public MAplus(String modtext, String unmod) {
	super();
	mod=modtext; un=unmod;
      }
      public void mouseEntered(java.awt.event.MouseEvent e)
      {
	if ( statusArea==null ) return;
	if (busyState) statusArea.setText("Working...");
	else if ( 0==un.length() ) statusArea.setText(mod);
	else if (modifyState) statusArea.setText(mod);
	else statusArea.setText(un);
      }
    }

    comp.addMouseListener( new MAplus(modtext, unmod) );
  }

  public static java.awt.Container addCont;
  public static java.awt.GridBagLayout addLay;
  public static java.awt.Label addLabel = null;
  public static int countStep = 0;
  public static int countInterval = 0;
  public static int countIndex = -1;
  public static String countMsg[] = { "Preparing login screen... ",
				      "Preparing main screen... " };
  
  public static void addConstrained ( java.awt.Label lb, int iv,
				      java.awt.Container cont,
				      java.awt.GridBagLayout lay,
				      java.awt.Component item,
				      int gridx, int gridy, int gridw, int gridh,
				      double wtx, double wty,
				      int fill,
				      int ins0 )
  {
    addLabel = lb; countStep = 0; countInterval = iv%100; countIndex = iv/100;
    addConstrained(cont, lay, item, gridx, gridy, gridw, gridh, wtx, wty, fill,
		   ins0);
  }

  public static void addConstrained ( java.awt.Container cont,
				      java.awt.GridBagLayout lay,
				      java.awt.Component item,
				      int gridx, int gridy, int gridw, int gridh,
				      double wtx, double wty,
				      int fill,
				      int ins0 )
  {
    addCont = cont; addLay = lay;
    addConstrained(item, gridx, gridy, gridw, gridh, wtx, wty, fill,
		   ins0);
  }

  public static void addConstrained (   java.awt.Component item,
					int gridx, int gridy, int gridw, int gridh,
					double wtx, double wty,
				        int fill, int inset,
					String s1, String s2 )
  {
    addConstrained(item, gridx, gridy, gridw, gridh, wtx, wty, fill,
		   inset);
    addStatusLine(item, s1, s2);
  }


  public static void addConstrained (   java.awt.Component item,
					int gridx, int gridy, int gridw, int gridh,
					double wtx, double wty,
				        int fill, int inset )
  {
    int ins0, ins1, ins2, ins3;
    ins3 = inset%100;  inset = inset/100;
    ins2 = inset%100;  inset = inset/100;
    ins1 = inset%100;
    ins0 = inset/100;

    java.awt.GridBagConstraints cnstr 
      = new java.awt.GridBagConstraints
      (gridx, gridy, gridw, gridh, wtx, wty,
       java.awt.GridBagConstraints.CENTER,
       fill,
       new java.awt.Insets(ins0, ins1, ins2, ins3), 0, 0);
    addLay.setConstraints(item, cnstr);
    addCont.add(item, cnstr);
    if ( addLabel!=null ) 
      {
	int i = countStep;
	countStep++;
	i = i*countInterval;
	if (99<i) i=99;
	addLabel.setText(countMsg[countIndex]+i+"% done...");
      }
  }

  public static void setCellText( java.awt.Label l1, String t1,
				  java.awt.Label l2, String t2,
				  java.awt.Label l3, String t3,
				  java.awt.TextComponent l4, String t4,
				  java.awt.Label l5, String t5,
				  java.awt.TextComponent l6, String t6
				  )
  {
    l1.setText(t1);
    l2.setText(t2);
    l3.setText(t3);
    l4.setText(t4);
    l5.setText(t5);
    l6.setText(t6);
  }

}
