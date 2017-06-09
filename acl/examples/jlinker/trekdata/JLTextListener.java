
// copyright (c) 2000 Franz Inc, Berkeley, CA
// copyright (c) 2000-2012 Franz Inc, Oakland, CA - All rights reserved.
//
// The software, data and information contained herein are proprietary
// to, and comprise valuable trade secrets of, Franz, Inc.  They are
// given in confidence by Franz, Inc. pursuant to a written license
// agreement, and may be stored and used only in accordance with the terms
// of such license.
//
// Restricted Rights Legend
// ------------------------
// Use, duplication, and disclosure of the software, data and information
// contained herein by any agency, department or entity of the U.S.
// Government are subject to restrictions of Restricted Rights for
// Commercial Software developed at private expense as specified in
// DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.

// $Id: JLTextListener.java,v 5.0 2004/01/14 18:31:35 layer Exp $

// A TextListener that forwards TextEvent events to the
//   jLinker notify-lisp dispatcher.

import java.awt.*;
import java.awt.event.*;


public class JLTextListener implements TextListener {

  private Object handle;

  public static synchronized void addTo( TextComponent comp ) {
    JLTextListener l = new JLTextListener();
    l.handle = (Object)comp;
    comp.addTextListener( (TextListener)l );
  }


  public void textValueChanged(TextEvent e) {

    String[] s = new String[0];
    int[]    l = new int[0];
    
    com.franz.jlinker.JavaLinkCommon.ltoj_anchor.callLisp
      ("textValueChanged", handle, s, l);
  }

}

