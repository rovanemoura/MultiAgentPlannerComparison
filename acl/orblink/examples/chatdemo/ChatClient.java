import java.awt.*;
import java.awt.event.*;
import chat.*;
import org.omg.CORBA.*;

public class ChatClient extends Frame{
private TextArea chatData;
private TextField sendData;
private Button connectButton;
private Button sendButton;
static String filename;
  
public ChatClient() {
  setLayout (new BorderLayout());
  add(chatData = new TextArea (10,20), "Center");
  Panel bottom = new Panel();
  bottom.setLayout(new GridLayout (2,2));
  bottom.add (new Label("Data: "));
  bottom.add(sendData = new TextField());
  bottom.add(sendButton = new Button("Send"));
  bottom.add(connectButton =new Button("Connect"));
  add(bottom, "South");
  pack();

  sendButton.setEnabled(false);

  ButtonListener listener = new ButtonListener();
  connectButton.addActionListener(listener);
  sendButton.addActionListener(listener);
}

  class ButtonListener implements ActionListener {
    private ORB orb = null;
    private BOA boa = null;
    private ChatServerI server = null;
    
    public void actionPerformed(ActionEvent ae){
       if (ae.getSource() == connectButton) doConnect();
       else doSend();
    }

    private void doConnect(){
       orb = ORB.init();
       boa = orb.BOA_init();
       //       server = ChatServerIHelper.bind(orb,"JavaPRO"); 
       org.omg.CORBA.Object obj = IorIo.resolve(orb,filename);
       server = ChatServerIHelper.narrow(obj);
       ChatListenerI listener = new Listener();
       boa.obj_is_ready(listener);
       server.addListener(listener);
       sendButton.setEnabled(true);
       connectButton.setEnabled(false);
       System.out.println("connected");
     }

    private void doSend(){
     server.sendMessage(sendData.getText());
     sendData.setText("");
    }
  }
 
  class Listener extends _ChatListenerIImplBase{
    public Listener() {
     super();
      }

    public void messageReceived(String message){
      chatData.append(message + "\n");
     }
  }

  public static final void main (String args[]){
    if (args.length != 1) {
      System.out.println
	("Usage: ChartDrawer <filename> [where filename is the path to the file in which the IOR of the server grid object is stored]");
      System.exit(1);
    };
    filename = args[0];
    
   ChatClient client = new ChatClient();
   client.setVisible(true);
   }
}
