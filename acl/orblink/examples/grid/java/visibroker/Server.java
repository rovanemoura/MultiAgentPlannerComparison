// Sample server for the grid example, visibroker

public class Server {

public static void main (String[] args) {

    if (args.length != 1) {
      System.out.println("Usage: Client <filename> [where filename is the path to the file in which the IOR of the server grid object is stored]");
      System.exit(1);
    };
    String filename = args[0];

  //Initialize the ORB
  org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init(args, null);
  
  //Instantiate a grid object
  example.grid grid = new gridImpl();


  // Connect the grid object to the ORB
  orb.connect(grid);


  //Bind the IOR of the grid object to the name "grid"
  IorIo.publish(orb,grid,filename);

  System.out.println(grid + " is ready.");

  //This just blocks and waits for invocations on the grid object
  try {
    Thread.currentThread().join();
  }
  catch (InterruptedException e){
    System.out.println(e);
  }
}
}


