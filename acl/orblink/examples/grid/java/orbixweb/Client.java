//Sample client for visibroker java
// Retrieves value of object named "grid" and invokes an operation on it.

public class Client {

  public static void main(String[] args) {
    // Initialize the ORB.
    if (args.length != 1) {
      System.out.println("Usage: Client <filename> [where filename is the path to the file in which the IOR of the server grid object is stored]");
      System.exit(1);
    };
    String filename = args[0];
			    
    org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init(args,null);
    // Get the object bound to "grid"
    org.omg.CORBA.Object obj = IorIo.resolve(orb,filename);

    //Narrow grid to the given value
    example.grid grid = 
      example.gridHelper.narrow(obj);

    System.out.println ("Setting the value of cell 2,3");

    // Invoke some operations
    grid.set((short)2, (short)3,"test");
    System.out.println("Got value of: " + grid.get((short)2, (short)3));
  }
}

