//Sample client for visibroker java
// Retrieves value of object named "grid" and invokes an operation on it.
// This client retrieves grid and grid 2 and invokes operations on both of them
// It is used to test clien socket protocol procedure.

public class Client2 {

  public static void main(String[] args) {
    // Initialize the ORB.
    org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init(args,null);
    // Get the object bound to "grid"
    org.omg.CORBA.Object obj = com.franz.orblink.SimpleNameServer.resolve(orb,"grid");
    org.omg.CORBA.Object obj2 = com.franz.orblink.SimpleNameServer.resolve(orb,"grid2");

    //Narrow grid to the given value
    example.grid grid = 
      example.gridHelper.narrow(obj);
    example.grid grid2 = 
      example.gridHelper.narrow(obj2);

    System.out.println ("Setting the value of cell 2,3");

    // Invoke some operations
    grid.set((short)2, (short)3,"test");
    System.out.println("Got value of: " + grid.get((short)2, (short)3));

    System.out.println ("Setting the value of cell 2,3 in grid2");

    // Invoke some operations
    grid2.set((short)2, (short)3,"test2");
    System.out.println("Got value of: " + grid2.get((short)2, (short)3));
    
    // Wait for input
    try{
      Thread.currentThread().sleep(1000000);}
    catch (Exception e){;};
  }
}

