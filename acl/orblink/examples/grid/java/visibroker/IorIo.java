import java.io.RandomAccessFile;
import java.io.FileWriter;

public class IorIo{
  static String NamingDirectory = "";
  
public static void publish (org.omg.CORBA.ORB orb, org.omg.CORBA.Object object, String name)
    {
      String ior = orb.object_to_string (object);
      String filename = NamingDirectory + name;
      try{
	FileWriter output= new FileWriter (filename);
	output.write(ior);
	output.close();
	System.out.println ("SimpleNameServer.publish: Wrote IOR of " + ior + " to file " + filename );
      }
      catch (Exception e)
	{System.out.println("Write failed on exception: "+e);}
    }

public static String getLine(String name){
  String filename=NamingDirectory + name;
  System.out.println("opening file: "+filename);
  String line=null;
  RandomAccessFile reader=null;
  try{
     reader=new RandomAccessFile(filename,"r");
  }
  catch (Exception e)
    {System.out.println ("Could not open file: "+ filename +" on exception: "+e);
    return null;}
  try{
    line=reader.readLine();
  }
  catch (Exception e)
    {System.out.println("Read failed on exception: "+e);
    return null;}
  return line;
}

public static org.omg.CORBA.Object resolve (org.omg.CORBA.ORB orb, String name)
    {
      String line=getLine(name);
      if (line==null){
	System.out.println("resolve: Could not read name: "+line);
	return null;}
      org.omg.CORBA.Object obj = orb.string_to_object (line);
      return obj;
}  
}

  
