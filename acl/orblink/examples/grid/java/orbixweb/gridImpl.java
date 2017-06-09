import org.omg.CORBA.ORB;

public class gridImpl extends example._gridImplBase{
  short numberrows=4;
  short numbercolumns=5;
  String[][] theArray = new String[numberrows][numbercolumns];

public gridImpl(){
 for (int row=0;row<numberrows;++row)
   for (int column=0;column<numbercolumns;++column)
     theArray[row][column]="X";
}

public short height(){
  return numberrows;}

public short width(){
  return numbercolumns;}

public String toString(){
  return "gridImpl of "+numberrows + " rows and "+ numbercolumns+ " columns";
}

public synchronized void set(short row, short column, String value){
  System.out.println("gridImpl: set called with args of: row, column, value: "+ row+" "+column+" "+value);
  theArray[row][column]=value;}

public synchronized String get(short row, short column){
  return theArray[row][column];};
}





