import java.awt.Event;
import jclass.chart.EventTrigger;
import jclass.chart.Chartable;
import jclass.chart.JCChart;
import java.util.Vector;
import java.awt.GridLayout;
import jclass.chart.EditableChartable;
import jclass.chart.ChartDataModel;
import jclass.chart.ChartDataModelUpdate;

public class mydatasource extends ChartDataModel{
  double[] values;
  chart.dataset dataset;
  int numbervalues;

public void dataset_was_adjusted(int entry, double value){
  System.out.println("mydatasource: Dataset was adjusted on entry: "
		     +entry
		     +" and new value: "
		     +value);
  double delta=values[entry]-value;
  if (delta < 0) delta= -delta;
  if (delta < 0.001) {System.out.println("Ignoring change");return;}
  values[entry]=value;
  setChanged();
  notifyObservers(new ChartDataModelUpdate(ChartDataModelUpdate.CHANGE_VALUE, 1,entry));
}


public mydatasource(chart.dataset originaldataset){
  dataset=originaldataset;
  System.out.println("ChartDrawer: Starting with dataset: " + dataset);
  numbervalues = dataset.number_entries();
  System.out.println("Got number values of: " + numbervalues);
  values = new double[numbervalues];
  for (int i = 0 ; i< numbervalues; ++i)
    {
      values[i]=dataset.get(i);
    }
}

public String toString(){
  String s = "mydataset: ";
  for (int i = 0; i<numbervalues; i++)
    s = s+
      "[" +
      i+
      " . "
      + values[i]
      + "]";
  return s;
}
public boolean setDataItem (int row, int column, Object o){
  System.out.println
    ("mydatasource: setDataItem called on row of: " 
     + row
     + " and column of: "
     + column
     + " on object: "
     +o);
  dataset.adjust_requested(column,((Double)o).doubleValue());
  return true;
}

public int getDataInterpretation(){
  return Chartable.GENERAL;}

public Object getDataItem (int row, int column){
  if (row==0)
    return new Double((double)column);
  else if (row==1)
    return new Double(values[column]);
  else
    {
      System.out.println("Bad arg to getDataItem");
      System.exit(1);
    }
  return null;
}

public Vector getRow(int row){
  Vector v = new Vector();
  for (int i=0;i<numbervalues;++i){
    Double d=null;
    if(row==0) d = new Double((double)i);
    else if (row==1) d = new Double (values[i]);
    v.addElement(d);
  }
  return v;
}
public int getNumRows(){
  return 2;}

public String[] getPointLabels(){return null;}

public String getSeriesName(int row){return null;}

public String getSeriesLabel(int row){return null;}

public String getName() {return null;}
}

