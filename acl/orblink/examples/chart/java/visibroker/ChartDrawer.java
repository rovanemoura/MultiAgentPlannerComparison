import java.awt.Event;
import jclass.chart.EventTrigger;
import jclass.chart.Chartable;
import jclass.chart.JCChart;
import java.util.Vector;
import java.awt.GridLayout;
import jclass.chart.EditableChartable;

public class ChartDrawer extends chart._datasetlistenerImplBase{
  mydatasource mydata;

public ChartDrawer(chart.dataset originaldataset){
  mydata = new mydatasource(originaldataset);}

public void dataset_was_adjusted(int entry, double value){
  System.out.println("ChartDrawer: dataset_was_adjusted called on entry: "
		     +entry
		     +" and value: "
		     +value);
  mydata.dataset_was_adjusted(entry, value);
}

public mydatasource get_datasource(){
  return mydata;}

public String toString(){
  return "ChartDrawer with mydatasource of: "+mydata;}

public static void main(String args[]){
  System.out.println("ChartDrawer: starting");
    if (args.length != 1) {
      System.out.println("Usage: ChartDrawer <filename> [where filename is the path to the file in which the IOR of the server grid object is stored]");
      System.exit(1);
    };
    String filename = args[0];

  org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init();
  org.omg.CORBA.BOA boa = orb.BOA_init();

  System.out.println("ChartDrawer: main: getting dataset");
  org.omg.CORBA.Object obj = IorIo.resolve(orb,filename);
  System.out.println("got object of: "+obj);
  chart.dataset d = chart.datasetHelper.narrow(obj);
  System.out.println("main: creating ChartDrawer");
  ChartDrawer chartdrawer = new ChartDrawer(d);
  orb.connect(chartdrawer);
  System.out.println("main: registering chartdrawer: "+chartdrawer);
  d.register_listener(chartdrawer);
  mydatasource mydatasource = chartdrawer.get_datasource();
  System.out.println("main: got chartdrawer of: " + chartdrawer);
  JCChart jc = new JCChart();
  jc.getDataView(0).setDataSource(mydatasource);
  jc.setAllowUserChanges(true);
  jc.setTrigger(0,new EventTrigger(Event.META_MASK, EventTrigger.CUSTOMIZE));
  jc.setTrigger(0,new EventTrigger(0,EventTrigger.EDIT));
  ExampleFrame f = new ExampleFrame("plot1");
  f.setLayout(new GridLayout(1,1));
  f.add(jc);
  f.pack();
  f.resize(400, 300);
  f.show();
  try {Thread.currentThread().join();}
  catch (InterruptedException e){
    System.out.println(e);
  }
}
}


  
