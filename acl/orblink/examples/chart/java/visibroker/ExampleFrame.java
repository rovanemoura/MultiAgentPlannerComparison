import java.awt.*;

public class ExampleFrame extends Frame {

private static int instances = 0;

public ExampleFrame() {
	super();
	addInstance();
}

public ExampleFrame(String s) {
	super(s);
	addInstance();
}

public boolean handleEvent(Event e) {
	if(e.id == Event.WINDOW_DESTROY) {
		hide();
		dispose();
	}

	return super.handleEvent(e);
}

public synchronized void dispose() {
	super.dispose();
	removeInstance();
}

private void addInstance() {
	instances++;
}

private void removeInstance() {
	instances--;

	if(instances == 0)
		System.exit(0);
}

}
