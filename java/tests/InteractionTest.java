package tests;

import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.graph.NetList;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class InteractionTest {

    protected RControl ctrl;
    protected NetList nl;
    protected CreateGraphGUI gui;

    @Before
    public void setUp() {
    	ctrl = RControl.getRControl(true);
    	/*gui = new CreateGraphGUI("graph", new double[] {}, true,  10, true);
    	nl = gui.getGraphView().getNL();*/
    }

    @After
    public void cleanUp() {
    }

    @Test
    public void testReject() {
    	//((MenuBarMGraph)gui.getJMenuBar()).loadGraph("parallelGatekeeping()");
    	
    }
    
  }
	  

