package tests;

import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.graph.GraphMCP;
import org.af.gMCP.gui.graph.NetList;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class LoadSaveTest {

    protected RControl ctrl;
    protected NetList nl;
    protected CreateGraphGUI gui;

    @Before
    public void setUp() {
    	ctrl = RControl.getRControl(true);
    	RControl.getR().evalVoid("library(gMCP)");
    	gui = new CreateGraphGUI("graph", new double[] {}, true,  10, true);
    	nl = gui.getGraphView().getNL();
    	//nl = new NetList(new JLabel(), new GraphView("graph", null));
    }

    @After
    public void cleanUp() {
    }

    @Test
    public void testLoadSave() {
    	String graph = TestSuite.exampleGraphs[1];
        //for (String graph : TestSuite.exampleGraphs)   	
    	{
        	RControl.getR().evalVoid("graph <- "+graph);
        	nl.updateGUI = false;
        	nl.reset();        	
        	nl.graph = new GraphMCP("graph", nl);
        	nl.saveGraph(".testGraph", false, false);
        	nl.refresh();
        	nl.updateGUI = true;
        	//ctrl.getR().eval("gMCP:::equals(graph, .testGraph)");
        }
    	gui.dispose();
    }
    
  }
	  

