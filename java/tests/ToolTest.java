package tests;

import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.graph.NetList;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ToolTest {

    protected RControl ctrl;
    protected NetList nl;

    @Before
    public void setUp() {
    	ctrl = RControl.getRControl(true);
    }

    @After
    public void cleanUp() {
    }

    @Test
    public void testLoadSave() {
    }
    
  }
	  

