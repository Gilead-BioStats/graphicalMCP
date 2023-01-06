package tests;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;

import org.af.gMCP.gui.RControl;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class RControlTest {

    private RControl ctrl;

    @Before
    public void setUp() {
    	ctrl = RControl.getRControl(true);
    }

    @After
    public void cleanUp() {
    }

    @Test
    public void testEmptyCollection() {
        Collection collection = new ArrayList();
        assertTrue(collection.isEmpty());
    }
    
    @Test
    public void numericTransfer() {
    	for (int i = 0; i < 1000; i++) {
    		Double d = RControl.getR().eval("x <- runif(1)").asRNumeric().getData()[0];
    		assertTrue(RControl.getR().eval("x =="+d).asRLogical().getData()[0]);
    	}
    }

    @Test(expected= IndexOutOfBoundsException.class) public void empty() { 
        new ArrayList<Object>().get(0); 
    }
    
  }
	  

