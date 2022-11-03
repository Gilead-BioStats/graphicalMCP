package org.af.gMCP.tests;

import org.af.gMCP.gui.RControl;
import org.af.jhlir.backends.rengine.RCallServicesREngine;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.REngineException;
import org.rosuda.REngine.JRI.JRIEngine;

public class TestImportExport {

	public TestImportExport(String graph1, String graph2) throws REngineException, REXPMismatchException {
		System.out.println(((JRIEngine)RControl.getR().getREngine()).parseAndEval("x <- \"\\\\omega\"").asString());
		System.out.println(((JRIEngine)RControl.getR().getREngine()).parseAndEval("x").asString());
		/*vs.nl = new NetList(new JLabel(""), vs, new GraphView(graph1, null));
		GraphMCP jGraph = new GraphMCP(graph1, vs);
		vs.nl.saveGraph(graph1, false);*/
	}
	
	/**
	 * @param args
	 * @throws REXPMismatchException 
	 * @throws REngineException 
	 */
	public static void main(String[] args) throws REngineException, REXPMismatchException {
		RCallServicesREngine r = RControl.getRControl(true).getR();
		//r.eval("library(gMCP)");
		//r.eval("graph <- createGraphFromBretzEtAl()");
		//r.eval("graph <- improvedParallelGatekeeping()");
		new TestImportExport("graph","graphExport");
	}

}
