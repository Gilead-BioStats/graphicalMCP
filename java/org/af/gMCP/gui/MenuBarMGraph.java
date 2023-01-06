package org.af.gMCP.gui;

import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.io.FileTransfer;
import org.af.commons.logging.LoggingSystem;
import org.af.commons.logging.widgets.DetailsDialog;
import org.af.commons.tools.OSTools;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.dialogs.GraphSaveDialog;
import org.af.gMCP.gui.dialogs.GraphSendToArchiveDialog;
import org.af.gMCP.gui.dialogs.ImageExportDialog;
import org.af.gMCP.gui.dialogs.ParameterDialog;
import org.af.gMCP.gui.dialogs.RObjectLoadingDialog;
import org.af.gMCP.gui.dialogs.RearrangeNodesDialog;
import org.af.gMCP.gui.dialogs.TextFileViewer;
import org.af.gMCP.gui.graph.GraphView;
import org.af.gMCP.gui.graph.WrongInputException;
import org.af.gMCP.gui.options.OptionsDialog;
import org.af.gMCP.gui.power.PowerDialog;
import org.af.gMCP.gui.power.SampleSizeDialog;
import org.af.gMCP.poi.GraphDocXWriter;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.jdesktop.swingworker.SwingWorker;

public class MenuBarMGraph extends JMenuBar implements ActionListener {
	
	GraphView control;
    private static final Log logger = LogFactory.getLog(MenuBarMGraph.class);
    JMenu fmenu = new JMenu("File");
    JMenu extraMenu = new JMenu("Extras");
    JMenu exampleMenu = new JMenu("Example graphs");

	public MenuBarMGraph(GraphView control) {
		
		this.control = control;		

		fmenu.add(makeMenuItem("New Graph", "new graph", KeyEvent.VK_N));
		fmenu.add(makeMenuItem("Load Graph from R", "load graph from R", KeyEvent.VK_L));
		fmenu.add(makeMenuItem("Load Graph from RData file", "load graph"));
		fmenu.addSeparator();
		fmenu.add(makeMenuItem("Load p-Values from R", "load p-values from R"));
		fmenu.addSeparator();
		fmenu.add(makeMenuItem("Save Graph to R", "save graph to R", KeyEvent.VK_S));	
		fmenu.add(makeMenuItem("Save Graph to RData file", "save graph"));		
		fmenu.addSeparator();		
		fmenu.add(makeMenuItem("Export Graph to PNG Image", "export graph image", KeyEvent.VK_P));		
		fmenu.add(makeMenuItem("Export Graph to LaTeX File", "export graph latex", KeyEvent.VK_A));
		fmenu.add(makeMenuItem("Copy Graph to Clip Board", "copy graph to clipboard"));
		fmenu.add(makeMenuItem("Show LaTeX Code for Graph", "show graph latex", KeyEvent.VK_C));
		fmenu.addSeparator();
		fmenu.add(makeMenuItem("Save LaTeX Report", "save latex report", KeyEvent.VK_R));
		fmenu.add(makeMenuItem("Save Word Docx Report (work in progress)", "save docx report", KeyEvent.VK_R));
		/*JMenuItem item = makeMenuItem("Save PDF Report", "save pdf");
		item.setEnabled(false);
		fmenu.add(item);*/
		//fmenu.add(makeMenuItem("Save PDF Report", "save pdf"));
		fmenu.addSeparator();
		createLastUsed();
		fmenu.setMnemonic(KeyEvent.VK_F);
		add(fmenu);
		
		createExampleMenu();
		add(exampleMenu);
		exampleMenu.setMnemonic(KeyEvent.VK_X);

		JMenu menu = new JMenu("Analysis");
		menu.setMnemonic(KeyEvent.VK_A);

		menu.add(makeMenuItem("Graph analysis", "graphAnalysis"));
		//if (Configuration.getInstance().getGeneralConfig().experimentalFeatures()) {
		menu.addSeparator();
		menu.add(makeMenuItem("Power analysis (testing)", "powerAnalysis"));		
		menu.add(makeMenuItem("Sample size calculation (experimental work in progress)", "samplesize"));
		//}

		add(menu);

		createExtraMenu();
		add(extraMenu);

		menu = new JMenu("Help");
		menu.setMnemonic(KeyEvent.VK_H);
		menu.add(makeMenuItem("About", "showAbout", KeyEvent.VK_B));         
		menu.add(makeMenuItem("Introduction to gMCP", "showAppHelp", KeyEvent.VK_I));
		menu.add(makeMenuItem("Weighted parametric tests defined by graphs", "showParametric", KeyEvent.VK_P));
		//menu.add(makeMenuItem("gMCP R Online Reference manual", "showManual", KeyEvent.VK_M));
		menu.add(makeMenuItem("Paper about gMCP in the Biometrical Journal", "showPaper1", KeyEvent.VK_P));
		menu.add(makeMenuItem("References", "showReferences", KeyEvent.VK_R));
		//menu.add(makeMenuItem("Theoretical Background", "showAppHelp"));
		/*menu.addSeparator();
         menu.add(makeMenuItem("Description of Edges with Infinitesimal Small Epsilon Weights", "showEpsDoc"));*/
		menu.addSeparator();
		menu.add(makeMenuItem("Version Info / NEWS", "showNEWS", KeyEvent.VK_N));
		add(menu);

	}
	
	public void createExampleMenu() {
		exampleMenu.removeAll();
		
		JMenu subMenu = new JMenu("Common test procedures for any number of hypotheses");		
		subMenu.add(makeMenuItem("Bonferroni-Holm procedure", "bht"));
		subMenu.add(makeMenuItem("Fixed sequence test", "fixedSequence"));
		subMenu.add(makeMenuItem("Fallback procedure", "fallback"));
		exampleMenu.add(subMenu);
		
		subMenu = new JMenu("3 unstructured hypotheses");
		subMenu.add(makeMenuItem("Improved fallback procedure I", "fallbackI"));
		subMenu.add(makeMenuItem("Improved fallback procedure II", "fallbackII"));
		exampleMenu.add(subMenu);		
		
		subMenu = new JMenu("2 primary & 2 secondary hypotheses");		
		subMenu.add(makeMenuItem("Parallel Gatekeeping with 4 Hypotheses", "pg"));
		subMenu.add(makeMenuItem("Improved Parallel Gatekeeping with 4 Hypotheses", "pgi"));
		subMenu.addSeparator();
		subMenu.add(makeMenuItem("Truncated Holm procedure", "truncHolm"));
		subMenu.addSeparator();
		subMenu.add(makeMenuItem("General successive graph", "gSuccessive"));
		subMenu.add(makeMenuItem("   Simple successive graph I from Maurer et al. (2011)", "successiveI"));
		subMenu.add(makeMenuItem("   Simple successive graph II from Maurer et al. (2011)", "successiveII"));
		subMenu.addSeparator();
		subMenu.add(makeMenuItem("Graph from Hung and Wang (2010)", "hung"));
		subMenu.add(makeMenuItem("Graph from Huque, Alosh and Bhore (2011)", "huque"));
		exampleMenu.add(subMenu);

		subMenu = new JMenu("3 primary & 3 secondary hypotheses");		
		subMenu.add(makeMenuItem("Graph from Bauer et al. (2001)", "bauer"));
		subMenu.add(makeMenuItem("Graph from Bretz et al. (2011)", "bretzEtAl"));
		exampleMenu.add(subMenu);
		
		subMenu = new JMenu("2 primary & 2 secondary & 2 tertiary hypotheses");		
		subMenu.add(makeMenuItem("Graph from Bretz et al. (2009), Figure 14a", "bretzEtAl2009a"));
		subMenu.add(makeMenuItem("Graph from Bretz et al. (2009), Figure 14b", "bretzEtAl2009b"));
		subMenu.add(makeMenuItem("Graph from Bretz et al. (2009), Figure 15", "bretzEtAl2009c"));
		exampleMenu.add(subMenu);
		
		subMenu = new JMenu("Miscellaneous");		
		subMenu.add(makeMenuItem("Graph from Hommel et al. (2007)", "hommelEtAl"));
		subMenu.add(makeMenuItem("Graph from Hommel et al. (2007) simplified", "hommelEtAlSimple"));
		//subMenu.addSeparator();
		subMenu.add(makeMenuItem("Drug clinical trial example (serial gatekeeping) from Maurer et al. (1995)", "maurer1995"));
		subMenu.add(makeMenuItem("Graph I from Ferber et al. (2011)", "ferber2011"));
		subMenu.add(makeMenuItem("Graph II from Ferber et al. (2011)", "ferber2011b"));
		subMenu.add(makeMenuItem("Graph from Wang and Ting (2014)", "wangting2014"));
		exampleMenu.add(subMenu);
		
		subMenu = new JMenu("Entangled graphs");		
		subMenu.add(makeMenuItem("Entangled Graph I from Maurer et al. (2012)", "entangled1"));
		subMenu.add(makeMenuItem("Entangled Graph II from Maurer et al. (2012)", "entangled2"));
		exampleMenu.add(subMenu);
		
		//exampleMenu.add(makeMenuItem("Browse archive of user submitted graphs", "userSubmitted"));
	}
	
	public void createExtraMenu() {
		extraMenu.removeAll();
		extraMenu.add(makeMenuItem("Options", "showOptions", KeyEvent.VK_O));
		extraMenu.add(makeMenuItem("Set all options back to default", "clearOptions", KeyEvent.VK_C));
		extraMenu.addSeparator();
		extraMenu.add(makeMenuItem("Change layout of graph", "changeGraphLayout", KeyEvent.VK_G));
		extraMenu.add(makeMenuItem("Set variables to specific real values", "replaceVariables", KeyEvent.VK_V));
		extraMenu.addSeparator();
		extraMenu.add(makeMenuItem("Log", "showLog", KeyEvent.VK_L));
		extraMenu.add(makeMenuItem("Report error", "reportError", KeyEvent.VK_R));
		//extraMenu.add(makeMenuItem("Submit your own graph to gMCP archive", "submitGraph"));
		extraMenu.addSeparator();
		extraMenu.add(makeMenuItem("Entangled graphs: Add component graph", "entangledGraphs"));	
		if (System.getProperty("eclipse") != null) {		
			extraMenu.add(makeMenuItem("Debug console", "debugConsole", KeyEvent.VK_D));
		}
		extraMenu.setMnemonic(KeyEvent.VK_E);
		if (Configuration.getInstance().getGeneralConfig().experimentalFeatures()) {
			//extraMenu.add(makeMenuItem("Adaptive Designs", "adaptiveDesigns"));
		}
	}

	private void createLastUsed() {
		List<String> graphs = Configuration.getInstance().getGeneralConfig().getLatestGraphs();
		
		for(int i=fmenu.getItemCount()-1; i>16; i--) {
			fmenu.remove(i);
		}
		
		if (graphs.size()>0) {	
			int i = 0;
			for (String graph : graphs) {
				i++;
				String s = graph;
				logger.info("Process last used graph: '"+s+"'.");			
				File f = new File(s);				
				if (f.exists()) {
					String path = f.getParent();
					if (path.length()>20) {
						path = path.substring(0, 17)+"...";
					}
					s = f.getName()+" ["+path+"]";
					fmenu.add(makeMenuItem(i+" "+s, "LOAD_GRAPH"+graph, (i+"").charAt(0)));
				} else {					
					if (s.startsWith("R Object: ")) {
						s = s.substring(10);
						if (RControl.getR().eval("exists(\""+s+"\")", true).asRLogical().getData()[0]) {
							fmenu.add(makeMenuItem(i+" "+s, "LOAD_GRAPH"+graph, (i+"").charAt(0)));
						}
					}
				}				
			}
			fmenu.addSeparator();
		}		
		fmenu.add(makeMenuItem("Exit", "exit", KeyEvent.VK_X));
	}

	private JMenuItem makeMenuItem(String text, String action, int key) {
		JMenuItem item = makeMenuItem(text, action);
		item.setMnemonic(key);
		return item;
	}
	
	private JMenuItem makeMenuItem(String text, String action, char key) {
		JMenuItem item = makeMenuItem(text, action);
		item.setMnemonic(key);
		return item;
	}

	public void loadGraph(String string, boolean global) {
		control.stopTesting();
		control.getNL().loadGraph(string, global);
		control.setGraphName(RControl.getR().eval("gMCP:::nextAvailableName(gMCP:::removeSymbols(\""+string+"\", numbers=FALSE))").asRChar().getData()[0]);
		control.getMainFrame().validate();
	}

	public void newGraph() {
		control.stopTesting();
		control.getNL().reset();		
	}
	
    public void showAbout() {
        new AboutDialog(control.getMainFrame());
    }

    public void showLog() {
        new DetailsDialog(LoggingSystem.getInstance().makeDetailsPanel());
    }
    
    /**
     * Creates an ErrorDialog
     */
    public void reportError() {    	
        ErrorHandler.getInstance().makeErrDialog("Report Error");
    }

	public void actionPerformed(ActionEvent e) {
		ReproducableLog.logGUI("Menu entry \""+e.getActionCommand()+"\"");
        if (e.getActionCommand().startsWith("LOAD_GRAPH")) {        	
        	String s = e.getActionCommand().substring(10);
        	logger.info("Trying to load \""+s+"\"");
        	if (s.startsWith("R Object")) {
        		s = s.substring(10);
        		loadGraph(s, true);
        		Configuration.getInstance().getGeneralConfig().addGraph("R Object: "+s);
            	createLastUsed();
        	} else {
        		File f = new File(s);
        		if (!f.exists()) {
        			JOptionPane.showMessageDialog(control.getMainFrame(), "Could not find file:\n"+s, "Could not find file", JOptionPane.ERROR_MESSAGE);
        			return;
        		}
        		if (s.toLowerCase().endsWith(".rdata")) {
        			loadGraph(f);
        		}
        	}
        } else if (e.getActionCommand().equals("new graph")) {
        	newGraph();			
        } else if (e.getActionCommand().equals("save graph")) {     
        	if (control.getNL().getNodes().size()==0) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Will not save empty graph.", "Saving to R failed.", JOptionPane.ERROR_MESSAGE);
        		return;
        	}
        	control.saveGraph();
        	createLastUsed();
        	control.isGraphSaved = true;
        } else if (e.getActionCommand().equals("save graph to R")) {
        	if (control.getNL().getNodes().size()==0) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Will not save empty graph.", "Saving to R failed.", JOptionPane.ERROR_MESSAGE);
        		return;
        	}
        	GraphSaveDialog vnd = new GraphSaveDialog(control, control.getGraphName());            	
        	String name = vnd.getName();
        	// Save graph (globally).
        	name = control.getNL().saveGraph(name, false, true, vnd.attachPValues(), vnd.attachCorrMat());
        	Configuration.getInstance().getGeneralConfig().addGraph("R Object: "+name);
        	createLastUsed();
        } else if (e.getActionCommand().equals("copy graph to clipboard")) {    
        	new ImageExportDialog(control.getMainFrame(), false);
        } else if (e.getActionCommand().equals("export graph image")) {
    		if (control.getNL().getNodes().size()==0) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Will not save empty graph.", "Empty graph", JOptionPane.ERROR_MESSAGE);
        		return;
        	}
        	new ImageExportDialog(control.getMainFrame(), true);
        } else if (e.getActionCommand().equals("export graph latex")) {       	
        	exportLaTeXGraph();
        } else if (e.getActionCommand().equals("show graph latex")) {       	
        	showLaTeXGraph();
        } else if (e.getActionCommand().equals("save pdf")) {  
        	notYetSupported();
        	//savePDF();
        } else if (e.getActionCommand().equals("save latex report")) {
        	exportLaTeXReport();
        } else if (e.getActionCommand().equals("save docx report")) {
        	exportDocX();
        } else if (e.getActionCommand().equals("load graph")) {       	
        	loadGraph();
        } else if (e.getActionCommand().equals("load graph from R")) {
        	new RObjectLoadingDialog(control.getGraphGUI());
        	createLastUsed();        	
        } else if (e.getActionCommand().equals("showLog")) {    	
        	showLog();
        } else if (e.getActionCommand().equals("reportError")) {       	
        	 reportError();
        } else if (e.getActionCommand().equals("exit")) {       	
        	 control.getMainFrame().windowClosing(null);
        } else if (e.getActionCommand().equals("showAppHelp")) {
        	showPackageFile("doc/gMCP.pdf");       	 	
        } else if (e.getActionCommand().equals("showParametric")) {
        	showPackageFile("doc/parametric.pdf");       	 	
        } else if (e.getActionCommand().equals("showManual")) {
        	showURL("http://cran.at.r-project.org/web/packages/gMCP/gMCP.pdf");
        } else if (e.getActionCommand().equals("showPaper1")) {
        	showURL("http://onlinelibrary.wiley.com/doi/10.1002/bimj.201000239/full");
        } else if (e.getActionCommand().equals("showReferences")) {
        	showPackageFile("References.html");
        } else if (e.getActionCommand().equals("showEpsDoc")) {
        	showPackageFile("doc/EpsilonEdges.pdf");       	 	
        } else if (e.getActionCommand().equals("showNEWS")) {
        	new TextFileViewer(control.getMainFrame(), new File(RControl.getR().eval("system.file(\"NEWS\", package=\"gMCP\")").asRChar().getData()[0]));      	 	
        } else if (e.getActionCommand().equals("showAbout")) {
        	new AboutDialog(control.getMainFrame());
        } else if (e.getActionCommand().equals("showOptions")) {
        	new OptionsDialog(control.getMainFrame());
        } else if (e.getActionCommand().equals("clearOptions")) {
        	Configuration.getInstance().clearConfiguration();
        	control.getMainFrame().repaint();
        } else if (e.getActionCommand().equals("debugConsole")) {
        	RControl.console.setVisible(true);
        } else if (e.getActionCommand().equals("submitGraph")) {
        	if (control.getNL().getNodes().size()==0) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Will not submit empty graph.", "Empty graph", JOptionPane.ERROR_MESSAGE);
        		return;
        	}        	
        	submitGraph();
        } else if (e.getActionCommand().equals("userSubmitted")) {
        	JOptionPane.showMessageDialog(control.getMainFrame(), "This is a brand new feature and there are no user submitted graphs yet.\n"+
        			"Be the first to submit a graph:\n You find the option to do so in the \"Extras\" menu.", 
					"No graphs available.", JOptionPane.INFORMATION_MESSAGE);
        } else if (e.getActionCommand().equals("graphAnalysis")) {
        	if (control.getNL().getNodes().size()==0) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Graph is empty!", "Graph is empty!", JOptionPane.ERROR_MESSAGE);
        		return;
        	}
        	control.getNL().saveGraph(".tmpGraph", false, false);
        	String text = RControl.getR().eval("graphAnalysis(.tmpGraph, file=tempfile())").asRChar().getData()[0];
        	new TextFileViewer(control.getMainFrame(), "Graph analysis", text);
        } else if (e.getActionCommand().equals("powerAnalysis")) {        	
        	if (control.getNL().getNodes().size()==0) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Graph is empty!", "Graph is empty!", JOptionPane.ERROR_MESSAGE);
        		return;
        	}
        	if (control.getMainFrame().getPView().jrbSimes.isSelected()) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Simes test not yet supported in power calculations.", "Simes test not yet supported", JOptionPane.ERROR_MESSAGE);
        		return;
        	}
        	if (control.getNL().getLayer()>1) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Entangled graphs are not yet supported!", "Entangled graphs not supported", JOptionPane.ERROR_MESSAGE);
        		return;
        	}
        	new PowerDialog(control.getMainFrame());
        } else if (e.getActionCommand().equals("samplesize")) {
        	if (control.getNL().getNodes().size()==0) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Graph is empty!", "Graph is empty!", JOptionPane.ERROR_MESSAGE);
        		return;
        	}
        	if (control.getMainFrame().getPView().jrbSimes.isSelected()) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Simes test not yet supported in power calculations.", "Simes test not yet supported", JOptionPane.ERROR_MESSAGE);
        		return;
        	}
        	new SampleSizeDialog(control.getMainFrame());
        } else if (e.getActionCommand().equals("load p-values from R")) {
        	control.loadPValuesFromR(); 
        } else if (e.getActionCommand().equals("changeGraphLayout")) {
        	if (control.getNL().getNodes().size()==0) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "Graph is empty.", "Empty graph", JOptionPane.ERROR_MESSAGE);
        		return;
        	}        	
        	new RearrangeNodesDialog(control.getMainFrame());
        } else if (e.getActionCommand().equals("replaceVariables")) {
        	Set<String> variables = control.getNL().getAllVariables();
        	if (variables.isEmpty() || (variables.size()==1 && variables.contains("ε"))) {
        		JOptionPane.showMessageDialog(control.getMainFrame(), "No variables to replace!", "No variables to replace!", JOptionPane.INFORMATION_MESSAGE);
        		return;
        	}
        	control.getNL().saveGraphWithoutVariables(control.getNL().initialGraph, false, false);
        	control.getNL().loadGraph();
        } else if (e.getActionCommand().equals("entangledGraphs")) {
        	control.addEntangledLayer();
        } else if (e.getActionCommand().equals("adaptiveDesigns")) {
        	
        } else if (e.getActionCommand().equals("bht")) {
        	Hashtable<String,Object> ht = new Hashtable<String,Object>();
        	ht.put("n", new int[] {1,4,10});
        	new ParameterDialog(control.getGraphGUI(), ht, this, "BonferroniHolm");        	
        } else if (e.getActionCommand().equals("fixedSequence")) {
        	Hashtable<String,Object> ht = new Hashtable<String,Object>();
        	ht.put("n", new int[] {1,4,20});
        	new ParameterDialog(control.getGraphGUI(), ht, this, "fixedSequence");        	
        }  else if (e.getActionCommand().equals("fallback")) {
        	Hashtable<String,Object> ht = new Hashtable<String,Object>();
        	ht.put("n", new int[] {1,4,20});
        	ht.put("weights", new double[] {0.25, 0.25, 0.25, 0.25});
        	new ParameterDialog(control.getGraphGUI(), ht, this, "fallback");        	
        } else if (e.getActionCommand().equals("pg")) {       	
        	loadGraph("parallelGatekeeping()", false);
        } else if (e.getActionCommand().equals("pgi")) {       	
        	loadGraph("improvedParallelGatekeeping()", false);
        } else if (e.getActionCommand().equals("bauer")) {       	
        	loadGraph("BauerEtAl2001()", false);
        } else if (e.getActionCommand().equals("bretzEtAl")) {       	
        	loadGraph("BretzEtAl2011()", false);
        } else if (e.getActionCommand().equals("bretzEtAl2009a")) {       	
        	loadGraph("BretzEtAl2009a()", false);
        } else if (e.getActionCommand().equals("bretzEtAl2009b")) {       	
        	loadGraph("BretzEtAl2009b()", false);
        } else if (e.getActionCommand().equals("bretzEtAl2009c")) {       	
        	loadGraph("BretzEtAl2009c()", false);
        } else if (e.getActionCommand().equals("hommelEtAl")) {      	
        	loadGraph("HommelEtAl2007()", false);
        } else if (e.getActionCommand().equals("hommelEtAlSimple")) {       	
        	loadGraph("HommelEtAl2007Simple()", false);
        } else if (e.getActionCommand().equals("hung")) { 	
        	loadGraph("HungEtWang2010()", false);
        } else if (e.getActionCommand().equals("huque")) { 	
        	loadGraph("HuqueAloshEtBhore2011()", false);
        } else if (e.getActionCommand().equals("maurer1995")) {     	
        	loadGraph("MaurerEtAl1995()", false);
        } else if (e.getActionCommand().equals("truncHolm")) {     	
        	loadGraph("truncatedHolm()", false);
        } else if (e.getActionCommand().equals("gSuccessive")) {     	
        	loadGraph("generalSuccessive()", false);
        } else if (e.getActionCommand().equals("successiveI")) {     	
        	loadGraph("simpleSuccessiveI()", false);
        } else if (e.getActionCommand().equals("successiveII")) {     	
        	loadGraph("simpleSuccessiveII()", false);
        } else if (e.getActionCommand().equals("fallbackI")) {     	
        	loadGraph("improvedFallbackI()", false);
        } else if (e.getActionCommand().equals("fallbackII")) {     	
        	loadGraph("improvedFallbackII()", false);
        } else if (e.getActionCommand().equals("ferber2011")) {     	
        	loadGraph("Ferber2011()", false);
        } else if (e.getActionCommand().equals("entangled1")) {     	
        	loadGraph("Entangled1Maurer2012()", false);
        	control.getNL().placeEntangledLegend();
        } else if (e.getActionCommand().equals("entangled2")) {     	
        	loadGraph("Entangled2Maurer2012()", false);
        	control.getNL().placeEntangledLegend();
        } else if (e.getActionCommand().equals("ferber2011b")) {     	
        	Hashtable<String,Object> ht = new Hashtable<String,Object>();
        	ht.put("times", new int[] {1,5,20});
        	ht.put("doses", new int[] {1,3,20});
        	//ht.put("w", new Double(0.5));
        	new ParameterDialog(control.getGraphGUI(), ht, this, "FerberTimeDose2011");
        } else if (e.getActionCommand().equals("wangting2014")) {     	
        	loadGraph("WangTing2014()", false);
        } 
	}
	
	private void submitGraph() {
		new GraphSendToArchiveDialog(control.getMainFrame(), control);		
	}

	public void showURL(String url) {
		try {	
			Method main = Class.forName("java.awt.Desktop").getDeclaredMethod("getDesktop");
			Object obj = main.invoke(new Object[0]);
			Method second = obj.getClass().getDeclaredMethod("browse", new Class[] { URI.class }); 
			second.invoke(obj, new URI(url));
		} catch (Exception exc) {			
			logger.warn("No Desktop class in Java 5 or URI error.");
			RControl.getR().eval("browseURL(\""+url+"\")");
			if (System.getProperty("eclipse") != null) exc.printStackTrace();
		}	
	}

	private void notYetSupported() {
		JOptionPane.showMessageDialog(control.getMainFrame(), "Not yet supported.", "Not yet supported", JOptionPane.INFORMATION_MESSAGE);
	}
	
	String[] pdfViewers = new String[] {
			"evince", "xpdf"
	};

	public void showPackageFile(String s) {
		File f = new File(RControl.getR().eval("system.file(\""+s+"\", package=\"gMCP\")").asRChar().getData()[0]);
		if (OSTools.isWindows() && s.indexOf('.') == -1) {
			try {
				f = FileTransfer.copyFile(f, new File(System.getProperty("java.io.tmpdir"), f.getName()+"TXT"));
			} catch (IOException e) {
				JOptionPane.showMessageDialog(control.getMainFrame(), "Please open and read the following file:\n"+f.getAbsolutePath(), "Could not copy file.", JOptionPane.WARNING_MESSAGE);
			}
		}		
		showFile(f);
	}
	
	public void showFile(File f) {
		if (!f.exists()) {
			throw new RuntimeException("This is strange. The file \""+f.getAbsolutePath()+"\" could not be found.");
		} else {
			try {	
				Method main = Class.forName("java.awt.Desktop").getDeclaredMethod("getDesktop");
				Object obj = main.invoke(new Object[0]);
				Method second = obj.getClass().getDeclaredMethod("open", new Class[] { File.class }); 
				second.invoke(obj, f);
			} catch (Exception exc) {			
				logger.warn("No Desktop class in Java 5 or URI error: "+exc.getMessage(), exc);
				if (f.getName().toLowerCase().endsWith(".html") || f.getName().toLowerCase().endsWith(".htm")) {
					RControl.getR().eval("browseURL(\"file://"+f.getAbsolutePath().replace('\\', '/')+"\")");
					return;
				}
				try {
					if (OSTools.isWindows()) {
						Process p;							
						p = Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler \"" + f.getAbsolutePath()+"\"");
						/*if (s.indexOf('.') == -1) {
							p = Runtime.getRuntime().exec("wordpad \"" + f.getAbsolutePath()+"\"");
						}*/						
						p.waitFor();
					} else {
						boolean opened = false;						
						for (String pdfViewer : pdfViewers) {
							try {
								if (!opened) Runtime.getRuntime().exec(pdfViewer + " "+ f.getAbsolutePath());
								opened = true;
							} catch (Exception ePDF) {
								// Nothing to do here - we will try the next pdfViewer.
							}
						}
						if (!opened) JOptionPane.showMessageDialog(control.getMainFrame(), "Please open and read the following file:\n"+f.getAbsolutePath(), "Could not find appropriate viewer", JOptionPane.WARNING_MESSAGE);
					}
				} catch (Exception e1) {
					logger.error(e1.getMessage(), e1);					
					JOptionPane.showMessageDialog(control.getMainFrame(), "Please open and read the following file:\n"+f.getAbsolutePath(), "Could not find appropriate viewer", JOptionPane.WARNING_MESSAGE);
					if (System.getProperty("eclipse") != null) e1.printStackTrace();
				}

			}
		}
	}
	
	String correlation;
	File f;
	
	public void exportLaTeXReport() {
		if (control.getNL().getNodes().size()==0) {
    		JOptionPane.showMessageDialog(control.getMainFrame(), "Can not create report for empty graph.", "Can not create report for empty graph.", JOptionPane.ERROR_MESSAGE);
    		return;
    	}
		if (!RControl.getR().eval("exists(\""+control.getNL().initialGraph+"\")").asRLogical().getData()[0]) {
			control.getNL().saveGraph(false);
		}
		JFileChooser fc = new JFileChooser(Configuration.getInstance().getClassProperty(this.getClass(), "LaTeXReportDirectory"));
		fc.setDialogType(JFileChooser.SAVE_DIALOG);		
		int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION) {			
			f = fc.getSelectedFile();
			Configuration.getInstance().setClassProperty(this.getClass(), "LaTeXReportDirectory", f.getParent());
			if (!f.getName().toLowerCase().endsWith(".tex")) {
            	f = new File(f.getAbsolutePath()+".tex");
            }
			logger.info("Export to: " + f.getAbsolutePath() + ".");
		} else {
			return;
		}
		control.getMainFrame().glassPane.start();
		//startTesting();
		correlation = control.getPView().getParameters();
		SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
			@Override
			protected Void doInBackground() throws Exception {
				try {
					if (!control.isResultUpToDate()) {
						RControl.getR().evalVoid(control.result+" <- gMCP("+control.getNL().initialGraph+control.getGMCPOptions()+")");
						control.setResultUpToDate(true);
					}
					String filename = f.getAbsolutePath().replaceAll("\\\\", "\\\\\\\\");
					RControl.getR().eval("gMCPReport("+control.result+", file=\""+filename+"\")");
					control.getMainFrame().glassPane.stop();
				} catch (Exception ex) {
					if (ex instanceof WrongInputException) {
						control.getMainFrame().glassPane.stop();
						return null;
					}
					ErrorHandler.getInstance().makeErrDialog(ex.getMessage(), ex);
				}
				return null;
			}  
		};
		worker.execute();
	}
	
	public void exportDocX() {
		if (control.getNL().getNodes().size()==0) {
    		JOptionPane.showMessageDialog(control.getMainFrame(), "Graph is empty.", "Empty graph", JOptionPane.ERROR_MESSAGE);
    		return;
    	} 
		if (!control.isResultUpToDate()) {
			JOptionPane.showMessageDialog(control.getMainFrame(), "Result is not up to date.", "Result not up to date", JOptionPane.WARNING_MESSAGE);
		}
		JFileChooser fc = new JFileChooser(Configuration.getInstance().getClassProperty(this.getClass(), "DocXDirectory"));
		fc.setDialogType(JFileChooser.SAVE_DIALOG);
		File f;
		int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION) {			
			f = fc.getSelectedFile();
			Configuration.getInstance().setClassProperty(this.getClass(), "DocXDirectory", f.getParent());
			if (!f.getName().toLowerCase().endsWith(".docx")) {
            	f = new File(f.getAbsolutePath()+".docx");
            }
			logger.info("Export to: " + f.getAbsolutePath() + ".");
		} else {
			return;
		}
		try {
			(new GraphDocXWriter(control)).createDocXReport(f);			
			try {	
				Method main = Class.forName("java.awt.Desktop").getDeclaredMethod("getDesktop");
				Object obj = main.invoke(new Object[0]);
				Method second = obj.getClass().getDeclaredMethod("open", new Class[] { File.class }); 
				second.invoke(obj, f);
			} catch (Exception exc) {			
				logger.warn("No Desktop class in Java 5 or URI error: "+exc.getMessage(), exc);
			}
		} catch (InvalidFormatException e) {
			new ErrorDialogGMCP("An error occured submitting the graph.", e, false).showDialog();
		} catch (IOException e) {
			JOptionPane.showMessageDialog(control.getGraphGUI(), "Error writing docx report:\n"+e.getMessage(), "Error writing docx report", JOptionPane.ERROR_MESSAGE);
		}
	}
	
	public void writeLaTeX(String s) {
		JFileChooser fc = new JFileChooser(Configuration.getInstance().getClassProperty(this.getClass(), "LaTeXDirectory"));
		fc.setDialogType(JFileChooser.SAVE_DIALOG);
		File f;
		int returnVal = fc.showSaveDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION) {			
			f = fc.getSelectedFile();
			Configuration.getInstance().setClassProperty(this.getClass(), "LaTeXDirectory", f.getParent());
			if (!f.getName().toLowerCase().endsWith(".tex")) {
            	f = new File(f.getAbsolutePath()+".tex");
            }
			logger.info("Export to: " + f.getAbsolutePath() + ".");
		} else {
			return;
		}
		try {
			FileWriter out = new FileWriter(f);
			out.write(LATEX_BEGIN_DOCUMENT);
			out.write(s);
			out.write(LATEX_END_DOCUMENT);
			out.close();
		} catch( Exception ex ) {
			JOptionPane.showMessageDialog(null, "Saving LaTeX code to '" + f.getAbsolutePath() + "' failed: " + ex.getMessage(), "Saving failed.", JOptionPane.ERROR_MESSAGE);
			if (System.getProperty("eclipse") != null) ex.printStackTrace();
		}
	}
	
	public void exportLaTeXGraph() {
		if (control.getNL().getNodes().size()==0) {
    		JOptionPane.showMessageDialog(control.getMainFrame(), "No LaTeX output for empty graph.", "No LaTeX output for empty graph.", JOptionPane.ERROR_MESSAGE);
    		return;
    	}
		writeLaTeX(control.getNL().getLaTeX());
	}
	
	/**
	 * Opens a TextFileViewer dialog and displays the LaTeX source to display the graph.
	 */
	public void showLaTeXGraph() {
		if (control.getNL().getNodes().size()==0) {
    		JOptionPane.showMessageDialog(control.getMainFrame(), "No LaTeX output for empty graph.", "No LaTeX output for empty graph.", JOptionPane.ERROR_MESSAGE);
    		return;
    	}
		String latexCode = control.getNL().getLaTeX();
		Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new StringSelection(latexCode), null);		
		new TextFileViewer(control.getGraphGUI(), "LaTeX code", latexCode, 
				"Also add the following lines to the header of your LaTeX file:\n\\usepackage{tikz}\n\\usetikzlibrary{decorations,arrows,shapes}");
	}
	
	public String LATEX_BEGIN_DOCUMENT = "\\documentclass[11pt]{article}\n"+
										 "\\usepackage{tikz}\n"+
										 "\\usetikzlibrary{decorations,arrows,shapes}\n"+
										 "\\begin{document}\n";
	
	public String LATEX_END_DOCUMENT = "\\end{document}";

	/*
	private void savePDF() {		
		JFileChooser fc = new JFileChooser(Configuration.getInstance().getGeneralConfig().getProjectPDFsPath().getAbsolutePath());		
        fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
        int returnVal = fc.showOpenDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File f = fc.getSelectedFile();
            if (!f.getName().toLowerCase().endsWith(".pdf")) {
            	f = new File(f.getAbsolutePath()+".pdf");
            }
            try {
    			PDFReport pr = new PDFReport(((ControlMGraph) control));
    			pr.makePDF(f);
    		} catch( Exception ex ) {
    			JOptionPane.showMessageDialog(this, "Saving pdf report to '" + f.getAbsolutePath() + "' failed: " + ex.getMessage(), "Saving failed.", JOptionPane.ERROR_MESSAGE);
    			//ex.printStackTrace();
    		}
        }
	}
	*/
	
	/**
	 * Opens a JFileChooser and loads a graph from an RData file.
	 */
	private void loadGraph() {		
		JFileChooser fc = new JFileChooser(Configuration.getInstance().getClassProperty(this.getClass(), "RObjDirectory"));		
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fc.setFileFilter(new FileFilter() {
			public boolean accept(File f) {
				if (f.isDirectory()) return true;
				return f.getName().toLowerCase().endsWith(".rdata");
			}
			public String getDescription () { return "RData files"; }  
		});

        int returnVal = fc.showOpenDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
        	File f = fc.getSelectedFile();
        	loadGraph(f);
        }
        control.getMainFrame().validate();
	}
	
	/**
	 * Loads a graph from a selected RData file into the gMCP environment
	 * and the GUI. An objects already in the gMCP environment with the same name
	 * will be overwritten.
	 * @param f file that contains the graph to load.
	 */
	private void loadGraph(File f) {
		control.stopTesting();            
        Configuration.getInstance().setClassProperty(this.getClass(), "RObjDirectory", f.getParent());
        try {            	
        	//((ControlMGraph) control).getNL().loadFromXML(f);
        	String filename = f.getAbsolutePath().replaceAll("\\\\", "\\\\\\\\"); 
        	// TODO: Check whether more than one object was in the file
    		String loadedGraph = RControl.getR().eval("load(file=\""+filename+"\")").asRChar().getData()[0];
    		RControl.getR().eval(loadedGraph+ "<- gMCP:::updateGraphToNewClassDefinition("+loadedGraph+")");
    		// RControl.getR().eval("assign(\""+loadedGraph+ "\", gMCP:::updateGraphToNewClassDefinition("+loadedGraph+"), envir=globalenv()");
    		loadGraph(loadedGraph, false);
    		Configuration.getInstance().getGeneralConfig().addGraph(f.getAbsolutePath());
        	createLastUsed();
		} catch( Exception ex ) {
			JOptionPane.showMessageDialog(this, "Loading graph from '" + f.getAbsolutePath() + "' failed: " + ex.getMessage(), "Loading failed.", JOptionPane.ERROR_MESSAGE);
			if (System.getProperty("eclipse") != null) ex.printStackTrace();
		}
	}

	protected JMenuItem makeMenuItem(String text, String action) {
        return makeMenuItem(text, action, true);
    }

    protected JMenuItem makeMenuItem(String text, String action, boolean enabled) {
        JMenuItem item = new JMenuItem(text);        
        item.setActionCommand(action);
        item.setEnabled(enabled);
        item.addActionListener(this);
        return (item);
    }

}
