package org.af.gMCP.gui;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.io.IOException;
import java.util.Locale;

import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import org.af.commons.errorhandling.DefaultExceptionHandler;
import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.tools.OSTools;
import org.af.commons.widgets.InfiniteProgressPanel;
import org.af.commons.widgets.InfiniteProgressPanel.AbortListener;
import org.af.commons.widgets.WidgetFactory;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.config.GeneralConfig;
import org.af.gMCP.config.VersionComparator;
import org.af.gMCP.gui.datatable.DataFramePanel;
import org.af.gMCP.gui.datatable.RDataFrameRef;
import org.af.gMCP.gui.dialogs.TellAboutOnlineUpate;
import org.af.gMCP.gui.graph.DView;
import org.af.gMCP.gui.graph.GraphView;
import org.af.gMCP.gui.graph.PView;
import org.af.gMCP.gui.options.OptionsDialog;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.rosuda.REngine.JRI.JRIEngine;

public class CreateGraphGUI extends JFrame implements WindowListener, AbortListener {
	 static { // Static initializer block to set the Locale
		 Locale.setDefault(Locale.ENGLISH);
		 JComponent.setDefaultLocale(Locale.ENGLISH);
		 /* Comment: There are other ways, but if if this is ever changed, 
		  * please check that JFileChooser displays "All Files" instead of
		  * "Alle Dateien" on a German system. 
		  */
	 };
	 
	GraphView control;
	PView pview;
	DView dview;
	public DataFramePanel dfp;
	public InfiniteProgressPanel glassPane;
	protected static Log logger = LogFactory.getLog(CreateGraphGUI.class);
	public static CreateGraphGUI lastCreatedGUI;
	/** If we drop back to zero GUIs we may want to terminate the currently running Java Virtual Machine. */
	public static int countGUIs = 0; 
	public static String helpURL = "http://cran.r-project.org/web/packages/gMCP/vignettes/gMCP.pdf";
	
	/**
	 * Constructor of the GUI main frame
	 * @param graph Variable name for the graph, given as a character string. If the GUI is created by graphGUI the name is valid.
	 * @param pvalues Double array that optionally (if pvalues.length>0) specifies the p-values.
	 * @param debug If true logging will be written to standard out and application will be more verbose.
	 * @param grid Positive integer that sets the grid size for easier placement of nodes.
	 *  (Therefore grid size 1 allows unrestricted placement and disables the grid.)  
	 *  If grid=0 the last used grid value is used or if the GUI is started the first time a value of 50.
	 * @param experimentalFeatures Boolean. If true some unfinished / insufficiently tested experimental features are available in the GUI.
	 */
	public CreateGraphGUI(String graph, double[] pvalues, boolean debug, double grid, boolean experimentalFeatures) {
		super("gMCP GUI");
		countGUIs++;
		System.setProperty("java.net.useSystemProxies","true");
		RControl.getRControl(debug);
		if (grid>0) {
			Configuration.getInstance().getGeneralConfig().setGridSize((int)grid);
		}
		Configuration.getInstance().getGeneralConfig().setExperimental(experimentalFeatures);
		if (System.getProperty("eclipse") != null) helpURL = "http://algorithm-forge.com/gMCP/archive/gMCP.pdf";
		
		/* Get and save R and gMCP version numbers */
		try {		
			Configuration.getInstance().getGeneralConfig().setRVersionNumber(RControl.getR().eval("paste(R.version$major,R.version$minor,sep=\".\")").asRChar().getData()[0]);
			Configuration.getInstance().getGeneralConfig().setVersionNumber(RControl.getR().eval("gMCP:::gMCPVersion()").asRChar().getData()[0]);		
			this.setTitle("gMCP GUI "+Configuration.getInstance().getGeneralConfig().getVersionNumber());
			//The following line may fail if "Date/Publication" is NA: WARN - Package version could not be set: org.rosuda.REngine.REXPLogical cannot be cast to org.rosuda.REngine.REXPString
			Configuration.getInstance().getGeneralConfig().setReleaseDate(RControl.getR().eval("packageDescription(\"gMCP\", fields=\"Date/Publication\")").asRChar().getData()[0]);
		} catch (Exception e) {
			// This is no vital information and will fail for e.g. R 2.8.0, so no error handling here...
			logger.warn("Package version could not be set:\n"+e.getMessage());
		}
		
		// Java 7 does not respect system property "sun.awt.exception.handler".
		// Eventually this fix should be included in afcommons.
		Thread.setDefaultUncaughtExceptionHandler(new DefaultExceptionHandler());
		
		/* Count the number of starts */
		int n = Configuration.getInstance().getGeneralConfig().getNumberOfStarts();
		Configuration.getInstance().getGeneralConfig().setNumberOfStarts(n+1);		
		logger.info("gMCP start No. "+n+1);
		
		setIconImage((new ImageIcon(getClass().getResource("/org/af/gMCP/gui/graph/images/rjavaicon64.png"))).getImage());
		try {
			setLooknFeel();
		} catch (Exception e1) {
			JOptionPane.showMessageDialog(this, "Font size and Look'n'Feel could not be restored.", "Error restoring Look'n'Feel", JOptionPane.ERROR_MESSAGE);
		}
				
		/* 
		 * We want to check for unsaved changes and eventually quit the R console as well, 
		 * so we implement the WindowListener interface and let windowClosing() do the work.
		 */
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(this);
		
		dview = new DView(this);
		dfp = new DataFramePanel(new RDataFrameRef());
		control = new GraphView(graph, this);  // NetList object is created here.
		dfp.registerControl(control);
		pview = new PView(this);
		setJMenuBar(new MenuBarMGraph(control));		
		makeContent();
		
		if (RControl.getR().eval("exists(\""+graph+"\""+Configuration.getInstance().getGeneralConfig().getEnvir()+")", true).asRLogical().getData()[0]) {
			control.getNL().loadGraph(graph, true);
		}
		
		if (System.getProperty("eclipse") != null) {
			((MenuBarMGraph)getJMenuBar()).loadGraph("HungEtWang2010()", false);
		}
		
		if (pvalues.length>0) getPView().setPValues(ArrayUtils.toObject(pvalues));
		glassPane = new InfiniteProgressPanel(this, "Calculating");
	    setGlassPane(glassPane);
	    glassPane.addAbortListener(this);

	    // Place the frame in the middle of the screen with a border of inset = 50 pixel.
		int inset = 50;
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		//System.out.println("Screen width:" + screenSize.width + " Height: " +screenSize.height);
		setBounds(inset, inset,	screenSize.width  - inset*2, screenSize.height - inset*2);
		//getContentPane().setPreferredSize(Toolkit.getDefaultToolkit().getScreenSize());
		setExtendedState(getExtendedState()|JFrame.MAXIMIZED_BOTH);
		setVisible(true);
		
		splitPane1.setDividerLocation(0.75);
		splitPane2.setDividerLocation(0.5);
		splitPane1.setResizeWeight(0.75);
		splitPane2.setResizeWeight(0.5);		
		
		// If this causes trouble look again at the following bug work around:
		// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6409815
		// or perhaps try something like this:
		// http://www.jguru.com/faq/view.jsp?EID=27191

		//TODO Is there really no better way than this kind of strange workaround?!?
		new Thread(new Runnable() {
			public void run() {	
				for (int i=0; i<6; i++) {
					try {
						Thread.sleep(1000);
					} catch (InterruptedException e) {
						logger.warn("Interrupted: "+e.getMessage(), e);
					}				
					splitPane1.setDividerLocation(0.75);
					splitPane2.setDividerLocation(0.5);
				}
			}
		}).start();
		
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {				
				if (!Configuration.getInstance().getGeneralConfig().tellAboutCheckOnline()) {
					new TellAboutOnlineUpate(null);
					Configuration.getInstance().getGeneralConfig().setTellAboutCheckOnline(true);
				}				
				new Thread(new Runnable() {
					public void run() {
						VersionComparator.getOnlineVersion();
					}
				}).start();				
			}
		});
		
		lastCreatedGUI = this;
	}
	
	/**
	 * The following three variables are only needed at start time and ignored after that!
	 */
	static String graphStr;
	static boolean debug;
	static double grid;
	
	public static void startGUI(String graphStr, boolean debug, double grid) {
		CreateGraphGUI.graphStr = graphStr;
		CreateGraphGUI.debug = debug;
		CreateGraphGUI.grid = grid;
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				new CreateGraphGUI(CreateGraphGUI.graphStr, new double[] {}, CreateGraphGUI.debug, CreateGraphGUI.grid, true);
			}
		});		
	}
	
	JSplitPaneBugWorkAround splitPane;
	JSplitPaneBugWorkAround splitPane1;
	JSplitPaneBugWorkAround splitPane2;
	
	private void makeContent() {		
		splitPane1 = new JSplitPaneBugWorkAround(JSplitPane.VERTICAL_SPLIT, control, dview);		
		splitPane2 = new JSplitPaneBugWorkAround(JSplitPane.VERTICAL_SPLIT, new JScrollPane(dfp), new JScrollPane(pview));		
		splitPane = new JSplitPaneBugWorkAround(JSplitPane.HORIZONTAL_SPLIT, splitPane1, splitPane2);		
		getContentPane().add(splitPane);		
	}

	public static void main(String[] args) {
		new CreateGraphGUI("graph", new double[] {}, true,  50, true);
	}

	/**
	 * Closes the R console if we are in bundled mode and checks for unsaved changes. 
	 */
	public void windowClosing(WindowEvent e) {
		if (!control.isGraphSaved && Configuration.getInstance().getGeneralConfig().askWhenGraphIsNotSaved()) {
			int answer = JOptionPane.showConfirmDialog(this, "The current graph is not saved yet!\nDo you want to save it?", 
					"Do you want to save the graph?",
					JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
			if (answer==JOptionPane.CANCEL_OPTION) return;
			if (answer==JOptionPane.YES_OPTION) {
				control.saveGraph();
			}
		}
		countGUIs--;
		if (countGUIs==0) { 
			//System.exit(0);
			//RControl.console.f.dispose();
		}
		if (RControl.getR().eval("exists(\".isBundle\")").asRLogical().getData()[0]) {
			RControl.getR().eval("q(save=\"no\")");
		}
		dispose();
	}
	public void windowDeactivated(WindowEvent e) {}
	public void windowDeiconified(WindowEvent e) {}
	public void windowIconified(WindowEvent e) {}
	public void windowOpened(WindowEvent e) {}
	public void windowActivated(WindowEvent e) {}
	public void windowClosed(WindowEvent e) {}
	
	public PView getPView() {		
		return pview;
	}

	public GraphView getGraphView() {		
		return control;
	}

	public MenuBarMGraph getMBar() {
		return (MenuBarMGraph) this.getJMenuBar();
	}
	
	public void abort() {
		if (RControl.getR().getREngine().getClass() == JRIEngine.class) {
			JRIEngine engine = (JRIEngine) RControl.getR().getREngine();
			engine.getRni().rniStop(0);
			// We try to evaluate some non-trivial command to see whether the REngine works as expected:
			try {
				RControl.getR().eval("mvtnorm::rmvnorm(n=5, mean=c(1,2), sigma=matrix(c(4,2,2,3), ncol=2))");
			} catch (Exception e) {
				/* There is a chance this first evaluation goes
				 * wrong due to protect / unprotect issues caused
				 * by the interrupt. 
				 */
				logger.warn("There was an error in the test eval after interrupt:\n"+e.getMessage(), e);
			}
			// Now the second evaluation should be fine:
			try {
				RControl.getR().eval("mvtnorm::rmvnorm(n=5, mean=c(1,2), sigma=matrix(c(4,2,2,3), ncol=2))");
			} catch (Exception e) {				
				logger.error("There was an error in the 2. test eval after interrupt:\n"+e.getMessage(), e);
				String message = "There was an error interrupting the R calculation.\n"
					+"After you press okay an error dialog will open and please inform us about this.\n"
					+"After that we recommend that you close the GUI (but you can try whether saving of graphs or other things work).";
				JOptionPane.showMessageDialog(this, message, "Error interrupting R calculation", JOptionPane.ERROR_MESSAGE);
				ErrorHandler.getInstance().makeErrDialog("");
			}
		} else {
			logger.error("Could not stop REngine of class '"+RControl.getR().getREngine().getClass()+"'");
		}
	}

	public DView getDView() {		
		return dview;
	}
	
	public DataFramePanel getDataFramePanel() {
		return dfp;
	}
	
	private void setLooknFeel() throws ClassNotFoundException, IllegalAccessException,
	InstantiationException, UnsupportedLookAndFeelException {
		UIManager.setLookAndFeel(Configuration.getInstance().getJavaConfig().getLooknFeel());
		WidgetFactory.setFontSizeGlobal(Configuration.getInstance().getGeneralConfig().getFontSize());
		SwingUtilities.updateComponentTreeUI(this);
	}

	public int getLayerNr() {		
		return getGraphView().getNumberOfLayers();
	}

	public void openHelp(String topic) {
		if (Configuration.getInstance().getGeneralConfig().showOnlineHelp()) {
			getMBar().showURL(helpURL+"#nameddest="+topic);
		} else {
			String manual = "doc/gMCP.pdf";
			File f = new File(RControl.getR().eval("system.file(\""+manual+"\", package=\"gMCP\")").asRChar().getData()[0]);			
			
			String pdfViewerPath = Configuration.getInstance().getGeneralConfig().getPDFViewerPath();
			if (pdfViewerPath.equals(GeneralConfig.UNSET)) {
				int answer = JOptionPane.showConfirmDialog(this, "PDF viewer not configured.\n"
						+ "Do you want to open the options\n"
						+ "to select the PDF viewer?", "PDF viewer not set", JOptionPane.YES_NO_OPTION);
				if (answer == JOptionPane.YES_OPTION) {
					new OptionsDialog(this, OptionsDialog.MISC);
					dispose();
					return;
				}
				return;
			}				
			String parameter = "";
			if (pdfViewerPath.toLowerCase().contains("acro")){
				parameter = "nameddest="+topic+"";
			}
			String cmdString = "\"" + pdfViewerPath + "\" /A \""+ parameter + "\" \"" + f.getAbsolutePath() + "\""; 
			
			String[] cmdarray = new String[] {pdfViewerPath, f.getAbsolutePath()};
			if (!parameter.isEmpty()) {
				cmdarray = new String[] {pdfViewerPath, "/A", parameter, f.getAbsolutePath()};	
			}
			try {			
				if (OSTools.isWindows()) {
					System.out.println("Starting: "+cmdString);
					Process p = Runtime.getRuntime().exec(cmdString);
				} else {
					Process p = Runtime.getRuntime().exec(cmdarray);
				}
			} catch (IOException e) {
				ErrorHandler.getInstance().makeErrDialog(e.getMessage(), e, false);
				e.printStackTrace();
			}

		}
		//TODO If this does not work (or options are set? or generally?) open local file.
	}
	
}
