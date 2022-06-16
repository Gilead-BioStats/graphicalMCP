package org.af.gMCP.gui.graph;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Date;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToggleButton;
import javax.swing.filechooser.FileFilter;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.widgets.DesktopPaneBG;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.ReproducableLog;
import org.af.gMCP.gui.datatable.DataFramePanel;
import org.af.gMCP.gui.dialogs.AdjustedPValueDialog;
import org.af.gMCP.gui.dialogs.DialogConfIntEstVar;
import org.af.gMCP.gui.dialogs.GraphSaveDialog;
import org.af.gMCP.gui.dialogs.RejectedDialog;
import org.af.gMCP.gui.dialogs.VariableNameDialog;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jdesktop.swingworker.SwingWorker;

public class GraphView extends JPanel implements ActionListener {

	String name;
	CreateGraphGUI parent;

	public static final String STATUSBAR_DEFAULT = "Place new nodes and edges or start the test procedure";
	JLabel statusBar;
	public NetList nl;
	
	JToggleButton buttonNewNode;
	JButton buttonNewEdge;
	JButton buttonZoomOut;
	JButton buttonZoomIn;
	JButton buttonadjPval;
	JButton buttonConfInt;
	JButton buttonStart;	
	JButton buttonBack;

	String alternatives;
	String correlation = "";
	public String result = ".gMCPResult_" + (new Date()).getTime();
	protected boolean resultUpToDate = false;

	private static final Log logger = LogFactory.getLog(GraphView.class);
	
	public String getGraphName() {		
		return name;
	}

	public CreateGraphGUI getMainFrame() {		
		return parent;
	}

	public PView getPView() {		
		return parent.getPView();
	}

	public void updateEdge(int from, int to, Double w, int layer) {
		updateEdge(from, to, new EdgeWeight(w), layer);
	}

	public CreateGraphGUI getGraphGUI() {
		return parent;
	}
	
	public GraphView(String graph, CreateGraphGUI createGraphGUI) {
		this.name = graph;
		this.parent = createGraphGUI;
		statusBar = new JLabel(STATUSBAR_DEFAULT);
		nl = new NetList(statusBar, this);
		setLayout(new BorderLayout());
		add("North", getNorthPanel());		
		JScrollPane sPane = new JScrollPane(nl);
		add("Center", sPane);
    }
	
	public void setGraphName(String name) {
		this.name = name;
	}

	public JPanel getNorthPanel() {
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add("North", getToolBar());		
		panel.add("South", statusBar);
		return panel;
	}

	public JPanel getToolBar() {
		JPanel toolPanel = new JPanel();
		try {
			toolPanel.setLayout(new FlowLayout());
			((FlowLayout) (toolPanel.getLayout()))
					.setAlignment(FlowLayout.LEFT);
			
			buttonNewNode = new JToggleButton(
					new ImageIcon(ImageIO.read(DesktopPaneBG.class
											.getResource("/org/af/gMCP/gui/graph/images/vertex.png"))));
			toolPanel.add(buttonNewNode);
			buttonNewNode.addActionListener(this);
			buttonNewNode.setToolTipText("new vertex");
			
			buttonNewEdge = new JButton(
					new ImageIcon(ImageIO.read(DesktopPaneBG.class
											.getResource("/org/af/gMCP/gui/graph/images/edge.png"))));
			toolPanel.add(buttonNewEdge);
			buttonNewEdge.addActionListener(this);
			buttonNewEdge.setToolTipText("new edge");
			
			buttonZoomOut = new JButton(
					new ImageIcon(ImageIO.read(DesktopPaneBG.class
											.getResource("/org/af/gMCP/gui/graph/images/zoom_out.png"))));
			toolPanel.add(buttonZoomOut);
			buttonZoomOut.addActionListener(this);
			buttonZoomOut.setToolTipText("zoom out");
			
			buttonZoomIn = new JButton(
					new ImageIcon(ImageIO.read(DesktopPaneBG.class
											.getResource("/org/af/gMCP/gui/graph/images/zoom_in.png"))));
			toolPanel.add(buttonZoomIn);
			buttonZoomIn.addActionListener(this);
			buttonZoomIn.setToolTipText("zoom in");
			
			buttonStart = new JButton(
					new ImageIcon(ImageIO.read(DesktopPaneBG.class
											.getResource("/org/af/gMCP/gui/graph/images/StartTesting.png"))));
			toolPanel.add(buttonStart);
			buttonStart.addActionListener(this);
			buttonStart.setEnabled(false);
			buttonStart.setToolTipText("start testing");		
			
			buttonadjPval = new JButton(
					new ImageIcon(ImageIO.read(DesktopPaneBG.class
											.getResource("/org/af/gMCP/gui/graph/images/adjPval.png"))));
			toolPanel.add(buttonadjPval);			
			buttonadjPval.addActionListener(this);
			buttonadjPval.setEnabled(false);
			buttonadjPval.setToolTipText("calculate adjusted p-values");
			
			buttonConfInt = new JButton(
					new ImageIcon(ImageIO.read(DesktopPaneBG.class
											.getResource("/org/af/gMCP/gui/graph/images/confint2.png"))));
			toolPanel.add(buttonConfInt);
			buttonConfInt.addActionListener(this);
			buttonConfInt.setEnabled(false);
			buttonConfInt.setToolTipText("calculate confidence intervals");
			
		} catch (IOException e) {
			ErrorHandler.getInstance().makeErrDialog(e.getMessage(), e);
		}
		return toolPanel;
	}

	public NetList getNL() {
		return nl;
	}
	
	public void actionPerformed(ActionEvent e) {
		if (!e.getSource().equals(buttonNewNode)) {
			nl.setNewVertex(false);
			buttonNewNode.setSelected(false);
		}
		if (!e.getSource().equals(buttonNewEdge)) {
			nl.setNewEdge(false);			
		}
		if (e.getSource().equals(buttonZoomIn)) {
			nl.setZoom(nl.getZoom() * 1.25);
			getNL().refresh();
		} else if (e.getSource().equals(buttonZoomOut)) { 
			if (nl.getZoom()>0.025) {
				nl.setZoom(nl.getZoom() / 1.25);
				getNL().refresh();
			} else {
				JOptionPane.showMessageDialog(parent, "Highest zoom level. This is no Mandelbrot set.", "Highest zoom level", JOptionPane.INFORMATION_MESSAGE);
			}
		} else if (e.getSource().equals(buttonNewEdge)) {
			ReproducableLog.logGUI("Button \"new edge\"");			
			nl.setNewEdge(true);
			getNL().statusBar.setText("Select a node from which this edge should start.");
		} else if (e.getSource().equals(buttonNewNode)) {
			ReproducableLog.logGUI("Button \"new node\"");
			nl.setNewVertex(buttonNewNode.isSelected());
			nl.repaint();
			getNL().statusBar.setText("Click on the graph panel to place the node.");
		} else if (e.getSource().equals(buttonConfInt)) {
			ReproducableLog.logGUI("Button \"confint\"");
			if (!getNL().isTesting()) {
				getPView().savePValues();
				getNL().saveGraph(getNL().resetGraph, false, false);
				getNL().saveGraphWithoutVariables(getNL().initialGraph, false, false);
	        	getNL().loadGraph();
	        	getPView().restorePValues();
	        	showParamInfo();
			}
			if (getNL().getNodes().size()==0) {
				JOptionPane.showMessageDialog(parent, "Please create first a graph.", "Please create first a graph.", JOptionPane.ERROR_MESSAGE);
			} else {
				parent.glassPane.start();
				//startTesting();
				correlation = parent.getPView().getParameters();
				SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
					@Override
					protected Void doInBackground() throws Exception {
						try {
							if (!isResultUpToDate()) {
								rCode = RControl.setSeed();
								RControl.getR().evalVoid(result+" <- gMCP("+getNL().initialGraph+getGMCPOptions()+")");
								rCode += RControl.getR().eval("gMCP:::createGMCPCall("+getNL().initialGraph+getGMCPOptions()+")").asRChar().getData()[0];
								setResultUpToDate(true);
							}
							double[] alpha = RControl.getR().eval(""+getPView().getTotalAlpha()+"*getWeights("+result+")").asRNumeric().getData();
							boolean[] rejected = RControl.getR().eval("getRejected("+result+")").asRLogical().getData();
							parent.glassPane.stop();
							new DialogConfIntEstVar(parent, nl, rejected, alpha);
						} catch (Exception ex) {
							parent.glassPane.stop();
							if (ex instanceof WrongInputException) {								
								JOptionPane.showMessageDialog(parent, "Invalid values in input fields!", "Invalid values in input fields!", JOptionPane.ERROR_MESSAGE);
								return null;
							}
							ErrorHandler.getInstance().makeErrDialog(ex.getMessage(), ex);
						}
						return null;
					}  
				};
				worker.execute();				
			}
		} else if (e.getSource().equals(buttonStart)) {
			ReproducableLog.logGUI("Button \"start testing\"");
			if (!getNL().isTesting()) {				
				getPView().savePValues();
				getNL().saveGraph(getNL().resetGraph, false, false);
				getNL().saveGraphWithoutVariables(getNL().initialGraph, false, false);
	        	getNL().loadGraph();
	        	getPView().restorePValues();
	        	showParamInfo();
				parent.glassPane.start();				
				startTesting();
				correlation = parent.getPView().getParameters();
				SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
					@Override
					protected Void doInBackground() throws Exception {
						try {
							if (!isResultUpToDate()) {
								rCode = RControl.setSeed();
								RControl.getR().evalVoid(result+" <- gMCP("+getNL().initialGraph+getGMCPOptions()+")");
								rCode += RControl.getR().eval("gMCP:::createGMCPCall("+getNL().initialGraph+getGMCPOptions()+")").asRChar().getData()[0];
								setResultUpToDate(true);
							}
							boolean[] rejected = RControl.getR().eval(result+"@rejected").asRLogical().getData();
							String output = null;
							if (Configuration.getInstance().getGeneralConfig().verbose() && RControl.getR().eval("!is.null(attr("+result+", \"output\"))").asRLogical().getData()[0]) {
								output = RControl.getR().eval("attr("+result+", \"output\")").asRChar().getData()[0];
							}							
							new RejectedDialog(parent, rejected, parent.getGraphView().getNL().getNodes(), output, rCode);
						} catch (Exception ex) {							
							if (ex instanceof WrongInputException) {								
								JOptionPane.showMessageDialog(parent, "Invalid values in input fields!", "Invalid values in input fields!", JOptionPane.ERROR_MESSAGE);
								return null;
							}
							ErrorHandler.getInstance().makeErrDialog(ex.getMessage(), ex);
						} finally {
							parent.glassPane.stop();
						}
						return null;
					}  
				};
				worker.execute();				
			} else {
				stopTesting();
			}
		} else if (e.getSource().equals(buttonadjPval)) {
			ReproducableLog.logGUI("Button \"calculate p-values\"");
			if (getNL().getNodes().size()==0) {
				JOptionPane.showMessageDialog(parent, "Please create first a graph.", "Please create first a graph.", JOptionPane.ERROR_MESSAGE);				
			} else {
				if (!getNL().isTesting()) {
					getPView().savePValues();
					getNL().saveGraph(getNL().resetGraph, false, false);
					getNL().saveGraphWithoutVariables(getNL().initialGraph, false, false);
		        	getNL().loadGraph();					
					getPView().restorePValues();
					showParamInfo();
				}
				parent.glassPane.start();
				//startTesting();
				correlation = parent.getPView().getParameters();
				SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
					@Override
					protected Void doInBackground() throws Exception {
						try {							
							if (!isResultUpToDate()) {
								rCode = RControl.setSeed();
								RControl.getR().evalVoid(result+" <- gMCP("+getNL().initialGraph+getGMCPOptions()+")");
								rCode += RControl.getR().eval("gMCP:::createGMCPCall("+getNL().initialGraph+getGMCPOptions()+")").asRChar().getData()[0];
								setResultUpToDate(true);
							}
							double[] adjPValues = RControl.getR().eval(result+"@adjPValues").asRNumeric().getData();
							parent.glassPane.stop();
							new AdjustedPValueDialog(parent, getPView().pValues, adjPValues, getNL().getNodes());
						} catch (Exception ex) {
							parent.glassPane.stop();
							if (ex instanceof WrongInputException) {								
								JOptionPane.showMessageDialog(parent, "Invalid values in input fields!", "Invalid values in input fields!", JOptionPane.ERROR_MESSAGE);
								return null;
							}
							ErrorHandler.getInstance().makeErrDialog(ex.getMessage(), ex);
						}
						return null;
					}  
				};
				worker.execute();
			}
		}
	}
	
	/**
	 * rCode is just a helper variable that is used to give the r code
	 * to run to anonymous subclasses of SwingWorker.
	 * See method actionPerformed(ActionEvent e) where it is used.
	 */
	public String rCode = "";
	public boolean isGraphSaved = true;

	private void showParamInfo() {
		if (parent.getPView().jrbRCorrelation.isSelected()) {
    		if (!Configuration.getInstance().getClassProperty(this.getClass(), "showParamInfo", "yes").equals("no")) {
    			JCheckBox tellMeAgain = new JCheckBox("Don't show me this info again.");
    			String message = 
    					"The parametric test that takes correlation into\n"+
    					"account is appropriate if the p-values\n" +
    					"belong to one-sided test-statistics with a joint\n" +
    					"multivariate normal distribution under the null.\n" +
    					"(The Bonferroni- and Simes-based tests do not\n" +
    					"need this assumption).";
    			JOptionPane.showMessageDialog(parent, new Object[] {message, tellMeAgain}, "Info", JOptionPane.INFORMATION_MESSAGE);
    			if (tellMeAgain.isSelected()) {
    				Configuration.getInstance().setClassProperty(this.getClass(), "showParamInfo", "no");
    			}
    		}
    	}
	}

	public void stopTesting() {
		if (!getNL().testingStarted) return;
		getNL().stopTesting();
		getNL().reset();
		getNL().loadGraph(getNL().resetGraph, false);
		getDataFramePanel().setTesting(false);
		getPView().restorePValues();
		getPView().setTesting(false);
		getPView().revalidate();
		getPView().repaint();
		buttonNewNode.setEnabled(true);
		buttonNewEdge.setEnabled(true);
		try {
			buttonStart.setIcon(new ImageIcon(ImageIO.read(DesktopPaneBG.class
					.getResource("/org/af/gMCP/gui/graph/images/StartTesting.png"))));
		} catch (IOException ex) {
			ErrorHandler.getInstance().makeErrDialog(ex.getMessage(), ex);
		}
	}

	public void startTesting() {
		//getPView().savePValues();
		if (getNL().testingStarted || !getPView().jrbNoCorrelation.isSelected()) return;
		getPView().savePValues();
		try {
			getNL().startTesting();
			getNL().saveGraph(false);
			getDataFramePanel().setTesting(true);
			getPView().setTesting(true);			
			buttonNewNode.setEnabled(false);
			buttonNewEdge.setEnabled(false);				
			buttonStart.setIcon(new ImageIcon(ImageIO.read(DesktopPaneBG.class
					.getResource("/org/af/gMCP/gui/graph/images/Reset.png"))));
		} catch (Exception ex) {
			ErrorHandler.getInstance().makeErrDialog(ex.getMessage(), ex);
		} 
	}
	
	
	public void WriteLaTeXwithR() {
		JFileChooser fc = new JFileChooser();
		File file;
		int returnVal = fc.showSaveDialog(getMainFrame());
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			file = fc.getSelectedFile();			
		} else {
			return;
		}
		String filename = file.getAbsolutePath();
		nl.saveGraph(".exportGraphToLaTeX", false, false);
		RControl.getR().eval("gMCPReport(.exportGraphToLaTeX, file=\""+filename+"\")");
	}

	public void updateEdge(int from, int to, EdgeWeight weight, int layer) {
		logger.info("Adding Edge from "+from+" to "+to+" with weight "+weight.toString()+".");
		Edge e = getNL().findEdge(getNL().getNodes().get(from), getNL().getNodes().get(to), layer);
		if (e!=null) {
			int x = e.getK1();
			int y = e.getK2();			
			if (!weight.toString().equals("0")) {
				getNL().setEdge(new Edge(getNL().getNodes().get(from), getNL().getNodes().get(to), weight, getNL(), x, y, layer));
			} else {
				getNL().removeEdge(e);
			}
		} else {
			getNL().setEdge(getNL().getNodes().get(from), getNL().getNodes().get(to), weight, layer);
		}
		getNL().repaint();		
	}

	public void enableButtons(Boolean enabled) {
		buttonadjPval.setEnabled(enabled);
		if (getPView().jrbNoCorrelation.isSelected()) buttonConfInt.setEnabled(enabled);
		buttonStart.setEnabled(enabled);
	}

	public String getGMCPOptions() throws Exception {
		return ","+getPView().getPValuesString()
				+ correlation
				+", alpha="+getPView().getTotalAlpha()
				+", eps="+Configuration.getInstance().getGeneralConfig().getEpsilon()
				+", verbose="+(Configuration.getInstance().getGeneralConfig().verbose()?"TRUE":"FALSE"+", callFromGUI=TRUE")
			    +", upscale = "+(Configuration.getInstance().getGeneralConfig().getUpscale()?"TRUE":"FALSE")
				//+", alternatives="+alternatives
			    ;
	}

	public DView getDView() {
		return 	parent.getDView();	
	}
	
	public void saveGraphImage(File file, boolean drawHypNames, boolean drawHypWeights, boolean drawEdgeWeights) {
		BufferedImage img = getNL().getImage(Configuration.getInstance().getGeneralConfig().getExportZoom(), Configuration.getInstance().getGeneralConfig().getColoredImages(), drawHypNames, drawHypWeights, drawEdgeWeights);
		try {
			ImageIO.write(img, "png", file);
		} catch( Exception ex ) {
			JOptionPane.showMessageDialog(this, "Saving image to '" + file.getAbsolutePath() + "' failed: " + ex.getMessage(), "Saving failed.", JOptionPane.ERROR_MESSAGE);
		}
	}
	
	public void saveGraph() {
		JFileChooser fc = new JFileChooser(Configuration.getInstance().getClassProperty(this.getClass(), "RObjDirectory"));		
        fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fc.setDialogType(JFileChooser.SAVE_DIALOG);
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
            Configuration.getInstance().setClassProperty(this.getClass(), "RObjDirectory", f.getParent());
            if (!f.getName().toLowerCase().endsWith(".rdata")) {
            	f = new File(f.getAbsolutePath()+".RData");
            }
            try {
            	GraphSaveDialog vnd = new GraphSaveDialog(this, getGraphName());            	
            	String name = vnd.getName();
            	// Save graph (globally).
            	name = getNL().saveGraph(name, false, false, vnd.attachPValues(), vnd.attachCorrMat()); 
            	String filename = f.getAbsolutePath().replaceAll("\\\\", "\\\\\\\\");            	
            	RControl.getR().eval("save("+name+", file=\""+filename+"\")");        		
            	JOptionPane.showMessageDialog(getMainFrame(), "Exported graph to R object '"+name+"' and saved this to \n'" + f.getAbsolutePath() + "'.", "Saved graph", JOptionPane.INFORMATION_MESSAGE);
            	Configuration.getInstance().getGeneralConfig().addGraph(f.getAbsolutePath());
    		} catch( Exception ex ) {
    			JOptionPane.showMessageDialog(getMainFrame(), "Saving graph to '" + f.getAbsolutePath() + "' failed: " + ex.getMessage(), "Saving failed", JOptionPane.ERROR_MESSAGE);
    		}
        }	
	}

	public void loadPValuesFromR() {
		VariableNameDialog vnd = new VariableNameDialog(getGraphGUI());     
		try {
			double[] data = RControl.getR().eval(vnd.getName()).asRNumeric().getData();
			if (data.length!=getNL().getNodes().size()) {
				JOptionPane.showMessageDialog(getMainFrame(), "Number of hypotheses and values do not match.", 
						"Number of hypotheses and values do not match", JOptionPane.ERROR_MESSAGE);
				return;
			}
			getPView().setPValues(ArrayUtils.toObject(data));					
		} catch (Exception ex) {
			JOptionPane.showMessageDialog(this, "Error loading values from R:\n"+ex.getMessage(), 
					"Error loading values from R", JOptionPane.ERROR_MESSAGE);
		}		
	}
	
	public boolean isResultUpToDate() {
		return resultUpToDate;
	}

	public void setResultUpToDate(boolean resultUpToDate) {
		//statusBar.setText("Result up-to-date: "+resultUpToDate);
		this.resultUpToDate = resultUpToDate;
	}

	public void renameNode(Node node, String name) {
		int i = getNL().whichNode(node.getName());
		parent.getPView().renameNode(i, name);
		parent.getDataFramePanel().renameNode(i, name);
		node.setName(name);		
	}

	public DataFramePanel getDataFramePanel() {		
		return parent.dfp;
	}

	public int getNumberOfLayers() {
		return getDataFramePanel().getTable().size();
	}

	public void addEntangledLayer() {
		getDataFramePanel().addLayer();
		nl.addEntangledLayer();
		getPView().addEntangledLayer();		
	}
	
	/* This method should be called only from DataFramePanel.removeLayer()
	 * or if not, DataFramePanel.removeLayer() must be called separately.
	 */
	public void removeEntangledLayer(int layer) {
		nl.removeEntangledLayer(layer);
		getPView().removeEntangledLayer(layer);
	}
}
