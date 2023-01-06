package org.af.gMCP.gui.graph;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.Arrays;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.ReproducableLog;
import org.af.gMCP.gui.dialogs.VariableDialog;
import org.af.gMCP.gui.graph.annotations.Annotation;
import org.af.gMCP.gui.graph.annotations.AnnotationPanel;
import org.af.gMCP.gui.graph.annotations.Legend;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class NetList extends JTabbedPane implements ChangeListener, AnnotationPanel {

	private static final Log logger = LogFactory.getLog(NetList.class);
	GraphView control;
	
	protected Vector<Node> nodes = new Vector<Node>();
	protected Vector<Edge> edges = new Vector<Edge>();
	protected Vector<Annotation> annotations = new Vector<Annotation>();
	
	public GraphMCP graph;	
	
	public List<NetListPanel> nlp = new Vector<NetListPanel>();
	
	public String resetGraph = ".ResetGraph" + (new Date()).getTime();
	public String tmpGraph = ".tmpGraph" + (new Date()).getTime();
	public String initialGraph = ".InitialGraph" + (new Date()).getTime();
	
	JLabel statusBar;
	
	Legend entangledLegend = null; 

	public boolean testingStarted = false;	
	
	/** Counts the number of layers. Always greater 0, starts with 1. */
	private int layer = 1;
	
	/**
	 * For faster loading and resets the variable updateGUI exists.
	 * When a graph is changed rapidly at more than one place,
	 * it is set to false, the graph is changed, it is set back
	 * to true and after that graphHasChanged() is called once.
	 */
	public boolean updateGUI = true;	
	
	double zoom = 1d;
	
	public NetList(JLabel statusBar, GraphView graphview) {
		addChangeListener(this);
		this.statusBar = statusBar;
		this.control = graphview;
		Font f = statusBar.getFont();
		statusBar.setFont(f.deriveFont(f.getStyle() ^ Font.BOLD));
		nlp.add(new NetListPanel(this, null));
		addTab("Graph", nlp.get(0));
	}
	
	public void acceptNode(Node node) {
		control.getPView().savePValues();
		saveGraph(".tmpGraph", false, false);
		RControl.getR().eval(".tmpGraph <- substituteEps(.tmpGraph, eps="+Configuration.getInstance().getGeneralConfig().getEpsilon()+")");
		ReproducableLog.logR(RControl.getR().eval("gMCP:::dputGraph(.tmpGraph, \".tmpGraph\")").asRChar().getData()[0]);
		RControl.evalAndLog(".tmpGraph <- rejectNode(.tmpGraph, \""+node.getName()+"\""
				+", upscale = "+(Configuration.getInstance().getGeneralConfig().getUpscale()?"TRUE":"FALSE")+")");
		reset();
		new GraphMCP(".tmpGraph", this);
		control.getPView().restorePValues();
	}

	public void addDefaultNode(int x, int y) {
		int i = nodes.size() + 1;
		String name = "H" + i;		
		while (whichNode(name) != -1) {
			i = i + 1;
			name = "H" + i;
		}
		double[] weights = new double[layer];
		addNode(new Node(name, x, y, weights, this));		
	}
	
	public Point getMaxPoint() {
		int maxX = 0;
		int maxY = 0;
		for (Node node : getNodes()) {
			if (node.getX() > maxX)
				maxX = node.getX();
			if (node.getY() > maxY)
				maxY = node.getY();
		}
		for (Edge edge : getEdges()) {
			if (edge.getK1() > maxX)
				maxX = edge.getK1();
			if (edge.getK2() > maxY)
				maxY = edge.getK2();
		}
		return new Point(maxX, maxY);
	}
	
	public void placeEntangledLegend() {
		if (entangledLegend!=null) {
			Point p = getMaxPoint();
			entangledLegend.setX(100);
			entangledLegend.setY((int) p.getY() + 100);
		}		
	}
	
	public void addEntangledLayer() {		
		if (layer==1) {
			setTitleAt(0, "Combined");
			nlp.add(new NetListPanel(this, 0));
			addTab("Graph 1", nlp.get(nlp.size()-1));
		}
		
		nlp.add(new NetListPanel(this, layer));		
		layer++;				
		addTab("Graph "+layer, nlp.get(nlp.size()-1));
		for (Node n : nodes) {
			n.addLayer();
		}
		//System.out.println("Number of Layers:" + layer);
		
		Vector<String> vl = new Vector<String>();
		vl.add("Component Weights");			
		for (int i=0; i<layer; i++) {
			String weight = "";
			try {
				 weight = control.getPView().entangledWeights.get(i).getText();
			} catch (Exception e) {
				// Not yet created.
			}
			vl.add("Component Graph "+(i+1)+": "+ weight);
		}
		
		int x = -1, y = -1;
		if (entangledLegend!=null) {
			x = entangledLegend.getX();
			y = entangledLegend.getY();
		} else {
			Point p = getMaxPoint();
			x = 100;
			y = (int) p.getY() + 50;			
		}
		annotations.remove(entangledLegend);
		Color[] colors = new Color[layer+1];
		colors[0] = Color.BLACK;
		for (int i = 1; i<layer+1; i++) {
			colors[i] = NetListPanel.layerColors[(i-1)%NetListPanel.layerColors.length];
		}
		entangledLegend = new Legend(x, y, vl, Arrays.asList(colors), this);
		
		annotations.add(entangledLegend);		
		
		refresh();
	}

	public void addNode(Node node) {
		control.enableButtons(true);		
		nodes.add(node);
		control.getPView().addPPanel(node);
		control.getDataFramePanel().addRowCol(node.getName());
		calculateSize();
		graphHasChanged();
	}

	private int askForLayer() {
		int layer = 0;
		if (control.getNumberOfLayers()>1) {
			//We could ask with a JOptionPane window for the layer - but for now we just take the active tab from the DataFramePanel:
			layer = control.getDataFramePanel().getSelectedIndex();
		}
		return layer;
	}
	
	private void calculateSize() {
		for (NetListPanel n : nlp) {
			n.calculateSize();
		}
	}

	public Edge findEdge(Node von, Node nach, int layer) {
		for (Edge e : edges) {
			if (von == e.from && nach == e.to && e.layer == layer) {
				return e;
			}
		}
		return null;
	}

	public NetListPanel getActiveNLP() {
		return(nlp.get(getSelectedIndex()));		
	}
	
	public Set<String> getAllVariables() {
		Set<String> variables = new HashSet<String>();		
		for (Edge e : edges) {		
			variables.addAll(e.getVariable());
		}
		return variables;
	}

	public Vector<Edge> getEdges() {
		return edges;
	}

	//TODO This method is a little bit stupid, isn't it?!?
	//Remark: Not really, because we wanted to extend it - should be revisited...
	public String getGraphName() {
		saveGraph(".tmpGraph", false, false);
		return ".tmpGraph";
	}
	
	public BufferedImage getImage(Double d, boolean color) {
		return nlp.get(getSelectedIndex()).getImage(d, color, true, true, true);
	}
	

	public BufferedImage getImage(double d, boolean color,
			boolean drawHypNames, boolean drawHypWeights, boolean drawEdgeWeights) {
		return nlp.get(getSelectedIndex()).getImage(d, color, drawHypNames, drawHypWeights, drawEdgeWeights);
	}

	public String getLaTeX() {
		saveGraph(tmpGraph, false, false);
		return RControl.getR().eval("graph2latex("+tmpGraph+")").asRChar().getData()[0];
	}
	
	public Vector<Node> getNodes() {
		return nodes;
	}

	public String getRVariableList(Hashtable<String,Double> ht) {
		// For use in replaceVariables <-function(graph, variables=list())
		String list = "list(";
		Enumeration<String> keys = ht.keys();
		for (; keys.hasMoreElements();) {
			String key = keys.nextElement();
			list += "\""+LaTeXTool.UTF2LaTeX(key.charAt(0))+"\"="+ht.get(key)+",";
		}
		list += "\""+"epsilon"+"\"="+Configuration.getInstance().getGeneralConfig().getEpsilon()+",";
		return list.substring(0, list.length()>5?list.length()-1:list.length())+")";			
	}

	public double getZoom() {
		return zoom;
	}

	/**
	 * Is called whenever the graph has changed.
	 */
	public void graphHasChanged() {
		control.setResultUpToDate(false);
		control.isGraphSaved = false;
		if (!updateGUI) return;
		String analysis = null;
		Set<String> variables = getAllVariables();
		variables.remove("ε");
		if (variables.size()==0) {
			try {
				String graphName = ".tmpGraph" + (new Date()).getTime();
				saveGraph(graphName, false, false);				
				if (control.getDView().getSelectedIndex()==1) {
					System.out.println("Performing graph Analysis");
					analysis = RControl.getR().eval("graphAnalysis("+graphName+", file=tempfile())").asRChar().getData()[0];
				}
			} catch (Exception e) {
				// We simply set the analysis to null - that's fine.
			}
		} else {
			analysis = "Graphs with variables are not yet supported for analysis.";
		}
		control.getDView().setAnalysis(analysis);
	}
	
	
	public boolean isTesting() {		
		return testingStarted;
	}

	// This function neither sets the description nor the pvalues.
	// TODO Is this really a good design?
	public GraphMCP loadGraph() {
		control.stopTesting();
		reset();
		boolean updateGUIOld = updateGUI; 
		updateGUI = false;
		graph = new GraphMCP(initialGraph, this);
		control.getPView().restorePValues();
		if (graph.entangledWeights!= null) {
			control.getPView().setEntangledWeights(graph.entangledWeights);
		}
		updateGUI = updateGUIOld;
		graphHasChanged();
		revalidate();
		repaint();
		if (graph.getDescription()!=null) {
			control.getDView().setDescription(graph.getDescription());
		} else {
			control.getDView().setDescription("");
		}
		if (graph.test!=null) {
			control.getPView().setTest(graph.test);
			if (graph.corMatName!=null) {
				control.getPView().setCorrelation(graph.corMatName);
			}
		}
		return graph;
	}

	public void loadGraph(String string, boolean global) {
		if (global) {
			string = "get(\""+string+"\", envir=globalenv())";
		}
		boolean matrix = RControl.getR().eval("is.matrix("+string+")").asRLogical().getData()[0];
		RControl.getR().eval(initialGraph + " <- placeNodes("+ (matrix?"matrix2graph(":"(")+ string + "))");				
		graph = loadGraph();				
		if (graph.pvalues!=null && graph.pvalues.length>1) {
			control.getPView().setPValues(graph.pvalues);
		}
	}

	public void paintGraph(Graphics g) {
		nlp.get(0).paintComponent(g);		
	}

	/**
	 * Repaints the NetzListe and sets the preferredSize etc.
	 */
	public void refresh() {
		calculateSize();
		revalidate();
		repaint();
	}
	
	public void removeEdge(Edge edge) {
		logger.info("Removing "+edge);
		for (Edge e : edges) {
			if (e.from == edge.to && e.to == edge.from && e.layer == edge.layer) {
				e.curve = false;				
			}
		}
		edges.remove(edge);
		getActiveNLP().dragE = new int[0];
		control.getDataFramePanel().setValueAt(new EdgeWeight(0), getNodes().indexOf(edge.from), getNodes().indexOf(edge.to), edge.layer);
		graphHasChanged();
	}
	
	/**
	 * Removes layer from entangled graph. Counting layers starts from 0 (not 1).
	 * @param layer Layer to remove. Counting layers starts from 0 (not 1).
	 */
	public void removeEntangledLayer(int layer) {
		nlp.remove(layer+1);
		remove(layer+1); // +1 since the combined graph is shown at tab 0.
		this.layer--;
		//System.out.println("Number of Layers:" + this.layer);
		if (this.layer==1) {			
			setTitleAt(0, "Graph");
			remove(1);
			annotations.remove(entangledLegend);
			entangledLegend = null;			
		} else {
			entangledLegend.rm(layer+1, true); // +1 for header
			for (int j=0; j<layer; j++) {
				setTitleAt((j+1), "Graph "+(j+1));
			}
		}
		for (int i = edges.size(); i>0; i--) {
			if (edges.get(i-1).layer == layer) {
				edges.remove(i-1);
			}
		}
		for (Edge e : edges) {
			if (e.layer>layer) {
				e.layer--;
				e.color = NetListPanel.layerColors[e.layer%NetListPanel.layerColors.length];
			}
		}
		for (Node n : nodes) {
			n.removeLayer(layer);
		}		
		for (int i=1; i<nlp.size(); i++) {
			nlp.get(i).layer = i-1;
		}
		refresh();
	}	
	
	public void removeNode(Node node) {
		logger.info("Removing "+node);		
		for (int i=edges.size()-1; i>=0; i--) {
			Edge e = edges.get(i);
			if (e.from==node || e.to==node) {
				edges.remove(e);
			}
		}
		control.getDataFramePanel().delRowCol(getNodes().indexOf(node));
		nodes.remove(node);
		control.getPView().removePPanel(node);
		if (nodes.size()==0) {
			control.enableButtons(false);
		}
		repaint();
		graphHasChanged();
	}
	
	/**
	 * Removes all nodes and edges, cleans the description 
	 * and sets back button states, zoom etc. to start-up settings.
	 */
	public void reset() {		
		logger.info("Reset.");
		edges.removeAllElements();
		for (int i=getNodes().size()-1; i>=0; i--) {
			removeNode(getNodes().get(i));
		}
		control.getDataFramePanel().reset();
		while (layer>1) { 
			removeEntangledLayer(layer-1);
			control.getPView().removeEntangledLayer(layer-1);
		}
		statusBar.setText(GraphView.STATUSBAR_DEFAULT);
		for (NetListPanel n : nlp) { n.reset(); }
		zoom = 1.00;		
		control.getDView().setDescription("Enter a description for the graph.");
		graphHasChanged();
		control.isGraphSaved = true;
	}
	
	public void saveGraph(boolean global) {
		saveGraph(initialGraph, false, global);
		control.getPView().savePValues();
	}
	
	public String saveGraph(String graphName, boolean verbose, boolean global, boolean addPValues, boolean addCorrelation) {
		String finalGraphName = saveGraph(graphName, verbose, new Hashtable<String,Double>(), global);
		if (addCorrelation) {
			String test = control.getPView().getTest();
			RControl.getR().evalVoid("attr("+finalGraphName+", \"test\") <- "+ test, global);
			if (test.equals("\"parametric\"")) {
				String correlation = control.getPView().jcbCorObject.getSelectedItem().toString();
				RControl.getR().evalVoid("attr("+finalGraphName+", \"corMatName\") <- '"+ correlation+"'", global);
				RControl.getR().evalVoid("attr("+finalGraphName+", \"corMat\") <- "+ correlation, global);
			}
		}
		if (addPValues) {
			String pvals = control.getPView().getPValuesString();
			RControl.getR().evalVoid("attr("+finalGraphName+", \"pvalues\") <- "+ pvals, global);
		}
		return finalGraphName;
	}
	
	public String saveGraph(String graphName, boolean verbose, boolean global) {
		return saveGraph(graphName, verbose, new Hashtable<String,Double>(), global);
	}

	/**
	 * Exports the current graph to R  
	 * @param graphName variable name in R (will be processed with make.names)
	 * @param verbose if true, a JOption MessageDialog will be shown stating the success
	 * @param ht Hashtable that contains for latin and greek characters (as Strings)
	 * the corresponding Double values. Should not be null, but can be empty.
	 * @param global if true graph is saved to global/specified environment otherwise to gMCP:::env. 
	 * @return name under which graph was saved, i.e. make.names(graphName)
	 */
	public String saveGraph(String graphNameOld, boolean verbose, Hashtable<String,Double> ht, boolean global) {
		if (nodes.size()==0) {
			throw new RuntimeException("Cannot save empty graph.");
		}
		String graphName = RControl.getR().eval("make.names(\""+graphNameOld+"\")").asRChar().getData()[0];
		if (control.getNumberOfLayers()==1) {
			saveSingleLayerGraph(graphName, verbose, ht, 0, global);
		} else {
			String graphs = "";
			String weights = "";
			for (int i=0; i<control.getNumberOfLayers(); i++) {
				saveSingleLayerGraph(tmpGraph+"_layer_"+i, verbose, ht, i, false);
				graphs += tmpGraph+"_layer_"+i;
				weights += control.getPView().entangledWeights.get(i).getText();
				if (i!=control.getNumberOfLayers()-1) {
					graphs += ", ";
					weights += ", ";
				}
			}
			RControl.getR().evalVoid(graphName+" <- new(\"entangledMCP\", subgraphs=list("+graphs+"), weights=c("+weights+"))");
			if (global) RControl.getR().evalVoidInGlobalEnv(graphName+" <- get("+graphName+", envir=gMCP:::gMCPenv)");
		}
		RControl.getR().evalVoid("attr("+graphName+", \"description\") <- \""+ control.getDView().getDescription()+"\"", global);		
		if (verbose && !graphName.equals(graphNameOld)) { JOptionPane.showMessageDialog(this, "The graph as been exported to R under ther variable name:\n\n"+graphName, "Saved as \""+graphName+"\"", JOptionPane.INFORMATION_MESSAGE); }
		return graphName;
	}

	/**
	 * Will open dialog to enter values for all variables and then save the graph with these values.
	 * @param graphName Name of the graph
	 * @param verbose if true, a JOption MessageDialog will be shown stating the success
	 * @param global if true graph is saved to global/specified environment otherwise to gMCP:::env. 
	 * @return name under which graph was saved, i.e. make.names(graphName)
	 */
	public String saveGraphWithoutVariables(String graphName, boolean verbose, boolean global) {
		Set<String> variables = getAllVariables();
		if (!Configuration.getInstance().getGeneralConfig().useEpsApprox())	{
			variables.remove("ε");
		}
		Hashtable<String,Double> ht = new Hashtable<String,Double>();
		if (!variables.isEmpty() && !(variables.size()==1 && variables.contains("ε"))) {
			VariableDialog vd = new VariableDialog(this.control.parent, variables);
			ht = vd.getHT();
		} else if (variables.size()==1 && variables.contains("ε")){
			ht.put("ε", Configuration.getInstance().getGeneralConfig().getEpsilon());
		}		
		graphName = RControl.getR().eval("make.names(\""+graphName+"\")").asRChar().getData()[0];
		saveGraph(graphName, verbose, null, global);
		RControl.getR().eval(graphName+"<- gMCP:::replaceVariables("+graphName+", variables="+getRVariableList(ht)+", ask=FALSE)");
		//TODO This is really a strange place to load a graph... :
		loadGraph(graphName, false);
		return saveGraph(graphName, verbose, ht, global);
	}

	private void saveSingleLayerGraph(String graphName, boolean verbose, Hashtable<String, Double> ht, int layer, boolean global) {
		String alpha = "";
		String nodeStr = "";
		String x = "";
		String y = "";
		for (Node n : nodes) {
			//alpha += "\""+n.getWS() +"\",";
			alpha += n.getWeight().get(layer) +",";
			nodeStr += "\""+n.getRName() +"\","; 
			x += n.getX() + ",";
			y += n.getY() + ",";
		}
		alpha = alpha.substring(0, alpha.length()-1);
		nodeStr = nodeStr.substring(0, nodeStr.length()-1);
		x = x.substring(0, x.length()-1);
		y = y.substring(0, y.length()-1);
		
		RControl.getR().evalVoid(".gsrmtVar <- list()");
		RControl.getR().evalVoid(".gsrmtVar$alpha <- c("+alpha+")");
		RControl.getR().evalVoid(".gsrmtVar$hnodes <- c("+nodeStr+")");

		RControl.getR().evalVoid(".gsrmtVar$m <- matrix(0, nrow="+nodes.size()+", ncol="+nodes.size()+")");
		RControl.getR().evalVoid("rownames(.gsrmtVar$m) <- colnames(.gsrmtVar$m) <- .gsrmtVar$hnodes");
		
		for (Edge e : edges) {
			if (e.layer==layer) {
				RControl.getR().evalVoid(".gsrmtVar$m[\""+e.from.getRName() +"\",\""+e.to.getRName() +"\"] <- \""+ e.getPreciseWeightStr().replaceAll("\\\\", "\\\\\\\\") +"\"");
			}
		}

		RControl.getR().evalVoid(".gsrmtVar$m <- gMCP:::parse2numeric(.gsrmtVar$m)");
	
		RControl.getR().evalVoid(graphName+" <- new(\"graphMCP\", m=.gsrmtVar$m, weights=.gsrmtVar$alpha)");
		for (int i=nodes.size()-1; i>=0; i--) {
			Node n = nodes.get(i);
			if (n.isRejected()) {
				RControl.getR().evalVoid("nodeAttr("+graphName+", \""+n.getName()+"\", \"rejected\") <- TRUE");
			}
		}
		RControl.getR().evalVoid(graphName+"@nodeAttr$X <- c("+x+")");
		RControl.getR().evalVoid(graphName+"@nodeAttr$Y <- c("+y+")");
		for (Edge e : edges) {				
			if (e.layer==layer) {
				RControl.getR().evalVoid("edgeAttr("+graphName+", \""+e.from.getRName() +"\", \""+e.to.getRName() +"\", \"labelX\") <- "+(e.k1-Node.getRadius()));
				RControl.getR().evalVoid("edgeAttr("+graphName+", \""+e.from.getRName() +"\", \""+e.to.getRName() +"\", \"labelY\") <- "+(e.k2-Node.getRadius()));
				//logger.debug("Weight is: "+e.getW(ht));
				if (((Double)e.getW(ht)).isNaN()) {
					//TODO Is the following line necessary? Are we still using the edge attribute "variableWeight"?
					RControl.getR().evalVoid("edgeAttr("+graphName+", \""+e.from.getRName() +"\", \""+e.to.getRName() +"\", \"variableWeight\") <- \""+e.getWS().replaceAll("\\\\", "\\\\\\\\")+"\"");
				}
				if (e.getW(ht)==0) {
					RControl.getR().evalVoid(graphName +"@m[\""+e.from.getRName() +"\", \""+e.to.getRName() +"\"] <- 0");
				}			
			}
		}		
		if (global) RControl.getR().evalVoidInGlobalEnv(graphName+" <- get(\""+graphName+"\", env=gMCP:::gMCPenv)");
	}

	public void setEdge(Edge e) {
		Edge old = null;
		for (Edge e2 : edges) {
			if (e2.from == e.from && e2.to == e.to && e2.layer == e.layer) {
				old = e2;
			}
			if (e2.from == e.to && e2.to == e.from && e2.layer == e.layer) {
				e.curve = true;
				e2.curve = true;
			}
		}
		if (old != null) edges.remove(old);
		edges.add(e);
		control.getDataFramePanel().setValueAt(e.getEdgeWeight(), getNodes().indexOf(e.from), getNodes().indexOf(e.to), e.layer);
		graphHasChanged();
	}

	
	public void setEdge(Node from, Node to, Double w, int layer) {	
		setEdge(from, to, new EdgeWeight(w), layer);
	}

	public void setEdge(Node from, Node to, EdgeWeight w, int layer) {
		Integer x = null;
		Integer y = null;
		boolean curve = false;
		for (int i = edges.size()-1; i >= 0; i--) {
			if (edges.get(i).from == from && edges.get(i).to == to && edges.get(i).layer == layer) {
				x = edges.get(i).getK1();
				y = edges.get(i).getK2();
				removeEdge(edges.get(i));				
			}
		}
		for (Edge e : edges) {
			if (e.from == to && e.to == from && e.layer == layer) {
				e.curve = true;
				curve = true;
			}
		}		
		if (!w.toString().equals("0")) {
			if (x!=null) {
				edges.add(new Edge(from, to, w, this, x, y, layer));
			} else {
				edges.add(new Edge(from, to, w, this, curve, layer));
			}
			edges.lastElement().curve = curve;
		}
		control.getDataFramePanel().setValueAt(w, getNodes().indexOf(from), getNodes().indexOf(to), layer);
		graphHasChanged();
	}

	public void setEdge(Node from, Node to, int layer) {
		setEdge(from, to, 1d, layer);		
	}

	public void setEdge(Node firstVertex, Node secondVertex, NetListPanel netListPanel) {
		setEdge(firstVertex, secondVertex, this.askForLayer());		
	}
	
	public void setEdges(Vector<Edge> edges) {
		this.edges = edges;
		graphHasChanged();
	}

	public void setNodes(Vector<Node> nodes) {
		this.nodes = nodes;
		graphHasChanged();
	}

	public void setNewEdge(boolean b) {
		for (NetListPanel n : nlp) {
			n.newEdge = b;
			if (!b) {
				n.arrowHeadPoint = null;
				n.firstVertexSelected = false;
			}
		}
	}
	
	public void edgeWasSet() {
		for (NetListPanel n : nlp) {
			n.newEdge = false;
			n.arrowHeadPoint = null;
			n.firstVertexSelected = false;
		}
	}

	public void setNewVertex(boolean b) {
		for (NetListPanel n : nlp) {
			n.newVertex = b;
		}
	}

	public void setZoom(double p) {
		zoom = p;
	}

	public void startTesting() {		
		testingStarted = true;	
		statusBar.setText("Reject nodes or reset to the initial graph for modifications.");
	}

	public void stopTesting() {
		testingStarted = false;
		statusBar.setText(GraphView.STATUSBAR_DEFAULT);
	}

	public int whichNode(String name) {
		for (int i=0; i<nodes.size(); i++) {
			if (nodes.get(i).getName().equals(name)) {
				return i;
			}
		}
		return -1;
	}

	public void stateChanged(ChangeEvent e) {
		int i = getSelectedIndex();
		if (i!=0) {
			control.getDataFramePanel().setSelectedIndex(i-1);
		}		
	}
	
	public Edge getEdge (int i, int j, int layer) {
		if ( i < 0 || j < 0 || i >= nodes.size() || j >= nodes.size() ) return null;
		for (Edge e : getEdges()) {
			if (e.from == nodes.get(i) && e.to == nodes.get(j) && e.layer == layer) return e;
		}
		return null;
	}

	int oldi = -1, oldj = -1;
	Integer oldLinewidth = null;
	
	/**
	 * Only one edge can be highlighted at a time.
	 * Highlighting a new edge drops the highlight for the old egde.
	 * A call like highlightEdge(-1, -1) can be used to disable all highlighting.
	 * @param i Index of node the edge is starting.
	 * @param j Index of node the edge is ending.
	 * @param layer Layer of entangled graph. Counting starts with 0.
	 */
	public void highlightEdge(int i, int j, int layer) {
		Edge e = getEdge(oldi, oldj, layer);
		if (e!=null) {
			e.linewidth = oldLinewidth;
		}
		e = getEdge(i, j, layer);		
		if (e==null) {
			oldi = -1;
			oldj = -1;
			return;
		}
		oldi = i; oldj = j;
		oldLinewidth = e.linewidth;
		e.linewidth = 3;		
		repaint();
	}

	public void setEntangledLegendWeight(int i, String weight) {
		if (entangledLegend==null) return;
		try {
			entangledLegend.setText(i+1, "Component Graph "+(i+1)+": "+ weight);
		} catch(ArrayIndexOutOfBoundsException e) {
			//TODO this okay if we remove a layer, but should be handled differently in general. 
		}
		repaint();
	}

	public int getLayer() {
		return layer;
	}

	public void setRadius(int r) {
		Node.setRadius(r);
		for (Node n : nodes) {
			n.reCenter();
		}
	}

}
