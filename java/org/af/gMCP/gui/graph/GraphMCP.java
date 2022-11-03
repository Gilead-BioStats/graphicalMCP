package org.af.gMCP.gui.graph;

import java.util.Vector;

import javax.swing.JOptionPane;

import org.af.gMCP.gui.RControl;
import org.af.jhlir.call.RList;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class GraphMCP {

	private static final Log logger = LogFactory.getLog(GraphMCP.class);
	
	protected String name; /* Not used up to now */
	
	public Vector<Edge> edges = new Vector<Edge>();
	public Vector<Node> nodes = new Vector<Node>();
	NetList nl;
	
	String description;
	String corMatName;	
	String test;
	double[] pvalues = null;
	double[] entangledWeights = null;

	public GraphMCP(String name, NetList nl) {
		this.name = name;
		this.nl = nl;
		loadGraph(name);
		nl.revalidate();
		nl.repaint();
	}
	
	public String getDescription() {
		return description;
	}

	protected void loadGraph(String name) {
		if ( RControl.getR().eval("exists(\""+name+"\")").asRLogical().getData()[0] ) {
			if ( RControl.getR().eval("\"entangledMCP\" %in% class("+name+")").asRLogical().getData()[0] ) {
				String[] nodeArray = RControl.getR().eval("getNodes("+name+")").asRChar().getData();
				double[] x = RControl.getR().eval("getXCoordinates("+name+")").asRNumeric().getData();
				double[] y = RControl.getR().eval("getYCoordinates("+name+")").asRNumeric().getData();
				boolean[] rejected = RControl.getR().eval("getRejected("+name+")").asRLogical().getData();
				entangledWeights = RControl.getR().eval(name+"@weights").asRNumeric().getData();
				//Nodes:
				for (int i=0; i<nodeArray.length; i++) {
					logger.debug("Adding node "+nodeArray[i]+" at ("+x[i]+","+y[i]+").");
					double[] alpha = RControl.getR().eval("getWeights("+name+")[,"+(i+1)+"]").asRNumeric().getData();
					nodes.add(new Node(nodeArray[i], (int) x[i], (int) y[i], alpha, nl));
					if (rejected[i]) nodes.lastElement().rejected = true;
				}
				//Edges:
				for (int layer=0; layer<RControl.getR().eval("gMCP:::layers("+name+")").asRInteger().getData()[0]; layer++) {
					RList edgeL = RControl.getR().eval("gMCP:::getEdges("+name+"@subgraphs[["+(layer+1)+"]])").asRList();
					addEdges(edgeL, layer);
					if (layer!=0) nl.control.addEntangledLayer();
				}
			} else {
				String[] nodeArray = RControl.getR().eval("getNodes("+name+")").asRChar().getData();
				double[] alpha = RControl.getR().eval("getWeights("+name+")").asRNumeric().getData();
				double[] x = RControl.getR().eval("getXCoordinates("+name+")").asRNumeric().getData();
				double[] y = RControl.getR().eval("getYCoordinates("+name+")").asRNumeric().getData();
				boolean[] rejected = RControl.getR().eval("getRejected("+name+")").asRLogical().getData();
				for (int i=0; i<nodeArray.length; i++) {
					logger.debug("Adding node "+nodeArray[i]+" at ("+x[i]+","+y[i]+").");
					nodes.add(new Node(nodeArray[i], (int) x[i], (int) y[i], new double[] {alpha[i]}, nl));
					if (rejected[i]) nodes.lastElement().rejected = true;
				}
				// Edges:
				RList edgeL = RControl.getR().eval("gMCP:::getEdges("+name+")").asRList();
				/*
			    String[] debugEdges = RControl.getR().eval("capture.output(print(gMCP:::getEdges("+name+")))").asRChar().getData();
			    for (String s : debugEdges) {
				   System.out.println(s);
				}
				*/
				addEdges(edgeL, 0);				
			}
		}
		try {
			description = RControl.getR().eval("attr("+name+", \"description\")").asRChar().getData()[0];
		} catch (Exception e) {
			if (!(e instanceof NullPointerException)) {
				System.out.println("No description for graph \""+name+"\".");
				e.printStackTrace();
			}
			description = "Enter a description for the graph.";
		}
		try {
			pvalues = RControl.getR().eval("attr("+name+", \"pvalues\")").asRNumeric().getData();
			int answer = JOptionPane.showConfirmDialog(nl.control.getGraphGUI(), "Graph object has pvalues attached.\n Should they be loaded?", "Load p-values", JOptionPane.YES_NO_OPTION);
			if (answer==JOptionPane.NO_OPTION) pvalues = null;
		} catch (Exception e) {
			if (!(e instanceof NullPointerException)) {
				e.printStackTrace();
			}
			// Nothing to do here.
		}
		try {
			test = RControl.getR().eval("attr("+name+", \"test\")").asRChar().getData()[0];
			int answer = JOptionPane.showConfirmDialog(nl.control.getGraphGUI(), "Graph object has test information attached.\n Should they be loaded?", "Load test information", JOptionPane.YES_NO_OPTION);
			if (answer==JOptionPane.NO_OPTION) {
				test = null;
			} else {
				corMatName = RControl.getR().eval("attr("+name+", \"corMatName\")").asRChar().getData()[0];
				RControl.getR().eval("assign('"+corMatName+"', attr("+name+", \"corMat\"), envir=globalenv())").asRNumeric().getData();
			}
		} catch (Exception e) {
			if (!(e instanceof NullPointerException)) {
				e.printStackTrace();
			}
			// Nothing to do here.
		} 
		for (Node k : nodes) {
			nl.addNode(k);
		}		
		for (Edge e : edges) {
			nl.setEdge(e);
		}
	}

	private void addEdges(RList edgeL, int layer) {
		if (edgeL.get(0)!= null) {
			String[] from = edgeL.get(0).asRChar().getData();
			String[] to = edgeL.get(1).asRChar().getData();
			double[] weight = edgeL.get(2).asRNumeric().getData();
			double[] labelX = edgeL.get(3).asRNumeric().getData();
			double[] labelY = edgeL.get(4).asRNumeric().getData();		
			boolean[] curved = edgeL.get(5).asRLogical().getData();
			String[] weightStr = edgeL.get(6).asRChar().getData();
			for (int i=0; i<from.length; i++) {
				Node fromNode = getNode(from[i]);
				Node toNode = getNode(to[i]);
				int xl = (int) labelX[i];
				//if (xl<-50) xl = (fromNode.getX()+toNode.getX())/2;
				int yl = (int) labelY[i];
				//if (yl<-50) yl = (fromNode.getY()+toNode.getY())/2;				
				boolean curve = curved[i];
				if (!((Double)weight[i]).toString().equals("NaN")) {
					if (xl < -50 || yl < -50) {
						edges.add(new Edge(fromNode, toNode, weight[i], nl,  curve, layer));
					} else {
						edges.add(new Edge(fromNode, toNode, weight[i], nl, xl+Node.getRadius(), yl+Node.getRadius(), layer));
						edges.lastElement().setFixed(true);
					}
				} else {
					if (xl < -50 || yl < -50) {
						edges.add(new Edge(fromNode, toNode, weightStr[i], nl, /* xl+Node.getRadius(), yl+Node.getRadius(),*/ curve, layer));
					} else {
						edges.add(new Edge(fromNode, toNode, weightStr[i], nl, xl+Node.getRadius(), yl+Node.getRadius(), layer));
						edges.lastElement().setFixed(true);
					}
				}

			}
		}		
	}

	private Node getNode(String name) {
		for (Node node : nodes) {
			if (node.getName().equals(name)) {
				return node;
			}
		}
		return null;
	}
}
