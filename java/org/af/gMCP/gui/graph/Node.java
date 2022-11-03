package org.af.gMCP.gui.graph;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.font.FontRenderContext;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import java.text.DecimalFormat;
import java.util.List;
import java.util.Vector;

import javax.swing.JOptionPane;

import org.af.commons.tools.StringTools;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.RControl;
import org.scilab.forge.jlatexmath.TeXConstants;
import org.scilab.forge.jlatexmath.TeXFormula;
import org.scilab.forge.jlatexmath.TeXIcon;

/**
 * Node of a graph. Knows the NetList it belongs to and 
 * paints itself on a given Graphics object. 
 *
 */
public class Node {
	
	static DecimalFormat format = new DecimalFormat("#.####");
	static DecimalFormat formatSmall = new DecimalFormat("#.###E0");
	private static int r = 25;	
	private Color color = Color.WHITE;
	TeXIcon iconName;
	List<TeXIcon> iconWeightScaledDown;
	List<TeXIcon> iconWeight;
	/** lastFontSize is used to check whether TeXItems have to be reconstructed */
	int lastFontSize = 14;
	/** Normally each node has exactly one PPanel as NodeListener waiting for changes */
	public Vector<NodeListener> listener = new Vector<NodeListener>();
	
	private String name;
	NetList nl;
	
	private boolean rejectable = false;

	boolean rejected = false;

	private List<String> stringW = new Vector<String>();	
	private List<Double> weight = new Vector<Double>();

	int x, y;
	
	int mx, my;
	
	public Node(String name, int x, int y, double[] alpha, NetList vs) {
		this.nl = vs;
		setName(name);
		setX(x);
		setY(y);		
		setWeight(alpha, null);		
	}

	public void addNodeListener(NodeListener l) {
		listener.add(l);		
	}

	public Color getColor() {
		if (rejected) return Color.MAGENTA;
		return color;
	}

	public String getName() { return name; }
	
	public String getRName() { return name.replaceAll("\\\\", "\\\\\\\\"); }

	public List<Double> getWeight() { return weight; }
	
	private List<String> getWS() { return stringW; }

	public int getX() { return x; }

	public int getY() { return y; }

	public boolean inYou(int x, int y) {
		return ((x / nl.getZoom() - this.x - r)
				* (x / nl.getZoom() - this.x - r)
				+ (y / nl.getZoom() - this.y - r)
				* (y / nl.getZoom() - this.y - r) <= (r * r));
	}	

	public boolean containsYou(int[] start, int[] end) {
		return Math.min(start[0], end[0])/ nl.getZoom()<=x+r && Math.max(start[0], end[0])/ nl.getZoom()>=x-r
				&& Math.min(start[1], end[1])/ nl.getZoom()<=y+r && Math.max(start[1], end[1])/ nl.getZoom()>=y-r; 
	}

	//TODO Unify the following functions:
	public static int getRadius() { return r; }	
	static void setRadius(int r) {  Node.r = r; }	

	public boolean isRejectable() {
		return rejectable && !rejected;
	}

	public boolean isRejected() { 	return rejected; }
	
	/**
	 * Given a point (x2,y2) this returns the offset (coordinate wise difference) 
	 * of this node to this point in the current zoom level. 
	 * @return offset (coordinate wise difference) of this node to this point
	 */
	public int[] offset(int x2, int y2) {
		return new int[] {(int) (x* nl.getZoom())-x2, (int) (y* nl.getZoom())-y2};
	}
	
	/**
	 * Draws the node.
	 * @param g Graphs object for drawing
	 * @param layer Layer to draw. Will be 'null' if all layers should be drawn. Starts with 0 (not 1).
	 */
	public void paintYou(Graphics g, Integer layer, boolean color) {
		paintYou(g, layer, color, true, true);
	}
	
	public void paintYou(Graphics g, Integer layer, boolean color,				
				boolean drawHypNames, boolean drawHypWeights) {
			if (rejected && !Configuration.getInstance().getGeneralConfig().showRejected()) return;
		Graphics2D g2d = (Graphics2D) g;
		g2d.setFont(new Font("Arial", Font.PLAIN, (int) (12 * nl.getZoom())));		
		FontRenderContext frc = g2d.getFontRenderContext();		
		Rectangle2D rc;
		Ellipse2D e = new Ellipse2D.Double();
		if(color) {
			g2d.setColor(getColor());		
			// if (this.fix) {	g2d.setColor(new Color(50, 255, 50)); }		
			e.setFrame(x * nl.getZoom(), 
					y * nl.getZoom(), 
					r * 2 * nl.getZoom(), 
					r * 2 * nl.getZoom());
			g2d.fill(e);
		}
		g2d.setColor(new Color(0, 0, 0));
		e.setFrame(x * nl.getZoom(), 
				y * nl.getZoom(), 
				r * 2 * nl.getZoom(), 
				r * 2 * nl.getZoom());
		g2d.draw(e);
		
		if (!Configuration.getInstance().getGeneralConfig().useJLaTeXMath()) {			

			rc = g2d.getFont().getStringBounds(name, frc);
			if (drawHypNames) {
				g2d.drawString(name, 
						(float) ((x + r) * nl.getZoom() - rc.getWidth() / 2), 
						(float) ((y + r - 0.25*r) * nl.getZoom())); // +rc.getHeight()/2));
			}

			//TODO Color for different weights:
			String wStr;
			if (layer==null) {
				wStr = StringTools.collapseStringList(getWS(), " ");
			} else {
				wStr = stringW.get(layer);
			}
			rc = g2d.getFont().getStringBounds(wStr, frc);
			if (drawHypWeights) {
				g2d.drawString(wStr,
						(float) ((x + r) * nl.getZoom() - rc.getWidth() / 2),
						(float) ((y + 1.5 * r) * nl.getZoom()));
			}
		} else {		
			if (lastFontSize != (int) (14 * nl.getZoom())) {
				lastFontSize = (int) (14 * nl.getZoom());
				createWeightIcons();
				TeXFormula formula = new TeXFormula("\\mathbf{"+name+"}");
				iconName = formula.createTeXIcon(TeXConstants.ALIGN_CENTER, lastFontSize);
			}
			if (drawHypNames) {
				iconName.paintIcon(LaTeXTool.panel, g2d,
						(int) ((x + r) * nl.getZoom() - iconName.getIconWidth() / 2), 
						(int) ((y + r - 0.6*r) * nl.getZoom()));
			}

			List<TeXIcon> tmpIconWeight;
			if (layer==null) {
				tmpIconWeight = iconWeightScaledDown;
			} else {
				tmpIconWeight = new Vector<TeXIcon>();	
				tmpIconWeight.add(iconWeight.get(layer));
			}			
			int offset = (int)(-((tmpIconWeight.size()-1)/2.0)*10);
			if (drawHypWeights) {
				for (TeXIcon icon : tmpIconWeight) {				
					//TODO Color and correct x coordinates:
					icon.paintIcon(LaTeXTool.panel, g2d,
							(int) ((x + r + offset) * nl.getZoom() - icon.getIconWidth() / 2), 
							(int) ((y + 1.1 * r) * nl.getZoom()));
					offset+=10;
				}
			}
		}
		
	}

	public void createWeightIcons() {
		lastFontSize = (int) (14 * nl.getZoom());
		int fontSize = (int) (14 * nl.getZoom());
		int fontSizeScaledDown = (int) (14 * nl.getZoom() * (getWS().size()==1?1:0.8));
		iconWeight = new Vector<TeXIcon>();
		iconWeightScaledDown = new Vector<TeXIcon>();
		int layer = 0;
		for (String w : getWS()) {			
			TeXIcon icon = LaTeXTool.getTeXIcon(this.nl.control.getGraphGUI(), w, fontSize);
			icon.setForeground(NetListPanel.layerColors[layer%NetListPanel.layerColors.length]);
			iconWeight.add(icon);
			if (fontSize == fontSizeScaledDown) {
				iconWeightScaledDown = iconWeight;
			} else {
				icon = LaTeXTool.getTeXIcon(this.nl.control.getGraphGUI(), w, fontSizeScaledDown);
				icon.setForeground(NetListPanel.layerColors[layer%NetListPanel.layerColors.length]);
				iconWeightScaledDown.add(icon);	
			}
			layer++;
		}
	}

	/**
	 * This method will save the graph,
	 * call rejectNode in R and show the result.
	 * All nodeListeners (i.e. PPanel) are notified.
	 */
	public void reject() {		
		nl.acceptNode(this);
		for (NodeListener l : listener) {
			l.reject();
		}
	}
	
	public void setColor(Color color) {
		this.color = color;
		nl.repaint();
	}
	
	public void setName(String name) {
		this.name = name;	
		TeXFormula formula;
		try {
			formula = new TeXFormula("\\mathbf{"+LaTeXTool.sanitize(name)+"}");
		} catch(Exception e) {
			JOptionPane.showMessageDialog(nl, "The name for the node could not be parsed in LaTeX:\n"+e.getMessage()+"\n\nDisable JLaTeXMath in the options, if you want to use simple text labels.");
			formula = new TeXFormula("Syntax error");
		}
		iconName = formula.createTeXIcon(TeXConstants.ALIGN_CENTER, (int) (14 * nl.getZoom()));
		nl.graphHasChanged();
	}

	public void setRejectable(boolean rejectable) {
		if (rejectable) {
			setColor(new Color(50, 255, 50));
		} else {
			setColor(Color.WHITE);
		}
		this.rejectable = rejectable;
	}

	public void setWeight(double[] weights, NodeListener me) {
		DecimalFormat format = Configuration.getInstance().getGeneralConfig().getDecFormat();
		weight = new Vector<Double>();
		stringW = new Vector<String>();
		
		for (double w : weights) {
			addSingleLayerWeight(w);
		}
		
		createWeightIcons();
		
		for (NodeListener l : listener) {
			if (me!=l) {
				l.updated(this);
			}
		}
		nl.graphHasChanged();
		nl.repaint();
	}
	
	public void addSingleLayerWeight(double w) {	
		weight.add(w);
		if (!Configuration.getInstance().getGeneralConfig().showFractions()) {
			stringW.add(format.format(w));
		} else {
			if (w!=0 && w < Math.pow(0.1, 3)) {
				stringW.add(formatSmall.format(w));
			} else {
				String wS = RControl.getFraction(w, 5);
				if (wS.length()>7) {
					wS = "\\sim "+format.format(w);
				}
				stringW.add(wS);
			}
		}
	}
	
	public void addLayer() {
		addSingleLayerWeight(0);
		// Force recalculation of TeXItems:
		lastFontSize = 0;
	}
	
	public void setX(int x) {
		int grid = Configuration.getInstance().getGeneralConfig().getGridSize();
		x = ((x+ (int)(0.5*grid)) / grid)*grid;
		this.x = x;
		this.mx = x + r;
	}

	public void setY(int y) {
		int grid = Configuration.getInstance().getGeneralConfig().getGridSize();
		y = ((y+ (int)(0.5*grid)) / grid)*grid;
		this.y = y;
		this.my = y + r;
	}

	public void reCenter() {
		this.x = mx - r;
		this.y = my - r;
	}
	
	public String toString() {		
		return name+" (w: "+getWS()+")";
	}

	/**
	 * Removes layer. Counting layers starts from 0 (not 1).
	 * @param layer
	 */
	public void removeLayer(int layer) {				
		weight.remove(layer);
		stringW.remove(layer);	
		// Force recalculation of TeXItems:
		lastFontSize = 0;		
	}

	//TODO Remove this method and check whether color should always be true.
	public void paintYou(Graphics g, Integer layer) {
		paintYou(g, layer, true);		
	}

}
