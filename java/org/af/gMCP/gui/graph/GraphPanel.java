package org.af.gMCP.gui.graph;

import java.awt.Graphics;

import javax.swing.JPanel;

public class GraphPanel extends JPanel {
	
	NetList nl;
	int layer;
	
	GraphPanel(NetList nl, int layer) {
		this.nl = nl;
		this.layer = layer;
	}
	
	/**
	 * We use paintComponent() instead of paint(), since the later one
	 * is not called by a revalidate of the scrollbars.
	 */
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		
	}
	
	/**
	 * Repaints the NetzListe and sets the preferredSize etc.
	 */
	public void refresh() {
		this.setSize(nl.getSize());
		revalidate();
		repaint();
	}
	
	
}
