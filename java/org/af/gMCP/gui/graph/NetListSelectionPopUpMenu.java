package org.af.gMCP.gui.graph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class NetListSelectionPopUpMenu extends JPopupMenu implements ActionListener {
	JMenuItem anItem;
	NetList nl;
	List<Node> nodes;
	List<Edge> edges;
	
	public NetListSelectionPopUpMenu(NetList nl, List<Node> nodes, List<Edge> edges){
		this.nl = nl;
		this.nodes = nodes;
		this.edges = edges;
		add(makeMenuItem("Move", "move", !nl.isTesting()));
		add(makeMenuItem("Create subfamily", "subfamily", !nl.isTesting()));
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

    public void actionPerformed(ActionEvent e) {
    	if (e.getActionCommand().equals("move")) {
    		nl.getActiveNLP().dragN = new int[nodes.size()];
    		nl.getActiveNLP().dragE = new int[edges.size()];
    		for (int i=0; i<nodes.size(); i++) {
    			nl.getActiveNLP().dragN[i] = nl.getNodes().indexOf(nodes.get(i));
    		}
    		for (int i=0; i<edges.size(); i++) {
    			nl.getActiveNLP().dragE[i] = nl.getEdges().indexOf(edges.get(i));
    		}
    	} else if (e.getActionCommand().equals("subfamily")) {       	
    		
    	}    	
    	nl.getActiveNLP().repaint();
    }
}
