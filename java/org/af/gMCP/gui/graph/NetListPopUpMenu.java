package org.af.gMCP.gui.graph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class NetListPopUpMenu extends JPopupMenu implements ActionListener {

	JMenuItem anItem;
	NetList nl;
	Node node;
	Edge edge;
	
	public NetListPopUpMenu(NetList nl, Node node, Edge edge){
		this.nl = nl;
		this.node = node;
		this.edge = edge;
		add(makeMenuItem("Properties", "properties", !nl.isTesting()));
		add(makeMenuItem("Delete", "delete", !nl.isTesting()));		
		if (node!=null/* && nl.isTesting()*/)	{
			addSeparator();
			add(makeMenuItem("Reject", "reject", nl.isTesting() && node.isRejectable()));
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

    public void actionPerformed(ActionEvent e) {
    	if (e.getActionCommand().equals("properties")) {       	
    		if (node!=null) {
    			new UpdateNode(node, nl.control);
    		}
    		if (edge!=null) {
    			new UpdateEdge(edge, nl, nl.control);
    		}
    	} else if (e.getActionCommand().equals("delete")) {       	
    		if (node!=null) {
    			nl.removeNode(node);
    		}
    		if (edge!=null) {
    			nl.removeEdge(edge);
    		}
    	} else if (e.getActionCommand().equals("reject")) {
    		node.reject();
    	}
    	nl.repaint();
    }
}
