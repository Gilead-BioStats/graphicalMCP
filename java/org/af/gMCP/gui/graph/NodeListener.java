package org.af.gMCP.gui.graph;

/**
 * NodeListener are notified when a node is updated or rejected.
 * So far only class PPanel implements this interface.
 */
public interface NodeListener {
	
	public void updated(Node node);
	
	public void reject();

}
