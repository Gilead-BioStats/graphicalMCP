package org.af.gMCP.gui.graph;

import java.util.List;

public class SubFamily {
	
	List<Edge> edges;
	List<Node> nodes;
	
	/**
	 * Constructor of a subfamily of nodes/hypotheses.
	 * @param name (Short) name of the subfamily
	 * @param description Description of the subfamily. May contain line breaks.
	 * @param nodes List of nodes that form this subfamily
	 * @param edges Edges from or to nodes not contain in the parameter List nodes 
	 *               will be removed automatically.
	 */
	public SubFamily(String name, String description, List<Node> nodes, List<Edge> edges) {
		for (int i=edges.size(); i>0; i--) {
			Edge e = edges.get(i-1);
			if (!(nodes.contains(e.from) && nodes.contains(e.to))) {
				edges.remove(i-1);
			}
		}
		
	}
}
