package org.af.gMCP.gui.power;

import java.util.List;

public interface NCPRequestor {
	public void setNCP(List<Double> ncps);
	public List<Double> getOldNCP();
}
