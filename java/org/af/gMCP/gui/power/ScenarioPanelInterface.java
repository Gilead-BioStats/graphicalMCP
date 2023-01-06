package org.af.gMCP.gui.power;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public interface ScenarioPanelInterface {

	Element getConfigNode(Document document);

	void loadConfig(Element item);

	String getNCPString();
	
	String getEffSizeString();

}
