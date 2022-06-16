package org.af.gMCP.gui.power;

import java.io.File;
import java.util.Date;

import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class SettingsToXML {
	
	private static final Log logger = LogFactory.getLog(SettingsToXML.class);
	
	public static void loadConfigFromXML (File file, PDialog gui) {
		Document document;
		try {
			document = XMLIO.readXML(file);
			Element root = (Element) document.getChildNodes().item(0);		
			String title = root.getAttribute("title");
			
			//gui.generalPanel.projectName.setText(title);			
			gui.loadConfig(root);
			
	    } catch (Exception e) {
	    	JOptionPane.showMessageDialog(null, "Error Loading XML:\n"+e.getMessage(), "Error loading XML", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
	    }	
	}
	
	public static void saveSettingsToXML (File filename, PDialog gui) {
		try {
			XMLIO.saveXML(getDocument(gui), filename);
		} catch (Exception e) {
			JOptionPane.showMessageDialog(null, "Error Saving XML:\n"+e.getMessage(), "Error saving XML", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
		}
	}
	
	public static Document getDocument(PDialog gui) {
		Document document = null;
		Element root;
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        try {
          DocumentBuilder builder = factory.newDocumentBuilder(); 
          document = builder.newDocument();
          root = document.createElement("Settings");
          root.setAttribute("date", (new Date()).toString());
          
          document.appendChild(root);
          for (Element node : gui.getConfigurationNodes(document)) {
        	  root.appendChild(node);
          }
        } catch (ParserConfigurationException e) {
        	JOptionPane.showMessageDialog(null, "Error creating XML:\n"+e.getMessage(), "Error creating XML", JOptionPane.ERROR_MESSAGE);
        }
        
		return document;
	}

}
