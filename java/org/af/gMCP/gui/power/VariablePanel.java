package org.af.gMCP.gui.power;

import java.util.List;
import java.util.Set;
import java.util.Vector;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.graph.LaTeXTool;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class VariablePanel extends JPanel {
	
	/** Actually will only contain Strings and is created by Set<String>.toArray(). */
	Object[] variables;
	/** List of JTextFields to enter values for variables. */
	List<JTextField> jtlVar = new Vector<JTextField>();
	
	public VariablePanel(Set<String> v) {				
		variables = v.toArray();
		
        String cols = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu";
        
        for (Object s : variables) {
        	rows += ", pref, 5dlu";
        }
        
        FormLayout layout = new FormLayout(cols, rows);
        setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 2;
        
        jtlVar = new Vector<JTextField>();
        
        for (Object s : variables) {        	
        	JTextField jt = new JTextField("0");
        	if (s.equals("ε")) {
        		jt.setText(""+Configuration.getInstance().getGeneralConfig().getEpsilon());
        	} else {
        		jt.setText(""+Configuration.getInstance().getGeneralConfig().getVariable(s.toString()));
        	}
        	add(new JLabel("Value for '"+s+"':"), cc.xy(2, row));
        	add(jt, cc.xy(4, row));
        	jtlVar.add(jt);        	
        	
        	row += 2;
        }    
	}
	
	public String getVariables() {
		if (jtlVar.size()>0) {
			String s = "list("; 
			for (int i=0; i<variables.length; i++) {
				s = s + LaTeXTool.UTF2LaTeX(variables[i].toString().charAt(0))+" = c("+ jtlVar.get(i).getText()+")";
				if (i!=variables.length-1) s = s + ", ";
			}		
			return s+")";
		} else {
			return "";
		}
	}
	
	Element oldElement = null;
	
	public Element getConfigNode(Document document) {
		Element e = document.createElement("variablePanel");
		if (oldElement!=null) {
			NamedNodeMap attributes = oldElement.getAttributes();
			for (int j = 0; j < attributes.getLength(); j++) {
				e.setAttribute(attributes.item(j).getNodeName(), attributes.item(j).getNodeValue());
			}
		}
		for (int i=0; i<jtlVar.size(); i++) {			
			e.setAttribute(variables[i].toString(), jtlVar.get(i).getText());			
		}
		return e;
	}

	public void loadConfig(Element e) {		
		oldElement = e;
		for (int i=0; i<jtlVar.size(); i++) {			
			String value = e.getAttribute(variables[i].toString());
			if (value=="") value = "0.5";
			jtlVar.get(i).setText(value);
		}
					
	}
	
}
