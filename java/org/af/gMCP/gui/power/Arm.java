package org.af.gMCP.gui.power;

import java.util.List;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.af.commons.widgets.validate.RealTextField;
import org.af.commons.widgets.validate.ValidationException;
import org.af.gMCP.gui.graph.Node;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.jgoodies.forms.layout.CellConstraints;

public class Arm {
	List<JCheckBox> includeL = new Vector<JCheckBox>();
	JTextField scname;
	SampleSizeDialog sd;
	RealTextField ratio = new RealTextField("0.0", 10, -Double.MAX_VALUE, Double.MAX_VALUE);
	
	public Arm(SampleSizeDialog sd, String name) {
		this.sd = sd;
		scname = new JTextField(name, 25);
		ratio.setText("1");
		for (Node n : sd.getNodes()) {
			JCheckBox jc = new JCheckBox();
			includeL.add(jc);
		}
	}
	
	public void addComponents(JPanel panel, CellConstraints cc, int row) {
		int col = 2;
		panel.add(scname, cc.xy(col, row));		
		col += 2;
		panel.add(ratio, cc.xy(col, row));
		if (row<5) {
			ratio.setEnabled(false);
		}
		for (JCheckBox jc : includeL) {
			col += 2;
			panel.add(jc, cc.xy(col, row));
		}
		row +=2;
	}

	public Double getRatio() throws ValidationException {
		return ratio.getValidatedValue();
	}

	public boolean isSelected(int i) {		
		return includeL.get(i).isSelected();
	}
	
	public void loadConfig(Element e) {
		scname.setText(e.getAttribute("name"));
		ratio.setText(e.getAttribute("ratio"));
		NodeList nlist = e.getChildNodes();
		for (int i=0; i<Math.min(nlist.getLength(), includeL.size()); i++) {
			includeL.get(i).setSelected(Boolean.parseBoolean(((Element)nlist.item(i)).getAttribute("include")));
		}
	}

	public Element getConfigNode(Document document) {
		Element e = document.createElement("arm");		
		e.setAttribute("name", scname.getText());
		e.setAttribute("ratio", ratio.getText());
		for (JCheckBox jc : includeL) {
			Element eNCP = document.createElement("include");
			eNCP.setAttribute("include", ""+jc.isSelected());
			e.appendChild(eNCP);
		}
		return e;
	} 

}
