package org.af.gMCP.gui.power;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.af.commons.widgets.validate.RealTextField;
import org.af.gMCP.gui.graph.Node;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.jgoodies.forms.layout.CellConstraints;

public class Scenario implements NCPRequestor, ActionListener {
	List<JTextField> ncp = new Vector<JTextField>();
	JTextField scname;
	JButton ncpc = new JButton("Calculate NCP");
	
	PDialog pd;
	
	public Scenario(PDialog pd, String name) {
		this.pd = pd;
		scname = new JTextField(name);
		for (Node n : pd.getNodes()) {
			RealTextField rt = new RealTextField("0.0");			
			rt.setText("1.0");
			rt.setMaximumSize(
				    new Dimension(Integer.MAX_VALUE,
				    	    rt.getPreferredSize().height));
			ncp.add(rt);
		}
		ncpc.addActionListener(this);
	}
	
	public void addComponents(JPanel panel, CellConstraints cc, int row) {
		int col = 2;
		panel.add(scname, cc.xy(col, row));
		for (JTextField jt : ncp) {
			col += 2;
			panel.add(jt, cc.xy(col, row));
		}
		//col +=2;
		//panel.add(ncpc, cc.xy(col, row));
		row +=2;
	}
	
	public String getNCPString() {		
		String s = "'"+scname.getText()+"'=c(";
		for (JTextField jt : ncp) {		
			s += jt.getText()+", ";
		}
		return s.substring(0, s.length()-2)+")";
	}

	 public void loadConfig(Element e) {
		scname.setText(e.getAttribute("name"));
		NodeList nlist = e.getChildNodes();
		for (int i=0; i<Math.min(nlist.getLength(), ncp.size()); i++) {
			ncp.get(i).setText(((Element)nlist.item(i)).getAttribute("ncp"));
		}
	 }
	
	public Element getConfigNode(Document document) {
		Element e = document.createElement("scenario");
		e.setAttribute("name", scname.getText());
		for (JTextField jt : ncp) {
			Element eNCP = document.createElement("ncp");
			eNCP.setAttribute("ncp", jt.getText());
			e.appendChild(eNCP);
		}
		return e;
	}

	public void setNCP(List<Double> ncps) {		
		for (int i=0; i<ncps.size(); i++) {
			Double d = ncps.get(i);
			if (d!=null) ncp.get(i).setText(d.toString());
		}
	}

	NCPCalculatorDialog ncpCD = null;
	
	public void actionPerformed(ActionEvent e) {
		if (ncpCD==null) {
			ncpCD = new NCPCalculatorDialog(pd, this);
		} else {
			ncpCD.setNCPS(getOldNCP());
			ncpCD.setVisible(true);
		}
	}

	public List<Double> getOldNCP() {
		Vector<Double> ncps = new Vector<Double>(); 
		for (JTextField n : ncp) {			
			Double d = null;
			try {
				d = Double.parseDouble(n.getText());
			} catch (Exception e) {
				// Nothing to do - really.
			}
			ncps.add(d);
		}
		return ncps;
	}
}
