package org.af.gMCP.gui.power;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.af.gMCP.gui.graph.LaTeXTool;
import org.af.gMCP.gui.graph.Node;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class PowerReqPanel extends JPanel implements ActionListener {
	List<PowerReq> gv = new Vector<PowerReq>();
	JButton addPowerReq = new JButton("Add power requirement");
	JButton rmPowerReq = new JButton("Remove last power requirement");
	
	SampleSizeDialog sd;

	JPanel panel = new JPanel();
	
	public PowerReqPanel(SampleSizeDialog sd) {
		this.sd = sd;
		gv.add(new PowerReq(sd, "Power requirement "+(gv.size()+1)));
		setUpLayout();
	}
	
	
	// Add θ and standard error of θ.
	// Allocation ratio?
	
	public void setUpLayout() {

		CellConstraints cc = new CellConstraints();

		int row = 2;

		String cols = "5dlu, fill:min:grow, pref, 5dlu, pref, 5dlu";
		String rows = "5dlu, fill:min:grow, 5dlu, pref, 5dlu";
		
		setLayout(new FormLayout(cols, rows));

		add(new JScrollPane(getMainPanel()), cc.xyw(2, row, 4));
		
		row += 2;
		
		add(addPowerReq, cc.xy(3, row));
		add(rmPowerReq, cc.xy(5, row));
		addPowerReq.addActionListener(this);
		rmPowerReq.addActionListener(this);
		rmPowerReq.setEnabled(false);		
	}

	public JPanel getMainPanel() {
		panel.removeAll();

		CellConstraints cc = new CellConstraints();

		int row = 2;

		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
		String rows = "5dlu, pref, 5dlu";
		for (Node n : sd.getNodes()) {
			cols += ", pref, 5dlu";
		}
		for (PowerReq g : gv) {
			rows += ", pref, 5dlu";
		}

		panel.setLayout(new FormLayout(cols, rows));

		int col = 2;
		panel.add(new JLabel("Name"), cc.xy(col, row));
		col += 2;
		panel.add(new JLabel("Power requirement"), cc.xy(col, row));
		col += 2;
		panel.add(new JLabel("Target power / value"), cc.xy(col, row));

		for (Node n : sd.nodes) {
			col += 2;
			panel.add(new JLabel(LaTeXTool.LaTeX2UTF(n.getName())+"    "), cc.xy(col, row));
		}

		for (PowerReq g : gv) {
			row += 2;
			g.addComponents(panel, cc, row);
		}
		return panel;
	}

	public String getPowerTargets() {
		String s = "c(";
		for (PowerReq g : gv) {
			s += g.targetPower.getText()+", ";
		}
		return s.substring(0, s.length()-2)+")";
	}
	

	public String getPowerFunctions() {
		String s = "list(";
		for (PowerReq g : gv) {
			s += g.getPowerfunction() + ", ";
		}		
		s = s.substring(0, s.length()-2)+")";
		return s;
	}	

	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==addPowerReq) {
			PowerReq pr = new PowerReq(sd, "Power requirement "+(gv.size()+1));
			gv.add(pr);
			pr.setActionListener();
			getMainPanel();
			revalidate();
			repaint();
			rmPowerReq.setEnabled(true);
		} else if (e.getSource()==rmPowerReq) {
			if (gv.size()>1) {
				gv.remove(gv.size()-1);
				getMainPanel();
				revalidate();
				repaint();
			}
			if (gv.size()==1) {
				rmPowerReq.setEnabled(false);
			}
		}		
	}



	public Element getConfigNode(Document document) {
		Element e = document.createElement("powerreq");
		e.setAttribute("numberPR", ""+gv.size());
		e.setAttribute("numberHS", ""+gv.get(0).includeL.size());
		for (PowerReq pr : gv) {
			e.appendChild(pr.getConfigNode(document));
		}
      	return e;
	}
	
	public void loadConfig(Element e) {
		int nPR = Integer.parseInt(e.getAttribute("numberPR"));
		int nHS = Integer.parseInt(e.getAttribute("numberHS"));
		while(gv.size()<nPR) {
			gv.add(new PowerReq(sd, "Power requirement "+(gv.size()+1)));
			rmPowerReq.setEnabled(true);
		}
		NodeList nlist = e.getChildNodes();
		for (int i=0; i<gv.size(); i++) {			
			gv.get(i).loadConfig((Element)nlist.item(i));
		}
		getMainPanel();
		revalidate();
		repaint();
	}


	public void setActionListener() {
		for (PowerReq pr : gv) {
			pr.setActionListener();
		}
	}
	
}
