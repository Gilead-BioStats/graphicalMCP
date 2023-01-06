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

public class ScenarioPanel2 extends JPanel implements ActionListener, ScenarioPanelInterface {
	List<Scenario2> sc = new Vector<Scenario2>();
	JButton addScenario = new JButton("Add scenario");
	JButton rmScenario = new JButton("Remove last scenario");
	
	PDialog pd;
	
	JPanel panel = new JPanel();
	
	public ScenarioPanel2(PDialog pd) {
		this.pd = pd;		
		
		sc.add(new Scenario2(pd, "Scenario "+(sc.size()+1)));
				
		setUpLayout();
	}
	
	public void setUpLayout() {

		CellConstraints cc = new CellConstraints();

		int row = 2;

		String cols = "5dlu, fill:min:grow, pref, 5dlu, pref, 5dlu";
		String rows = "5dlu, fill:min:grow, 5dlu, pref, 5dlu";
		
		setLayout(new FormLayout(cols, rows));

		add(new JScrollPane(getMainPanel()), cc.xyw(2, row, 4));
		
		row += 2;
		
		add(addScenario, cc.xy(3, row));
		add(rmScenario, cc.xy(5, row));
		addScenario.addActionListener(this);
		rmScenario.addActionListener(this);
		rmScenario.setEnabled(false);
		
	}

	public JPanel getMainPanel() {
		panel.removeAll();

		CellConstraints cc = new CellConstraints();

		int row = 2;

		String cols = "5dlu, fill:pref:grow, 5dlu";
		String rows = "5dlu, pref, 5dlu";
		for (Node n : pd.getNodes()) {
			cols += ", fill:pref:grow, 5dlu";
		}
		for (Scenario2 s : sc) {
			rows += ", pref, 5dlu";
		}
		//cols += ", pref, 5dlu";

		panel.setLayout(new FormLayout(cols, rows));

		int col = 2;
		panel.add(new JLabel("Scenario name"), cc.xy(col, row));

		for (Node n : pd.nodes) {
			col += 2;
			panel.add(new JLabel(LaTeXTool.LaTeX2UTF(n.getName())), cc.xy(col, row));
		}

		for (Scenario2 s : sc) {
			row += 2;
			s.addComponents(panel, cc, row);
		}
		return panel;
	}

	public String getNCPString() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getEffSizeString() {
		String sList = "list(";
		for (Scenario2 s : sc) {
			sList += s.getEffSizeString()+", ";
		}
		return sList.substring(0, sList.length()-2)+")";
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==addScenario) {
			sc.add(new Scenario2(pd, "Scenario "+(sc.size()+1)));
			getMainPanel();
			revalidate();
			repaint();
			rmScenario.setEnabled(true);
		} else if (e.getSource()==rmScenario) {
			if (sc.size()>1) {
				sc.remove(sc.size()-1);
				getMainPanel();
				revalidate();
				repaint();
			}
			if (sc.size()==1) {
				rmScenario.setEnabled(false);
			}
		}		
	}

	public Element getConfigNode(Document document) {
		Element e = document.createElement("scenarios");
		e.setAttribute("numberSC", ""+sc.size());
		e.setAttribute("numberHS", ""+sc.get(0).effSizes.size());
		for (Scenario2 s : sc) {
			e.appendChild(s.getConfigNode(document));
		}
      	return e;
	}
	
	public void loadConfig(Element e) {
		int nSC = Integer.parseInt(e.getAttribute("numberSC"));
		int nHS = Integer.parseInt(e.getAttribute("numberHS"));
		while(sc.size()<nSC) {
			sc.add(new Scenario2(pd, "Scenario "+(sc.size()+1)));
			rmScenario.setEnabled(true);
		}
		NodeList nlist = e.getChildNodes();
		for (int i=0; i<sc.size(); i++) {			
			sc.get(i).loadConfig((Element)nlist.item(i));
		}
		getMainPanel();
		revalidate();
		repaint();
	}
	
}
