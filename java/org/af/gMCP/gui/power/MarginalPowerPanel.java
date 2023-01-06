package org.af.gMCP.gui.power;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.af.gMCP.gui.graph.LaTeXTool;
import org.af.gMCP.gui.graph.Node;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class MarginalPowerPanel extends JPanel {

	PDialog pd;
	
	JPanel panel = new JPanel();
	
	public MarginalPowerPanel(PDialog pd) {
			this.pd = pd;
					
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
			

			
		}
	
		public JPanel getMainPanel() {
			panel.removeAll();
	
			CellConstraints cc = new CellConstraints();
	
			int row = 2;
	
			String cols = "5dlu, pref, 5dlu";
			String rows = "5dlu, pref, 5dlu";
			for (Node n : pd.getNodes()) {
				cols += ", pref, 5dlu";
			}
	
			panel.setLayout(new FormLayout(cols, rows));
	
			int col = 2;
			panel.add(new JLabel("Scenario name"), cc.xy(col, row));
	
			for (Node n : pd.getParent().getGraphView().getNL().getNodes()) {
				col += 2;
				panel.add(new JLabel("NCP "+ LaTeXTool.LaTeX2UTF(n.getName())+"    "), cc.xy(col, row));
			}
	
			return panel;
		}
		
		public Element getConfigNode(Document document) {
			Element e = document.createElement("scenarios");
			/*e.setAttribute("numberSC", ""+sc.size());
			e.setAttribute("numberHS", ""+sc.get(0).ncp.size());
			for (Scenario s : sc) {
				e.appendChild(s.getConfigNode(document));
			}*/
	      	return e;
		}
		
		public void loadConfig(Element e) {
			int nSC = Integer.parseInt(e.getAttribute("numberSC"));
			int nHS = Integer.parseInt(e.getAttribute("numberHS"));
			/*while(sc.size()<nSC) {
				sc.add(new Scenario(pd, "Scenario "+(sc.size()+1)));
				rmScenario.setEnabled(true);
			}
			NodeList nlist = e.getChildNodes();
			for (int i=0; i<sc.size(); i++) {			
				sc.get(i).loadConfig((Element)nlist.item(i));
			}*/
			getMainPanel();
			revalidate();
			repaint();
		}	

}
