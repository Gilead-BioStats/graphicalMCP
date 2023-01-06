package org.af.gMCP.gui.power;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.af.commons.widgets.validate.ValidationException;
import org.af.gMCP.gui.graph.LaTeXTool;
import org.af.gMCP.gui.graph.Node;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class RandomizationPanel extends JPanel implements ActionListener {
	List<Arm> armL = new Vector<Arm>();
	JButton addArm = new JButton("Add arm");
	JButton rmArm = new JButton("Remove last arm");
	
	JButton jbEndpoint = new JButton("Endpoint");
	JButton jbPopulation = new JButton("Population");
	
	SampleSizeDialog sd;
	
	JPanel panel = new JPanel();
	
	public RandomizationPanel(SampleSizeDialog sd) {
		this.sd = sd;
		armL.add(new Arm(sd, "Arm "+(armL.size()+1)));
		setUpLayout();
	}
	
	
	// Add θ and standard error of θ.
	// Allocation ratio?
	
	public void setUpLayout() {

		CellConstraints cc = new CellConstraints();

		int row = 2;

		String cols = "5dlu, fill:min:grow, pref, 5dlu, pref, 5dlu";
		String rows = "5dlu, fill:min:grow, 5dlu, pref, 5dlu, pref, 5dlu";
		
		setLayout(new FormLayout(cols, rows));

		add(new JScrollPane(getMainPanel()), cc.xyw(2, row, 4));
		
		row += 2;
		
		add(addArm, cc.xy(3, row));
		add(rmArm, cc.xy(5, row));
		addArm.addActionListener(this);
		rmArm.addActionListener(this);
		rmArm.setEnabled(false);
		
		row += 2;
		
		add(jbEndpoint, cc.xy(3, row));
		add(jbPopulation, cc.xy(5, row));
		jbEndpoint.addActionListener(this);
		jbPopulation.addActionListener(this);
		jbEndpoint.setEnabled(false);
		jbPopulation.setEnabled(false);
		
	}

	public JPanel getMainPanel() {
		panel.removeAll();

		CellConstraints cc = new CellConstraints();

		int row = 2;

		String cols = "5dlu, pref, 5dlu, pref, 5dlu";
		String rows = "5dlu, pref, 5dlu";
		for (Node n : sd.getNodes()) {
			cols += ", pref, 5dlu";
		}
		for (Arm g : armL) {
			rows += ", pref, 5dlu";
		}

		panel.setLayout(new FormLayout(cols, rows));

		int col = 2;
		panel.add(new JLabel("Arm"), cc.xy(col, row));

		col += 2;
		panel.add(new JLabel("Ratio to first Arm "), cc.xy(col, row));
		
		for (Node n : sd.nodes) {
			col += 2;
			panel.add(new JLabel(LaTeXTool.LaTeX2UTF(n.getName())), cc.xy(col, row));
		}

		for (Arm g : armL) {
			row += 2;
			g.addComponents(panel, cc, row);
		}
		return panel;
	}
	
	//ESF = Effect size factor (i.e. either sqrt(r_i) or sqrt((r_i*r_j)/(r_i+r_j)))
	public String getESF() throws ValidationException { 		
		Vector<Node> nodes = sd.nodes;
		//Vector<Vector<Integer>> armsInvolved = new Vector<Vector<Integer>>();		
		
		double sum = 0;
		for (Arm g : armL) {
			sum += g.getRatio();
		}
		List<Double> r = new Vector<Double>();
		for (int j=0; j < armL.size(); j++) {
			r.add(armL.get(j).getRatio()/sum); // Randomization proportion
		}
		
		String esf = "c(";
		
		for (int i=0; i < nodes.size(); i++) {
			
			Vector<Integer> armsInvolved = new Vector<Integer>();
			for (int j=0; j < armL.size(); j++) {
				if (armL.get(j).isSelected(i)) {
					armsInvolved.add(j);
				}				
			}
			if (armsInvolved.size()==0) {
				throw new ValidationException("No arm involved in hypothesis "+nodes.get(i).getName()+".");
			} else if (armsInvolved.size()==1) {
				Double ri = r.get(armsInvolved.get(0));
				esf += Math.sqrt(ri) + ", ";
			} else if (armsInvolved.size()==2) {
				Double ri = r.get(armsInvolved.get(0));
				Double rj = r.get(armsInvolved.get(1));
				esf += Math.sqrt((ri*rj)/(ri+rj)) + ", ";
			} else if (armsInvolved.size()>2) {
				throw new ValidationException("Too many arms involvedin hypothesis "+nodes.get(i).getName()+".");
			}
			
		}		
		
		return esf.substring(0, esf.length()-2)+")";
	}
	

	public String getRatio() throws ValidationException {
		String ratio = "c(";
		for (Arm g : armL) {
			ratio += g.getRatio()+", ";
		}
		return ratio.substring(0, ratio.length()-2)+")";
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==addArm) {
			armL.add(new Arm(sd, "Arm "+(armL.size()+1)));
			getMainPanel();
			revalidate();
			repaint();
			rmArm.setEnabled(true);
		} else if (e.getSource()==rmArm) {
			if (armL.size()>1) {
				armL.remove(armL.size()-1);
				getMainPanel();
				revalidate();
				repaint();
			}
			if (armL.size()==1) {
				rmArm.setEnabled(false);
			}
		} else if (e.getSource()==jbEndpoint) {
			new EndpointDialog(sd);
		} else if (e.getSource()==jbPopulation) {
			new PopulationDialog(sd);
		}
	}

	public Element getConfigNode(Document document) {
		Element e = document.createElement("randomization");
		e.setAttribute("numberArms", ""+armL.size());
		e.setAttribute("numberHS", ""+armL.get(0).includeL.size());
		for (Arm a : armL) {
			e.appendChild(a.getConfigNode(document));
		}
      	return e;
	}
	
	public void loadConfig(Element e) {
		int nArms = Integer.parseInt(e.getAttribute("numberArms"));
		int nHS = Integer.parseInt(e.getAttribute("numberHS"));
		while(armL.size()<nArms) {
			armL.add(new Arm(sd, "Arm "+(armL.size()+1)));
			rmArm.setEnabled(true);
		}
		NodeList nlist = e.getChildNodes();
		for (int i=0; i<armL.size(); i++) {			
			armL.get(i).loadConfig((Element)nlist.item(i));
		}
		getMainPanel();
		revalidate();
		repaint();
	}
	
}
