package org.af.gMCP.gui.power;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.datatable.CellEditorE;
import org.af.gMCP.gui.datatable.RDataFrameRef;
import org.af.gMCP.gui.datatable.SingleDataFramePanel;
import org.af.gMCP.gui.dialogs.MatrixCreationDialog;
import org.af.gMCP.gui.dialogs.VariableNameDialog;
import org.af.gMCP.gui.graph.EdgeWeight;
import org.af.gMCP.gui.graph.Node;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class CVPanel extends JPanel implements ActionListener {
	
	PDialog pd;
	CreateGraphGUI parent;
	Vector<Node> nodes;
    
	SingleDataFramePanel dfp;
    SingleDataFramePanel dfpTest;
    boolean containsNA;
    
    JCheckBox secondCV = new JCheckBox("Use another correlation matrix of test statistics used by the parametric test (misspecified or contains NA values)");
    JButton loadCV = new JButton("Load Matrix from R");
    JButton createCV = new JButton("Advanced Matrix Creation");
   
	public CVPanel(PDialog pd) {
		this.pd = pd;
		parent = pd.getParent();
		
		nodes = parent.getGraphView().getNL().getNodes();
		
		RDataFrameRef df = new RDataFrameRef();
		RDataFrameRef df2 = new RDataFrameRef();
		for (Node n: nodes) {
			df.addRowCol(n.getName());
			df2.addRowCol(n.getName());
			df.setValue(df.getColumnCount()-1, df.getColumnCount()-1, new EdgeWeight(1));
			df2.setValue(df2.getColumnCount()-1, df2.getColumnCount()-1, new EdgeWeight(1));
		}		

		dfp = new SingleDataFramePanel(df);
		dfp.getTable().getModel().checkCorMat();
		dfp.getTable().setDefaultEditor(EdgeWeight.class, new CellEditorE(null, dfp.getTable()));
		dfp.getTable().getModel().setCheckRowSum(false);
		
		dfpTest = new SingleDataFramePanel(df2);
		dfpTest.getTable().getModel().checkCorMat();
		dfpTest.getTable().setDefaultEditor(EdgeWeight.class, new CellEditorE(null, dfpTest.getTable()));
		dfpTest.getTable().getModel().setCheckRowSum(false);
		dfpTest.setEnabled(false);		
		
		if (parent.getPView().jrbRCorrelation.isSelected()) {
			try {
			String mat = parent.getPView().jcbCorObject.getSelectedItem().toString();
			load(dfp, mat);
			load(dfpTest, mat);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		if (parent.getPView().jrbRCorrelation.isSelected()) {			
			String name = parent.getPView().jcbCorObject.getSelectedItem().toString();
			containsNA = RControl.getR().eval("any(is.na("+name+"))").asRLogical().getData()[0];
			try {
				double[] result = RControl.getR().eval("as.numeric("+name+")").asRNumeric().getData();
				int n = nodes.size();
				for (int i=0; i<n; i++) {
					for (int j=0; j<n; j++) {
						dfp.getTable().getModel().setValueAt(new EdgeWeight(result[i*n+j]), i, j);
						dfpTest.getTable().getModel().setValueAt(new EdgeWeight(result[i*n+j]), i, j);
					}
				}
			} catch (Exception exc) {
				JOptionPane.showMessageDialog((JDialog)pd, 
						"Could not load matrix \""+name+"\":\n"+exc.getMessage(), "Could not load matrix", JOptionPane.ERROR_MESSAGE);
			}
		}
		
		// Layout:
		
		CellConstraints cc = new CellConstraints();

		int row = 2;

		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
		String rows = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
		if (parent.getPView().jrbRCorrelation.isSelected()) {
			rows += ", pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
		}

		setLayout(new FormLayout(cols, rows));
		
		if (parent.getPView().jrbRCorrelation.isSelected()) {
			
			if (containsNA) {
				add(new JLabel("Correlation matrix used by parametric test (can be changed in the main window)"), cc.xyw(2, row, 3));
			} else {
				add(new JLabel("Correlation matrix used by parametric test and simulation (has to be changed in the main window)"), cc.xyw(2, row, 3));
			}

			row +=2;
			
			add(new JScrollPane(dfpTest), cc.xyw(2, row, 3));
			
			row +=2;
			
			/*
			if (!containsNA) {
				
				add(secondCV, cc.xyw(2, row, 3));
				secondCV.addActionListener(this);

				row +=2;
			}*/
			
		}
		
		if (!parent.getPView().jrbRCorrelation.isSelected() || containsNA) {

			if (containsNA) {
				add(new JLabel("For the simulation setting please sepcify values for the NA entries"), cc.xyw(2, row, 3));
			} else {
				add(new JLabel("Correlation matrix of test statistics for power simulations"), cc.xyw(2, row, 3));
			}

			row +=2;

			add(new JScrollPane(dfp), cc.xyw(2, row, 3));

			row +=2;

			add(loadCV, cc.xy(2, row));
			loadCV.addActionListener(this);

			add(createCV, cc.xy(4, row));
			createCV.addActionListener(this);
		}
	}	
	

	private void load(SingleDataFramePanel dfp) {
		VariableNameDialog vnd = new VariableNameDialog(parent);
		try {
			if (!RControl.getR().eval("gMCP:::checkQuadraticMatrix("+vnd.getName()+", n="+nodes.size()+")").asRLogical().getData()[0]) {
				JOptionPane.showMessageDialog((JDialog)pd, 
						"Can not get a numeric quadradtic matric from \""+vnd.getName()+"\" of dimension "+nodes.size()+"x"+nodes.size()+".", "Not a numeric quadratic matrix of correct dimension", JOptionPane.ERROR_MESSAGE);
				return;
			}		
			load(dfp, vnd.getName());
		} catch(Exception e) {
			JOptionPane.showMessageDialog(parent, "An error occured loading the matrix (please check especially the variable name):\n"+e.getMessage(), "Error loading matrix", JOptionPane.ERROR_MESSAGE);
		}
	}

	private void load(SingleDataFramePanel dfp3, String name) {
		try {
			String force = "";
			if (!RControl.getR().eval("gMCP:::checkCorrelation("+name+")").asRLogical().getData()[0]) {
				int answer = JOptionPane.showConfirmDialog((JDialog)pd, 
						"Matrix is not a correlation matrix. Should it be forced to be symmetric and normalized?", "Not a correlation matrix", JOptionPane.YES_NO_OPTION);
				if (answer == JOptionPane.NO_OPTION) return;
				force = "gMCP:::forceCorrelation";
			}
			double[] result = RControl.getR().eval("as.numeric("+force+"("+name+"))").asRNumeric().getData();
			int n = nodes.size();
			for (int i=0; i<n; i++) {
				for (int j=0; j<n; j++) {
					dfp.getTable().getModel().setValueAt(new EdgeWeight(result[i*n+j]), i, j);
				}
			}
		} catch (Exception exc) {
			JOptionPane.showMessageDialog((JDialog)pd, 
					"Could not load matrix \""+name+"\":\n"+exc.getMessage(), "Could not load matrix", JOptionPane.ERROR_MESSAGE);
		}
	}
	
	String getMatrixForParametricTest() {
		if (parent.getPView().jrbRCorrelation.isSelected()) {			
			SingleDataFramePanel df = dfpTest;
			return ", corr.test="+df.getTable().getModel().getDataFrame().getRMatrix();			
		}
		return "";
	}
	
	public void actionPerformed(ActionEvent e) {
		
		if (e.getSource() == secondCV) {
			// TODO
			return;
		}
		if (e.getSource() == createCV) {			
			MatrixCreationDialog mcd = new MatrixCreationDialog(parent, null, dfp.getTable().getRMatrix(), MatrixCreationDialog.getNames(parent.getGraphView().getNL().getNodes()));
			dfp.getTable().getModel().copy(mcd.dfp.getTable().getModel()); 
			return;
		}

		if (e.getSource() == loadCV) {
			load(dfp);
			return;
		}
	}


	public String getSigma() {
		if (parent.getPView().jrbRCorrelation.isSelected() && !containsNA) {
			return dfpTest.getTable().getModel().getDataFrame().getRMatrix();
		} else {			
			return dfp.getTable().getModel().getDataFrame().getRMatrix();
		}
	}
	
	public Element getConfigNode(Document document) {
		Element e = document.createElement("scenarios");
		e.setAttribute("secondCV", ""+secondCV.isSelected());
		int n = nodes.size();
		Element e1 = document.createElement("cv1."+n);
		Element e2 = document.createElement("cv2."+n);		
		for (int i=0; i<n; i++) {
			Element eRow1 = document.createElement("Row"+(i+1));
			Element eRow2 = document.createElement("Row"+(i+1));				
			for (int j=0; j<n; j++) {
				Element eCol1 = document.createElement("Col"+(j+1));
				Element eCol2 = document.createElement("Col"+(j+1));
				eCol1.setAttribute("value", dfp.getTable().getModel().getValueAt(i,j).toString());
				eCol2.setAttribute("value", dfpTest.getTable().getModel().getValueAt(i,j).toString());
				eRow1.appendChild(eCol1);
				eRow2.appendChild(eCol2);				
			}
			e1.appendChild(eRow1);
			e2.appendChild(eRow2);
		}
		e.appendChild(e1);
		e.appendChild(e2);
      	return e;
	}
	
	public void loadConfig(Element e) {
		secondCV.setSelected(Boolean.parseBoolean(e.getAttribute("secondCV")));
		int n = nodes.size();
		Element cv1 = (Element)e.getElementsByTagName("cv1."+n).item(0);
		Element cv2 = (Element)e.getElementsByTagName("cv2."+n).item(0);
		//ToDo Save all other nodes?
		for (int i=0; i<n; i++) {
			for (int j=0; j<n; j++) {
				if (cv1!=null) dfp.getTable().getModel().setValueAt(new EdgeWeight(Double.parseDouble(((Element)(cv1.getChildNodes().item(i).getChildNodes().item(j))).getAttribute("value"))), i, j);
				if (cv2!=null) dfpTest.getTable().getModel().setValueAt(new EdgeWeight(Double.parseDouble(((Element)(cv2.getChildNodes().item(i).getChildNodes().item(j))).getAttribute("value"))), i, j);
			}
		}		
		repaint();
	}	
	
}
