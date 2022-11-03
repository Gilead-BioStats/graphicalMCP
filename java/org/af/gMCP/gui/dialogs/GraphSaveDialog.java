package org.af.gMCP.gui.dialogs;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;

import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.graph.GraphView;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class GraphSaveDialog extends JDialog implements ActionListener {
	JButton ok = new JButton("Ok");

	GraphView control;
    JTextField jt = new JTextField("", 30);
    JCheckBox savePvalues = new JCheckBox("Save p-values");
    JCheckBox saveCorrelation = new JCheckBox("Save test information / correlation");
    
	public GraphSaveDialog(GraphView control, String name) {
		super(control.getMainFrame(), "R object name", true);
		setLocationRelativeTo(control.getMainFrame());
		this.control = control;		

		String cols = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
		String rows = "5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";

		FormLayout layout = new FormLayout(cols, rows);
		getContentPane().setLayout(layout);
		CellConstraints cc = new CellConstraints();

		int row = 2;

		jt.setText(name);
		jt.addActionListener(this);
		getContentPane().add(new JLabel("R object name:"), cc.xy(2, row));
		getContentPane().add(jt, cc.xyw(4, row, 3));

		row += 2;
		
		getContentPane().add(savePvalues, cc.xyw(2, row, 3));
		savePvalues.setSelected(Configuration.getInstance().getBooleanClassProperty(GraphSaveDialog.class, "savePvalues", false));
		
		row += 2;
		
		getContentPane().add(saveCorrelation, cc.xyw(2, row, 3));
		saveCorrelation.setSelected(Configuration.getInstance().getBooleanClassProperty(GraphSaveDialog.class, "saveCorrelation", false));
		
		row += 2;
				

		getContentPane().add(ok, cc.xy(6, row));
		ok.addActionListener(this);        

		actionPerformed(null);

		pack();
		setVisible(true);
	}

	public GraphSaveDialog(GraphView control) {
		this(control, Configuration.getInstance().getClassProperty(GraphSaveDialog.class, "variableName", ""));
	}

	public void actionPerformed(ActionEvent e) {
		Configuration.getInstance().setClassProperty(GraphSaveDialog.class, "variableName", jt.getText());
		Configuration.getInstance().setBooleanClassProperty(GraphSaveDialog.class, "saveCorrelation", saveCorrelation.isSelected());
		Configuration.getInstance().setBooleanClassProperty(GraphSaveDialog.class, "savePvalues", savePvalues.isSelected());
		dispose();
	}	
	
	//TODO DO we want to use this method really?
	public void saveGraph() {		
    	String name = control.getNL().saveGraph(getName(), true, true, savePvalues.isSelected(), saveCorrelation.isSelected());        	    	
    	Configuration.getInstance().getGeneralConfig().addGraph("R Object: "+name);
    	control.isGraphSaved = true;
	}
	
	public String getName() {
		return jt.getText();
	}

	public boolean attachPValues() {
		return savePvalues.isSelected();
	}

	public boolean attachCorrMat() {
		return saveCorrelation.isSelected();
	}
}