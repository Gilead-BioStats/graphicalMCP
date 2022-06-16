package org.af.gMCP.gui.dialogs;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;

import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.RControl;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class VariableDialog extends JDialog implements ActionListener {
	JButton ok = new JButton("Ok");

	Hashtable<String,Double> ht = new Hashtable<String,Double>(); 

    CreateGraphGUI parent;
    Object[] variables;
    List<JTextField> jtl;
    
	public VariableDialog(CreateGraphGUI parent, Set<String> v) {
		super(parent, "Variables", true);
		setLocationRelativeTo(parent);
		this.parent = parent;
		variables = v.toArray();
				
        String cols = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu";
        
        for (Object s : variables) {
        	rows += ", pref, 5dlu";
        }
        
        FormLayout layout = new FormLayout(cols, rows);
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 2;
        
        jtl = new Vector<JTextField>();
        
        for (Object s : variables) {        	
        	JTextField jt = new JTextField("0");
        	if (s.equals("ε")) {
        		jt.setText(""+Configuration.getInstance().getGeneralConfig().getEpsilon());
        	} else {
        		jt.setText(""+Configuration.getInstance().getGeneralConfig().getVariable(s.toString()));
        	}
        	getContentPane().add(new JLabel("Value for '"+s+"':"), cc.xy(2, row));
        	getContentPane().add(jt, cc.xy(4, row));
        	jtl.add(jt);        	
        	
        	row += 2;
        }
                
        getContentPane().add(ok, cc.xy(4, row));
        ok.addActionListener(this);        
        
        actionPerformed(null);
        
        pack();
        setVisible(true);
	}

	public void actionPerformed(ActionEvent e) {
		//Fill HashTable:
		for (int i=0; i<variables.length; i++) {
			double value = RControl.getR().eval(jtl.get(i).getText()).asRNumeric().getData()[0];
			if (value==Double.NaN) {
				// TODO Change Hashtable<String,Double> ht to Hashtable<String,Object> ht ?
				// Also variables entered as "\epsilon" should be "\\\\epsilon" in R, i.e. "\\\\\\\\epsilon" in Java call.
			}
			ht.put(variables[i].toString(), value);
			Configuration.getInstance().getGeneralConfig().setVariable(variables[i].toString(), value);
		}
		dispose();
	}	
	
	public Hashtable<String,Double> getHT() {
		return ht;
	}
}