package org.af.gMCP.gui.dialogs;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Hashtable;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.af.gMCP.gui.MenuBarMGraph;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class ParameterDialog extends JDialog implements ActionListener, ChangeListener {

	MenuBarMGraph mbar;
	String command;
	JButton ok = new JButton("Ok");
	JSpinner spinnerN;
	JSpinner spinnerNDoses;
	JSpinner spinnerNTimes;
	JTextField jtW;
	JPanel weightsPanel;
	Hashtable<String,Object> parameters;
	List<JTextField> weightsV = new Vector<JTextField>();

	public ParameterDialog(JFrame parent, Hashtable<String,Object> parameters, MenuBarMGraph menuBarMGraph, String command) {
		super(parent, "Number of Hypotheses", true);
		setLocationRelativeTo(parent);
		this.mbar = menuBarMGraph;
		this.command = command;
		this.parameters = parameters;
		
        String cols = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu";
        
        if (parameters.get("n")!=null) {
        	rows += ", pref, 5dlu";
        }
        
        if (parameters.get("weights")!=null) {
        	rows += ", pref:grow, 5dlu";
        }
        
        if (parameters.get("times")!=null) {
        	rows += ", pref, 5dlu";
        }
        
        if (parameters.get("doses")!=null) {
        	rows += ", pref, 5dlu";
        }
        
        if (parameters.get("w")!=null) {
        	rows += ", pref, 5dlu";
        }
        
               
        FormLayout layout = new FormLayout(cols, rows);
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 2;
        
        if (parameters.get("n")!=null) {

        	int[] n = (int[]) parameters.get("n");

        	spinnerN = new JSpinner(new SpinnerNumberModel(n[1], n[0], n[2], 1));    	
        	spinnerN.addChangeListener(this);

        	getContentPane().add(new JLabel("Number of hypotheses:"),     cc.xy(2, row));
        	getContentPane().add(spinnerN, cc.xy(4, row));        

        	row += 2;

        }

        if (parameters.get("weights")!=null) {

        	double[] weights = (double[]) parameters.get("weights");
        	weightsPanel = new JPanel();
        	GridBagConstraints c = new GridBagConstraints();
    		
    		c.fill = GridBagConstraints.BOTH;	
    		c.gridx=0; c.gridy=0;
    		c.gridwidth = 1; c.gridheight = 1;
    		c.ipadx=5; c.ipady=5;
    		c.weightx=1; c.weighty=1;
    		
    		weightsPanel.setLayout(new GridBagLayout());
        	
        	int n = (weights.length==0?Integer.parseInt(spinnerN.getModel().getValue().toString()):weights.length);
        	
        	for (int i=0;i<n;i++) {        		
        		weightsV.add(new JTextField(weights.length!=0?""+weights[i]:"0"));
        		weightsPanel.add(new JLabel("H"+(i+1)), c);
        		c.gridx++;
        		weightsPanel.add(weightsV.get(i), c);
        		c.gridx=0;c.gridy++;
        	}
        	
        	JScrollPane sp = new JScrollPane(weightsPanel);
        	getContentPane().add(sp, cc.xyw(2, row, 3));
        	
        	row += 2;
        }
        
        if (parameters.get("times")!=null) {
        	int[] n = (int[]) parameters.get("times");

        	spinnerNTimes = new JSpinner(new SpinnerNumberModel(n[1], n[0], n[2], 1));    	
        	spinnerNTimes.addChangeListener(this);

        	getContentPane().add(new JLabel("Number of time points:"),     cc.xy(2, row));
        	getContentPane().add(spinnerNTimes, cc.xy(4, row));        

        	row += 2;
        }
        
        if (parameters.get("doses")!=null) {
        	int[] n = (int[]) parameters.get("doses");

        	spinnerNDoses = new JSpinner(new SpinnerNumberModel(n[1], n[0], n[2], 1));    	
        	spinnerNDoses.addChangeListener(this);

        	getContentPane().add(new JLabel("Number of dose levels:"),     cc.xy(2, row));
        	getContentPane().add(spinnerNDoses, cc.xy(4, row));        

        	row += 2;        
        }
        
        if (parameters.get("w")!=null) {
        	Double w = (Double) parameters.get("w");

        	jtW = new JTextField(""+w);

        	getContentPane().add(new JLabel("Weight:"),     cc.xy(2, row));
        	getContentPane().add(jtW, cc.xy(4, row));        

        	row += 2;        
        }
        
        
        getContentPane().add(ok, cc.xy(4, row));
        ok.addActionListener(this);        
        
        pack();
        setVisible(true);
	}

	public void actionPerformed(ActionEvent e) {
		String command = this.command+"(";
		if (parameters.get("n")!=null && parameters.get("weights")==null) {
			command += "n="+spinnerN.getModel().getValue()+",";
		}
		if (parameters.get("weights")!=null) {
			command += "w=c(";
			for (JTextField tf : weightsV) {
				command += tf.getText()+",";
			}
			command = command.substring(0, command.length()-1)+"),";
		}
		if (parameters.get("times")!=null) {
			command += "times="+spinnerNTimes.getModel().getValue()+",";
        }
        
        if (parameters.get("doses")!=null) {
        	command += "doses="+spinnerNDoses.getModel().getValue()+",";
        }        
        if (parameters.get("w")!=null) {
        	command += "w="+jtW.getText()+",";
        }        
		mbar.loadGraph(command.substring(0, command.length()-1)+")", false);
		dispose();
	}

	public void stateChanged(ChangeEvent e) {		
		if (parameters.get("weights")!=null && parameters.get("n")!=null) {
			int n = Integer.parseInt(spinnerN.getModel().getValue().toString());
			
			if (weightsV.size()<n) {
				for(int i=weightsV.size(); i<n; i++)
					weightsV.add(new JTextField("0"));
			}
			if (weightsV.size()>n) {
				for(int i=n; i<weightsV.size(); i++)
					weightsV.remove(i);
			}
			weightsPanel.revalidate();
        	weightsPanel.removeAll();
        	GridBagConstraints c = new GridBagConstraints();
    		
    		c.fill = GridBagConstraints.BOTH;	
    		c.gridx=0; c.gridy=0;
    		c.gridwidth = 1; c.gridheight = 1;
    		c.ipadx=5; c.ipady=5;
    		c.weightx=1; c.weighty=1;
    		
    		weightsPanel.setLayout(new GridBagLayout());
        	
        	for (int i=0;i<n;i++) {      	
        		weightsPanel.add(new JLabel("H"+(i+1)), c);
        		c.gridx++;
        		weightsPanel.add(weightsV.get(i), c);
        		c.gridx=0;c.gridy++;
        	}        
        	weightsPanel.revalidate();
        }		
	}	
}
