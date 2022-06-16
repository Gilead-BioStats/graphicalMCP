package org.af.gMCP.gui.dialogs;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.DecimalFormat;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;

import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.graph.LaTeXTool;
import org.af.gMCP.gui.graph.Node;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class AdjustedPValueDialog extends JDialog implements ActionListener {

	JButton jb = new JButton("Ok");
	
	public AdjustedPValueDialog(JFrame mainFrame, List<Double> pValues, double[] adjPValues, Vector<Node> vector) {
		super(mainFrame, "Adjusted p-Values");

		String cols = "5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";
		String rows = "5dlu, pref, 5dlu, pref, 5dlu";
		for (int i=0; i<adjPValues.length; i++) {
			rows += ", pref, 5dlu";
		}

		FormLayout layout = new FormLayout(cols, rows);
		getContentPane().setLayout(layout);
		CellConstraints cc = new CellConstraints();

		int row = 2;		
		 
		getContentPane().add(new JLabel("Hypotheses"), cc.xy(2, row));		
		getContentPane().add(new JLabel("raw p-values"), cc.xy(4, row));		
		getContentPane().add(new JLabel("adjusted p-values"), cc.xy(6, row));
				
		DecimalFormat format = Configuration.getInstance().getGeneralConfig().getDecFormat();
		for (int i=0; i<adjPValues.length; i++) {			
			row += 2;			
			getContentPane().add(new JLabel(""+LaTeXTool.LaTeX2UTF(vector.get(i).getName())+":"), cc.xy(2, row));
			getContentPane().add(new JLabel(""+format.format(pValues.get(i))), cc.xy(4, row));
			getContentPane().add(new JLabel(""+format.format(adjPValues[i])), cc.xy(6, row));
		}
		
		row += 2;
		jb.addActionListener(this);		
		getContentPane().add(jb, cc.xy(6, row));
		pack();	
		
	    setLocationRelativeTo(mainFrame);
	    
		setVisible(true);		
	}

	public void actionPerformed(ActionEvent e) {
		dispose();		
	}

}
