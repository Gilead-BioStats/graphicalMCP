package org.af.gMCP.gui.power;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

import org.af.gMCP.gui.graph.Node;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class KoutofNDialog extends JDialog implements ActionListener {

	JButton ok = new JButton("Ok");
	JTextField howMany = new JTextField(5);
	List<JCheckBox> checkBox = new Vector<JCheckBox>();
	JLabel outOfN;
	int n;
	List<Node> nodes;

	public KoutofNDialog(JDialog parent, List<Node> nodes) {
		super(parent, "k out of n", true);
		setLocationRelativeTo(parent);
		this.nodes = nodes;
		
        String cols = "5dlu, pref, 5dlu, pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref, 5dlu";
        
        for (Node n : nodes) {
        	rows += ", pref, 5dlu";
        }        
               
        FormLayout layout = new FormLayout(cols, rows);
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 2;
        
        getContentPane().add(new JLabel("At least"), cc.xy(2, row));
        getContentPane().add(howMany, cc.xy(4, row));
        n = nodes.size();
        howMany.setText(""+(n-1));
        outOfN = new JLabel("out of the following "+n+":");
        getContentPane().add(outOfN, cc.xy(6, row));
        
        row += 2;
        
        for (Node n : nodes) {
        	
        	JCheckBox jc = new JCheckBox(n.getName(), true);
        	jc.addActionListener(this);
        	checkBox.add(jc);        	
        	getContentPane().add(jc,     cc.xyw(2, row, 7));       	        

        	row += 2;
        }
        
        getContentPane().add(ok, cc.xy(8, row));
        ok.addActionListener(this);        
        
        pack();
        setVisible(true);
	}

	public void actionPerformed(ActionEvent e) {
		int i = checkBox.indexOf(e.getSource());
		if ( i != -1 ) {
			n = 0;
			for (JCheckBox cb : checkBox) {
				if (cb.isSelected()) n++;
			}
			outOfN.setText("out of the following "+n+":");
		} else if (e.getSource()==ok) {
			if (n<2) {
				JOptionPane.showMessageDialog(this, "Please select at least two hypotheses.", "Please select at least two hypotheses", JOptionPane.ERROR_MESSAGE);
				return;
			}
			dispose();
		}		
	}
	
	public String getCommand() {
		if (n==nodes.size()) return "sum(x)>="+howMany.getText();
		String s = "";
		for (int i=0; i<checkBox.size(); i++) {
			if (checkBox.get(i).isSelected()) {
				s += (i+1)+",";
			}
		}		
		s = s.substring(0, s.length()-1);
		return "sum(x[c("+s+")])>="+howMany.getText();
	}


}
