package org.af.gMCP.gui.dialogs;

import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.graph.NetList;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

/**
 * This dialog was used when we allowed for one-sided _and_ two-sided testing in the parametric case.
 */
public class AlternativesDialog extends JDialog implements ActionListener {

	JButton ok = new JButton("Ok");
	JTextArea jta;
	JPanel panel = new JPanel();
	List<JComboBox> altBoxes = new Vector<JComboBox>();
	public static final String[] alternatives = new String[] {"less", "greater", "two.sided"};

	public AlternativesDialog(JFrame parent, NetList nl) {
		super(parent, "Number of Hypotheses", true);
		setLocationRelativeTo(parent);
		
        String cols = "5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
        
        
        FormLayout layout = new FormLayout(cols, rows);
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();

        jta = new JTextArea(
        		"This test is appropriate if the p-values\n" +
        		"belong to test-statistics with a joint\n" +
        		"multivariate normal null distribution.\n" +
        		"Please specify below for each test whether\n" +
        		"it is a one-sided or a two-sided test.");		
		jta.setLineWrap(false);
		//jta.setWrapStyleWord(true);
		jta.setMargin(new Insets(4,4,4,4));        
        
        getContentPane().add(jta, cc.xy(2, 2));       	        
        
        rows = "5dlu, fill:pref:grow, 5dlu";
        cols = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu";
        for (int i=0;i<nl.getNodes().size();i++) {
        	rows = rows + ", pref, 5dlu";
        }
        
        layout = new FormLayout(cols, rows);
        panel.setLayout(layout);
        int row = 2;        
        CellConstraints cc2 = new CellConstraints();
        panel.add(new JLabel("Alternatives:"), cc2.xyw(4, row, 2));
        row += 2;
        
    	for (int i=0;i<nl.getNodes().size();i++) {
    		altBoxes.add(new JComboBox(alternatives));
    		altBoxes.get(i).setSelectedIndex(getIndex(Configuration.getInstance().getClassProperty(this.getClass(), "altBox"+i)));
    		panel.add(new JLabel(nl.getNodes().get(i).getName()), cc2.xy(2, row));
    		panel.add(altBoxes.get(i), cc2.xy(4, row));
    		row += 2;
    	}

    	JScrollPane sp = new JScrollPane(panel);
    	getContentPane().add(sp, cc.xy(2, 4));

    	row += 2;
        
        getContentPane().add(ok, cc.xy(2, 6));
        ok.addActionListener(this);        
        
        pack();
        setSize(this.getSize().width, Math.min(this.getSize().height, 600));
        setVisible(true);
	}

	private int getIndex(String alt) {
		if (alt.equals(alternatives[0])) return 0;
		if (alt.equals(alternatives[1])) return 1;
		return 2;
	}

	public void actionPerformed(ActionEvent e) {
		for (int i=0; i<altBoxes.size(); i++) {				
			Configuration.getInstance().setClassProperty(this.getClass(), "altBox"+i, altBoxes.get(i).getSelectedItem().toString());
		}
		dispose();
	}

	public String getAlternatives() {
		String s = "c(";
		for (int i=0; i<altBoxes.size(); i++) {
			s = s+"\""+altBoxes.get(i).getSelectedItem()+"\"";
			if (i != altBoxes.size()-1) s+= ", ";
		}
		return s+")";
	}
}
