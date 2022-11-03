package org.af.gMCP.gui.power;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JDialog;

import org.af.gMCP.gui.graph.Node;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class PopulationDialog extends JDialog implements ActionListener {
	

    UserDefinedPanel udp;
    JButton ok = new JButton("OK");

	
	public PopulationDialog(PDialog pd) {
		super(pd, "Populations", true);
		setLocationRelativeTo(pd);
		
		List<Node> nodes = pd.getNodes();

        String cols = "5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
        String rows = "5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
        
        getContentPane().setLayout(new FormLayout(cols, rows));
        CellConstraints cc = new CellConstraints();
		
		int row = 2;
		
		udp = new UserDefinedPanel(pd, pd.getNodes(), true);		
		getContentPane().add(udp, cc.xyw(2, row, 3));
		
		row +=2;

		ok.addActionListener(this);
		getContentPane().add(ok, cc.xy(4, row));
		
		pack();
		setVisible(true);
		
	}
	
	public void actionPerformed(ActionEvent e) {
		dispose();
	}
	    
}
