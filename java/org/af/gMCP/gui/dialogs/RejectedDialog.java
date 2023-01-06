package org.af.gMCP.gui.dialogs;

import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.af.commons.widgets.RightClickTextMenuListener;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.graph.LaTeXTool;
import org.af.gMCP.gui.graph.Node;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class RejectedDialog extends JDialog implements ActionListener {

	JButton jb = new JButton("Ok");
	
	JTextArea jta = new JTextArea();
	JTextArea jta2 = new JTextArea();
	
	public RejectedDialog(JFrame mainFrame, boolean[] rejected, Vector<Node> vector, String output, String code) {
		super(mainFrame, "Rejected Null Hypotheses");

		String cols = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu";
		String rows = "5dlu, pref, 5dlu";
		for (int i=0; i<vector.size(); i++) {
			rows += ", pref, 5dlu";
		}
		if (output==null) {
			//Label R-Code, R-Code JTA, JButton
			rows +=", pref, 5dlu, fill:240dlu:grow, 5dlu, pref, 5dlu";
		} else {
			rows +=", pref, 5dlu, fill:120dlu:grow, 5dlu, fill:120dlu:grow, 5dlu, pref, 5dlu";
		}

		FormLayout layout = new FormLayout(cols, rows);
		getContentPane().setLayout(layout);
		CellConstraints cc = new CellConstraints();

		int row = 2;		
		
		getContentPane().add(new JLabel("Hypotheses"), cc.xy(2, row));
		getContentPane().add(new JLabel(""), cc.xy(4, row));
		
		for (int i=0; i<rejected.length; i++) {
			row += 2;
			getContentPane().add(new JLabel(""+LaTeXTool.LaTeX2UTF(vector.get(i).getName())+":"), cc.xy(2, row));
			getContentPane().add(new JLabel(""+(rejected[i]?"rejected":"not rejected")), cc.xy(4, row));
		}	

		if (code != null && Configuration.getInstance().getGeneralConfig().showRCode()) {
			jta2.setText(code);
			jta2.setMargin(new Insets(4,4,4,4));
			
			row += 2;
			
			getContentPane().add(new JLabel("R code for reproducing these results:"), cc.xyw(2, row, 3));
			
			row += 2;
			
			jta2.setFont(new Font("Monospaced", Font.PLAIN, 12));
			jta2.setLineWrap(false);
			jta2.addMouseListener(new RightClickTextMenuListener(jta2));
			//jta.setWrapStyleWord(true);
			getContentPane().add(new JScrollPane(jta2), cc.xyw(2, row, 3));			
		}	
		
		if (output != null) {
			jta.setText(output);
			jta.setMargin(new Insets(4,4,4,4));
			jta.setFont(new Font("Monospaced", Font.PLAIN, 12));
			jta.setLineWrap(false);
			//jta.setWrapStyleWord(true);
			row += 2;
			
			jta.addMouseListener(new RightClickTextMenuListener(jta));
			getContentPane().add(new JScrollPane(jta), cc.xyw(2, row, 3));
		}		

		row += 2;
		jb.addActionListener(this);
		getContentPane().add(jb, cc.xy(4, row));
		
		pack();
		
	    setLocationRelativeTo(mainFrame);
	    
		setVisible(true);		
	}

	public void actionPerformed(ActionEvent e) {
		dispose();		
	}

}
