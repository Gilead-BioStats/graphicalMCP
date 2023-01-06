package org.af.gMCP.gui.dialogs;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JDialog;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

/**
 * A modal JDialog that shows a JColorChooser component 
 * and provides the method getColor() to get the selected
 * Color object afterwards.
 */

public class ColorChooseDialog extends JDialog implements ActionListener {
	JColorChooser colorChooser = new JColorChooser();
	JButton jb = new JButton("OK");
	
	public ColorChooseDialog(JDialog parent) {
		super(parent, "Choose Color", true);
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref, 5dlu";
        
        FormLayout layout = new FormLayout(cols, rows);
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();
		
        getContentPane().add(colorChooser, cc.xyw(1, 2, 5));
                
        jb.addActionListener(this);
        getContentPane().add(jb, cc.xy(4, 4));
        
        pack();
        this.setLocation(300, 300);
        setVisible(true);
	}
	
	
	public Color getColor() {
		return colorChooser.getColor();
	}


	public void actionPerformed(ActionEvent arg0) {
		dispose();		
	}
	
}
