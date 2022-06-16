package org.mutoss.gui;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JPanel;

public class JavaGDPanel extends JPanel {
	public JavaGDPanel() {
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=0; c.ipady=0;
		c.weightx=1; c.weighty=1;
		this.setLayout(new GridBagLayout());
		add((Component) JavaGD.getLastJavaGD().c, c);
	} 
}
