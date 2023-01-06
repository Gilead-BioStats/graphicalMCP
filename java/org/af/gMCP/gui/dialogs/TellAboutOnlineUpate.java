package org.af.gMCP.gui.dialogs;

import java.awt.Container;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;

import org.af.commons.widgets.WidgetFactory;
import org.af.commons.widgets.buttons.OKButtonPane;
import org.af.gMCP.config.Configuration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class TellAboutOnlineUpate extends InfoDialog implements ActionListener {
	private static Log logger = LogFactory.getLog(TellAboutOnlineUpate.class);

	JButton jb = new JButton("Ok");
	private JCheckBox checkOnlineForUpdate = new JCheckBox("Check online for updates");
	
    JTextPane jtAbout = new JTextPane();
	
	public TellAboutOnlineUpate(JFrame mainFrame) {
		super(mainFrame, "Check for online updates, privacy statement and license", true);

		getContentPane().setLayout(new GridBagLayout());
		
		GridBagConstraints c = new GridBagConstraints();

		jtAbout.setStyledDocument(getDocument());
		jtAbout.setEditable(false);
		
		JTextArea jta3 = new JTextArea("gMCP - Graph based Multiple Comparison Procedures.\n"
				+"Copyright (C) 2009-2014 by K. Rohmeyer and F. Klinglmueller\n"
				+"\n"
				+"This program is free software; you can redistribute it and/or\n"
				+"modify it under the terms of the GNU General Public License\n"
				+"as published by the Free Software Foundation; either version 2\n"
				+"of the License, or (at your option) any later version.\n"
				+"\n"
				+"This program is distributed in the hope that it will be useful,\n"
				+"but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
				+"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
				+"GNU General Public License for more details. It is included\n" 
				+"in the R distribution (in directory share/licenses) or can be\n" 
				+"found at: http://www.gnu.org/licenses/\n");
		jta3.setFont(new Font("Monospaced", Font.PLAIN, 10));

		c.fill = GridBagConstraints.HORIZONTAL;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=10; c.ipady=10;
		c.weightx=1; c.weighty=1;
		
		checkOnlineForUpdate.setSelected(Configuration.getInstance().getGeneralConfig().checkOnline());
		
		c.gridx=0; 
		(getContentPane()).add(jtAbout, c);
		c.gridy++;
		(getContentPane()).add(checkOnlineForUpdate, c);
		c.gridy++; c.weighty=1;
		JScrollPane js = new JScrollPane(jta3);
		(getContentPane()).add(js, c);	
		
        Container cp = getContentPane();
        cp = WidgetFactory.makeDialogPanelWithButtons(cp, new OKButtonPane(), this);
		setContentPane(cp);
		
		pack();	
		
	    setLocationRelativeTo(mainFrame);
	    
		setVisible(true);		
	}

    private DefaultStyledDocument getDocument() {
    	DefaultStyledDocument doc = new DefaultStyledDocument();
        logger.info("Creating About-Text.");
		try {			
			doc.insertString(doc.getLength(),					
					"The gMCP-GUI would like to check online for updates on each start-up.\n", getH1());			
			doc.insertString(doc.getLength(),
					"No information about your computer is send.\n" +
					"Nevertheless you can disable this feature with the following checkbox\n" +
					"or later from the options dialog.", getT());			
			doc.setParagraphAttributes(0, doc.getLength(), getC(), true);
        } catch (BadLocationException ble) {
        	logger.error("BadLocationException was thrown. Should never happen.", ble);
        }
    	return doc;
    }	
	
	public void actionPerformed(ActionEvent e) {
		Configuration.getInstance().getGeneralConfig().setCheckOnline(checkOnlineForUpdate.isSelected());
		dispose();		
	}

}
