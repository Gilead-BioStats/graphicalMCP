package org.af.gMCP.gui;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;

import org.af.commons.widgets.WidgetFactory;
import org.af.commons.widgets.buttons.OKButtonPane;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.dialogs.InfoDialog;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Displays Information about the program, its licenses, used libraries,
 * their licenses, the web site ...
 */
public class AboutDialog extends InfoDialog implements ActionListener {
	private static Log logger = LogFactory.getLog(AboutDialog.class);
    private static final long serialVersionUID = 1L;
    
    JTextPane jtAbout = new JTextPane();
    
	
    public AboutDialog(JFrame parent) {
    	super(parent, "About gMCP-GUI "+Configuration.getInstance().getGeneralConfig().getVersionNumber());

        jtAbout.setStyledDocument(getDocument());
        jtAbout.setEditable(false);

        Container cp = getContentPane();
		cp.add(new JScrollPane(jtAbout));
        cp = WidgetFactory.makeDialogPanelWithButtons(cp, new OKButtonPane(), this);
		setContentPane(cp);

        pack();
		setLocationRelativeTo(parent);
		setVisible(true);
    }
    
    private DefaultStyledDocument getDocument() {
    	DefaultStyledDocument doc = new DefaultStyledDocument();
        logger.info("Creating About-Text.");
		try {			
			doc.insertString(doc.getLength(),					
					"gMCP "+Configuration.getInstance().getGeneralConfig().getVersionNumber()+"\n\n", getH1());			
			doc.insertString(doc.getLength(),
					"by Kornelius Rohmeyer and Florian Klinglmueller is distributed under GPL 2.0."+"\n\n", getT());			
			doc.insertString(doc.getLength(), "This program uses the libraries log4j, JLaTeXMath, POI, iText (2.1.4), jxlayer,\n swingworker, commons logging/lang, JRI and JGoodies Forms.\n", getT());
			doc.insertString(doc.getLength(),"\n"
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
					+"found at: http://www.gnu.org/licenses/\n", getT());
			doc.setParagraphAttributes(0, doc.getLength(), getC(), true);
        } catch (BadLocationException ble) {
        	logger.error("BadLocationException was thrown. Should never happen.", ble);
        }
    	return doc;
    }
    

    
	/**
	 * Evaluates ActionEvents.
	 * @param e ActionEvent
	 */
	public void actionPerformed(ActionEvent e) {		
		if (e.getActionCommand().equals(OKButtonPane.OK_CMD)) {
			dispose();
		}
		/* else if (jbVersion.getText().equals(e.getActionCommand())) {
			URL helpURL = null;
			try {		
				//TODO what happens if the user is offline like in the completely offline variant?
                helpURL = control.getConf().getWebstartConfig().getProjectVersionPageURL();
				jtAbout.setPage(helpURL);
			} catch (IOException ioe) {
		        logger.warn("Exception while displaying "+helpURL+".", ioe);
			}			
		}*/
	}
}
