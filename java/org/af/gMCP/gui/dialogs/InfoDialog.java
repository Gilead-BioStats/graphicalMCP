package org.af.gMCP.gui.dialogs;

import java.awt.Color;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

/**
 * A JDialog class that defines a few static SimpleAttributeSets
 * for formatted text output.
 */
public class InfoDialog extends JDialog {
	
	 public InfoDialog(JFrame parent, String title) {
		 super(parent, title);
	 }
	
    public InfoDialog(JFrame parent, String title, boolean modal) {
    	super(parent, title, modal);
	}

	/**
     * Return the SimpleAttributeSet for a level 1 headline.
     * @return The SimpleAttributeSet for a level 1 headline.
     */
    public static SimpleAttributeSet getH1() {
    	SimpleAttributeSet attr = new SimpleAttributeSet();
		StyleConstants.setFontFamily(attr, "SansSerif");
        StyleConstants.setFontSize(attr, 14);
        StyleConstants.setBold(attr, true);
        StyleConstants.setAlignment(attr, StyleConstants.ALIGN_CENTER);        
        return attr;
    }
    
    /**
     * Return the SimpleAttributeSet for a level 1 headline.
     * @return The SimpleAttributeSet for a level 1 headline.
     */
    public static SimpleAttributeSet getC() {
    	SimpleAttributeSet attr = new SimpleAttributeSet();
        StyleConstants.setAlignment(attr, StyleConstants.ALIGN_CENTER);        
        return attr;
    }
    
    /**
     * Return the SimpleAttributeSet for normal text.
     * @return The SimpleAttributeSet for normal text.
     */
    public static SimpleAttributeSet getT() {
    	SimpleAttributeSet attr = new SimpleAttributeSet();
		StyleConstants.setFontFamily(attr, "SansSerif");
        StyleConstants.setFontSize(attr, 12);
        return attr;
    }
    
    /**
     * Return the SimpleAttributeSet for hyperlinks.
     * @return The SimpleAttributeSet for hyperlinks.
     */
    public static SimpleAttributeSet getLink() {
    	SimpleAttributeSet attr = new SimpleAttributeSet();
		StyleConstants.setFontFamily(attr, "SansSerif");
        StyleConstants.setFontSize(attr, 12);
        StyleConstants.setForeground(attr, new Color(0,0,160));
        StyleConstants.setUnderline(attr, true);
        return attr;
    }
}
