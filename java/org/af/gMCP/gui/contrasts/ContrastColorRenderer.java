package org.af.gMCP.gui.contrasts;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ContrastColorRenderer extends JLabel
                           implements TableCellRenderer {
	
	public static final long serialVersionUID = 1L;
	protected static final Log logger = LogFactory.getLog(ContrastColorRenderer.class);
	
    public ContrastColorRenderer() {
    	setOpaque(true);
    }

    public Component getTableCellRendererComponent(
                            JTable table, Object object,
                            boolean isSelected, boolean hasFocus,
                            int row, int column) {
    	
    	Float v = 0f;
    	try {    		    		
    		v = new Float(object.toString()).floatValue() * 0.9f;    		
    	} catch (java.lang.NumberFormatException e) {
    		logger.warn("Could not parse "+object+".");
    	}
    	
    	if (v>0.9f|| v<-0.9f) {
    		setBackground(Color.YELLOW);
    	} else {
    		if(v > 0) {
    			setBackground(new Color(1f-v,1f-v,1));
    		} else if (v < 0) {
    			setBackground(new Color(1,1f+v,1f+v));        	
    		} else {
    			setBackground(Color.WHITE);        	
    		}       
    	}
        
        this.setText(object==null?"null":object.toString());
        
        return this;
    }
}
