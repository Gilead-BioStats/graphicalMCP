package org.af.gMCP.gui.datatable;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

import org.af.gMCP.gui.graph.EdgeWeight;
import org.af.gMCP.gui.graph.LaTeXTool;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class EpsilonTableCellRenderer extends DefaultTableCellRenderer {
	
	private static final Log logger = LogFactory.getLog(EpsilonTableCellRenderer.class);
	
	public EpsilonTableCellRenderer() { 
		super(); 
	}

    public void setValue(Object value) {
    	setText(value.toString());
    }

    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int col)	{    	
    	DataTableModel model = (DataTableModel) table.getModel();
    	
    	JLabel label = (JLabel) super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col);
    	
    	double sum = 0;
    	if (model.checkRowSum()) {    		
    		for (int i=0; i<model.getColumnCount(); i++) {
    			EdgeWeight ew = model.getValueAt(row, i);
    			Double d = 0.0;
    			try {
    				d = ew.getWeight(null);
    			} catch (Exception e) {
    				// Seriously - we don't want to do anything: d=0.0 is fine.
    			}
    			if (d>1.0001||d<0) sum=1000;
    			sum += (d>=0&&d<=1)?d:0;
    		}
    	}
    	if (model.checkCorr) {
    		try {
    			//System.out.println(""+model.getValueAt(row, col).getWeight(null));
    			double v = model.getValueAt(row, col).getWeight(null);
    			if (Math.abs(v)>1.00000001 || Double.isNaN(v)) {
    				label.setForeground(Color.RED);
            		label.setBackground(Color.ORANGE);
    			} else {
            		label.setForeground(null);
            		label.setBackground(null);
            	}
    		} catch (Exception e) {
				label.setForeground(Color.RED);
        		label.setBackground(Color.ORANGE);
    		}
    	}

        String text = value.toString();
        
        for (int i=0; i<LaTeXTool.greek.length; i++) {
			text = text.replaceAll("\\\\"+LaTeXTool.greekLaTeX[i], ""+LaTeXTool.greek[i]);			
		}
        
        label.setText(text);
        
        if (model.checkRowSum()) {
        	if(sum>1.0001) {
        		label.setForeground(Color.RED);
        		label.setBackground(Color.ORANGE);
        	} else {
        		label.setForeground(null);
        		label.setBackground(null);
        	}
        }
    	
    	if ((row==col && ! model.diagEditable) || model.testing) {
    		label.setForeground(Color.LIGHT_GRAY);
    	} else {
    		label.setForeground(null);
    	}
    	
    	return label;

    }


}
