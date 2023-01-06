package org.af.gMCP.gui.power;

import javax.swing.table.DefaultTableModel;

public class SampSizeResultTableModel extends DefaultTableModel {

	public SampSizeResultTableModel(Object[][] data, String[] colnames) {
		super(data, colnames);
	}

	public Class<?> getColumnClass(int columnIndex) {
	    if (columnIndex <= 1) {
	        return String.class;
	    } else {
	    	return Double.class;
	    }
	}
	
}
