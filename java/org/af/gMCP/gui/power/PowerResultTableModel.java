package org.af.gMCP.gui.power;

import javax.swing.table.DefaultTableModel;

public class PowerResultTableModel extends DefaultTableModel {

	public PowerResultTableModel(Object[][] data, String[] colnames) {
		super(data, colnames);
	}

	public Class<?> getColumnClass(int columnIndex) {
	    if (columnIndex == 0) {
	        return String.class;
	    } else {
	    	return Double.class;
	    }
	}
	
}
