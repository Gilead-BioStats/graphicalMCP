package org.af.gMCP.gui.datatable;

import java.awt.Component;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class SingleDataFramePanel extends JPanel {
    private DataTable table;
    private JScrollPane scrollPane;
    JTable rowHeader;

    public SingleDataFramePanel(RDataFrameRef dfRefW) {
    	table = new DataTable(dfRefW);
    	/*
    	 * if AutoReziseMode is set to something different to JTable.AUTO_RESIZE_OFF
    	 * the table will resize itself to fit into the width of the JScrollPane
    	 */
        //table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        
    	rowHeader = new JTable(new RowModel(table.getModel())) {
    		// We want to be able to disable the rowHeader.
    		public Component prepareRenderer (final TableCellRenderer renderer, int row, int column) {
    			Component renderer2 = super.prepareRenderer(renderer, row, column);
    			renderer2.setEnabled(isEnabled());
    			return renderer2;
    		}
    	};
		rowHeader.setRowHeight(table.getRowHeight());
        scrollPane = new JScrollPane(table);
        scrollPane.setRowHeaderView(rowHeader);
        rowHeader.setPreferredScrollableViewportSize(rowHeader.getPreferredSize());
        
        String cols = "fill:pref:grow";
		String rows = "fill:pref:grow";

		FormLayout layout = new FormLayout(cols, rows);
		setLayout(layout);
		CellConstraints cc = new CellConstraints();
        
        add(scrollPane, cc.xy(1, 1));        
    }
    
    public void setEnabled(boolean enabled) {
    	super.setEnabled(enabled);
    	table.setEnabled(enabled);
    	table.getTableHeader().setEnabled(enabled);
    	rowHeader.setEnabled(enabled);
    }
    
    public DataTable getTable() {
        return table;
    }
}
