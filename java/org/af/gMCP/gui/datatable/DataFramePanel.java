package org.af.gMCP.gui.datatable;

import java.util.Vector;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.af.commons.widgets.tables.CloseTabPanel;
import org.af.commons.widgets.tables.DFPanelIF;
import org.af.gMCP.gui.graph.EdgeWeight;
import org.af.gMCP.gui.graph.GraphView;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class DataFramePanel extends JTabbedPane implements ChangeListener, ListSelectionListener, DFPanelIF {
    private Vector<DataTable> tables = new Vector<DataTable>();
    private JScrollPane scrollPane;
    GraphView control;
    
    public DataFramePanel(RDataFrameRef dfRefW) {
    	addChangeListener(this);    	
    	tables.add(new DataTable(dfRefW));
    	this.addTab("Transition Matrix", getPanel(tables.get(0)));
    }
    
    private JPanel getPanel(DataTable table) {
    	JPanel panel = new JPanel();
    	table.getSelectionModel().addListSelectionListener(this);
    	table.getColumnModel().getSelectionModel().addListSelectionListener(this);
    	/*
    	 * if AutoReziseMode is set to something different to JTable.AUTO_RESIZE_OFF
    	 * the table will resize itself to fit into the width of the JScrollPane
    	 */
        //table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
    	JTable rowHeader = new JTable(new RowModel(table.getModel()));
		rowHeader.setRowHeight(table.getRowHeight());
        scrollPane = new JScrollPane(table);        
        scrollPane.setRowHeaderView(rowHeader);
        rowHeader.setPreferredScrollableViewportSize(rowHeader.getPreferredSize());
        
        String cols = "fill:pref:grow";
		String rows = "fill:pref:grow";

		FormLayout layout = new FormLayout(cols, rows);
		panel.setLayout(layout);
		CellConstraints cc = new CellConstraints();
        
		panel.add(scrollPane, cc.xy(1, 1)); 
        return panel;
    }
    
    public Vector<DataTable> getTable() {
        return tables;
    }

    Vector<CellEditorE> defaultEditors = new Vector<CellEditorE>(); 
    
	public void addLayer() {
		RDataFrameRef dfRefW = new RDataFrameRef();
		DataTable dt = new DataTable(dfRefW);
		CellEditorE de = new CellEditorE(control, dt, tables.size());
		defaultEditors.add(de);
		dt.setDefaultEditor(EdgeWeight.class, de);
		for (String s : tables.get(0).getNames()) {
			dt.getModel().addRowCol(s);
		}
		tables.add(dt);
		setTitleAt(0, "Transition matrix 1");
		addTab("Transition matrix "+tables.size(), getPanel(dt));
		if (getTabCount()==2) {
			setTabComponentAt(0, new CloseTabPanel(this));
		}
		setTabComponentAt(getTabCount()-1, new CloseTabPanel(this));
	}

	public void removeLayer(int i) {
		remove(i);
		tables.remove(i);	
		defaultEditors.remove(i);
		control.removeEntangledLayer(i);
		if (getTabCount()==1) {
			setTitleAt(0, "Transition matrix");
			setTabComponentAt(0, new JLabel("Transition matrix")); 
		} else {
			for (int j=0; j<tables.size(); j++) {
				setTitleAt(j, "Transition matrix "+(j+1));
			}
		}
		for (int j=i; j<tables.size(); j++) {
			defaultEditors.get(j).layer--;
		}
	}
	
	public void renameNode(int i, String name) {
		for (DataTable dt : getTable()) {
			dt.renameNode(i, name);
		}
	}

	public void registerControl(GraphView control) {
		this.control = control;
		defaultEditors.removeAllElements();		
		for (DataTable dt : getTable()) {
			CellEditorE de = new CellEditorE(control, dt, 0);
			defaultEditors.add(de);
			dt.setDefaultEditor(EdgeWeight.class, de);
		}
	}

	public void setTesting(boolean b) {
		for (DataTable dt : getTable()) {
			dt.setTesting(b);
		}		
	}

	public void delRowCol(int node) {
		for (DataTable dt : getTable()) {
			dt.getModel().delRowCol(node);
		}
	}

	public void addRowCol(String name) {
		for (DataTable dt : getTable()) {
			dt.getModel().addRowCol(name);
		}
	}

	public void setValueAt(EdgeWeight value, int row, int col, int layer) {
		getTable().get(layer).getModel().setValueAt(value, row, col);		
	}

	public void reset() {
		for (int i=tables.size()-1; i>0; i--) {
			tables.remove(i);
			remove(i);
		}		
	}
	
	public void stateChanged(ChangeEvent e) {
		int i = getSelectedIndex();
		/* If we are in the combined view, we stay in it */
		if (control != null && control.getNL().getSelectedIndex()!=0) {
			control.getNL().setSelectedIndex(i+1);
		}		
	}

	int oldi = -1;
	int oldj = -1;
	int oldLayer = -1;
	
	public void valueChanged(ListSelectionEvent e) {
		//int i = e.getFirstIndex();
		DataTable table = tables.get(getSelectedIndex());
		int i = table.getSelectedRow();
		int j = table.getSelectedColumn();
		int layer = getSelectedIndex();
		if (i!=oldi || j!=oldj) {
			control.getNL().highlightEdge(i, j, layer);			
		}
		oldi = i; 
		oldj = j;
		oldLayer = layer;
	}

}
