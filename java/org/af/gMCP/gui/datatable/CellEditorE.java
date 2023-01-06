package org.af.gMCP.gui.datatable;

import java.awt.Component;

import javax.swing.DefaultCellEditor;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.table.TableCellEditor;

import org.af.gMCP.gui.graph.EdgeWeight;
import org.af.gMCP.gui.graph.GraphView;

public class CellEditorE extends DefaultCellEditor implements TableCellEditor {
    GraphView agc;
    DataTable dt;
    int row;
    int col;
    int layer = 0;
    
    public CellEditorE(GraphView agc, DataTable dt) {    	
        super(new JTextField());
		this.agc = agc;
		this.dt = dt;
    }

    public CellEditorE(GraphView agc, DataTable dt, int layer) {
		this(agc, dt);
		this.layer = layer;
	}

	public Component getTableCellEditorComponent(JTable table, Object value,
                                                 boolean isSelected, int row, int col) {
        String s = value.toString();
        this.row = row;
        this.col = col;
    	//addCellEditorListener(table);
    	// TODO: WHY DO I NEED THIS s.replace(',','.'); Yes - I know, this looks simple, but there are strange things out there.
		s = s.replace(',','.');
		oldVal = new EdgeWeight(s);
		((JTextField)getComponent()).setText(oldVal.toString());
		//((JTextField)getComponent()).addFocusListener(this);	 
        
        return this.getComponent();
    }

    private EdgeWeight oldVal;

    public EdgeWeight getCellEditorValue() {
    	String s = ((JTextField)getComponent()).getText();
    	oldVal = new EdgeWeight(s);
    	if (agc!=null) { 
    		agc.updateEdge(row, col, oldVal, layer); 
    	} else {
    		dt.getModel().setValueAt(oldVal, row, col);
    		dt.getModel().setValueAt(oldVal, col, row);
    	}
    	return oldVal;
    }
    
    /*
    
	public void focusGained(FocusEvent e) {
		// The following line does not seem to work the way I thought it would:
		((JTextField)getComponent()).selectAll();
	}

	public void focusLost(FocusEvent e) {
		try {
			if (!System.getProperty("java.runtime.version").startsWith("1.5.")) {
				stopCellEditing();
			}
		} catch(Exception ex) {
			// Nothing to do
		}
	}
	
	*/
    
}
