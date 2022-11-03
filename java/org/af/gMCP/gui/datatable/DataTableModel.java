package org.af.gMCP.gui.datatable;

import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;

import org.af.gMCP.gui.graph.EdgeWeight;

public class DataTableModel extends AbstractTableModel {

	protected RDataFrameRef df;
	private RowModel rowModel = null;
	public boolean diagEditable = false;
	boolean testing = false;
	// Note that if both checkRowSum and checkCorr are true only checkRowSum will be checked by the EpsilonTableCellRenderer. 
	boolean checkRowSum = true;
	boolean checkCorr = false;

    public DataTableModel(RDataFrameRef df) {
        this.df = df;
    }

    public int getColumnCount() {
        return df.getColumnCount();
    }

    public String getColumnName(int col) {
        return df.getColName(col);
    }
    
    public int getRowCount() {
        return df.getRowCount();
    }

    public boolean isCellEditable(int rowIndex, int col) {
        return (rowIndex != col || diagEditable) && !testing;
    }

    public void setValueAt(EdgeWeight value, int row, int col) {
        getDataFrame().setValue(row, col, value);		
		fireTableChanged(new TableModelEvent(this, row));
    }

    public EdgeWeight getValueAt(int row, int col) {
        return df.getElement(row, col);
    }
    
    public Class<?> getColumnClass(int col) {
        return EdgeWeight.class;
    }
    
    public void addRowCol(String name) {
        df.addRowCol(name);
        fireTableRowsInserted(df.getColumnCount(), df.getColumnCount());        
        fireTableStructureChanged();  	
	}

    public void delRowCol(int col) {
        df.delRowCol(col);
        fireTableStructureChanged();
    }

    public RDataFrameRef getDataFrame() {
        return df;
    }
    
    public void fireTableStructureChanged() {
    	super.fireTableStructureChanged();
    	if (rowModel!=null) rowModel.fireTableStructureChanged();
    }    
    
	public void setRowModel(RowModel rowModel) {
		this.rowModel  = rowModel;		
	}

	public void setTesting(boolean testing) {
		this.testing = testing;		
	}

	public void removeAll() {
		df.removeAll();		
	}

	boolean checkRowSum() {		
		return checkRowSum;
	}
	
	public void setCheckRowSum(boolean checkRowSum) {
		this.checkRowSum = checkRowSum;
	}

	/**
	 * Copys values from one DataTableModel into another
	 * @param model DataTableModel that will be copied.
	 */
	public void copy(DataTableModel model) {
		for (int i=0; i<model.getRowCount(); i++) {
			for (int j=0; j<model.getColumnCount(); j++) {
				getDataFrame().setValue(i, j, model.getValueAt(i, j));			
			}			
		}
		this.fireTableDataChanged();		
	}

	public void checkCorMat() {
		diagEditable = false;
		checkCorr = true;
	}
	
}

