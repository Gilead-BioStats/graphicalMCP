package org.af.gMCP.gui.contrasts;

import java.util.List;
import java.util.Vector;

import javax.swing.JLabel;
import javax.swing.table.AbstractTableModel;

public class ContrastTableModel extends AbstractTableModel {

    List<String> header;

	public List<List<Double>> data = new Vector<List<Double>>(); 
	JLabel jlInfo;

    public ContrastTableModel(List<String> header, JLabel jlInfo) {
    	this.header = header;
    	this.jlInfo = jlInfo;
    	
    	for (String s : header) {
    		data.add(new Vector<Double>());
    	}

    }

    public int getColumnCount() {
        return header.size();
    }

    public int getRowCount() {
        return data.get(0).size();
    }

    public Object getValueAt(int r, int c) {
    	return data.get(c).get(r);
    }

    public void setValueAt(Object value, int row, int col) {
    	if (value != null && value instanceof Double) {
    		data.get(col).set(row, (Double) value);
    	}
    	check();
    }


    public String getColumnName(int column) {
        return header.get(column);
    }


    public Class<?> getColumnClass(int columnIndex) {
        return Double.class;
    }

    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return true;
    }

    public void addLine() {
    	for (int i=0; i< data.size(); i++) {
    		data.get(i).add(0.);
    	}
        fireTableStructureChanged();
    }
    
    public void addLine(Double[] row) {
    	for (int i=0; i< data.size(); i++) {
    		data.get(i).add(row[i]);
    	}
        fireTableStructureChanged();
    }

    public void removeLine(int i) {
    	for (List<Double> row : data) {
    		row.remove(i);
    	}
        fireTableStructureChanged();
    }

    public Double sum(int row) {
    	double sum = 0;
    	for (int i=0; i<data.size(); i++) {
			sum += data.get(i).get(row);
		}
    	return sum;
    }
    
    public Double sumabs(int row) {
    	double sum = 0;
    	for (int i=0; i<data.size(); i++) {
			sum += Math.abs(data.get(i).get(row));
		}
    	return sum;
    }
    

    public void check() {
    	String msg = "";
    	List<Integer> list = new Vector<Integer>();    	
    	for (int i=0; i<data.get(0).size(); i++) {
			if (sum(i)>(ratio?1:0)+0.001 || sum(i)<(ratio?1:0)-0.001) {
				list.add(i+1);
			}
    	}    	
    	if (list.size()!=0) {
    		if (list.size()==1) {
    			msg += "Line "+list.get(0)+" does not sum up to "+(ratio?1:0)+".";
    		} else {    			
    			for (int row : list) {
    				if (msg.equals("")) {
    					msg += "Line "+row;
    				} else {
    					msg += ", "+row;
    				}
    			}
    			msg += " do not sum up to "+(ratio?1:0)+".";
    		}
    	}
    	
    	jlInfo.setText(msg);
    }
    
    public void scale() {
    	for (int i=0; i<data.get(0).size(); i++) {
    		if (sum(i)==0) {
    			double sumabs = sumabs(i);
    			if (sumabs!=0) {
    				for (int j=0; j<data.size(); j++) {
    					setValueAt(2*data.get(j).get(i)/sumabs,i,j);
    				}
    			}
    		}
    	}	
    	fireTableStructureChanged();
    }

	public String getMatrix() {
		if (data.size()==0 || data.get(0).size()==0) return "matrix()";
		String s = "matrix(data=c( ";
		for (List<Double> l : data) {
			for (Double d : l) {
				s += d+",";
			}					
    	} 
		s = s.substring(0, s.length()-1);
		s += "), nrow="+data.get(0).size()+", ncol="+data.size()+", byrow = FALSE)";
		return s;
	}
	
	boolean ratio = false;

	public void setRatio(boolean b) {
		this.ratio = b;		
	}

}
