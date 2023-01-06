package org.af.gMCP.gui.datatable;

import java.util.List;
import java.util.Vector;

import org.af.gMCP.gui.graph.EdgeWeight;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class RDataFrameRef {
	
	private static final Log logger = LogFactory.getLog(RDataFrameRef.class);

	List<String> rcNames = new Vector<String>();
	Vector<Vector<EdgeWeight>> data = new Vector<Vector<EdgeWeight>>();

	public List<String> getNames() {
		return rcNames;
	}
	
	public String getColName(int col) {
		return rcNames.get(col);
	}
	
	public void setName(int i, String name) {
		rcNames.set(i, name);
	}
	
	public String getRowName(int row) {
		return rcNames.get(row);
	}
	
	public void setNames(List<String> names) {
		rcNames = names;
	}

	public void setValue(int row, int col, EdgeWeight value) {
		data.get(row).set(col, value);
	}

	public EdgeWeight getElement(int row, int col) {
		return data.get(row).get(col);
	}
	
	public void delRowCol(int col) {
		rcNames.remove(col); 
		data.remove(col);
		for (Vector<EdgeWeight> v: data) {
			v.remove(col);
		}
	}

	public int getRowCount() {
		return data.size();
	}

	public int getColumnCount() {
		return data.size();
	}

	public void addRowCol(String name) {
		rcNames.add(name);
		Vector<EdgeWeight> row = new Vector<EdgeWeight>();
		for (int i=0; i < getColumnCount(); i++) {row.add(new EdgeWeight(0.0));}
		data.add(row);
		for (int i=0; i < getRowCount(); i++) {data.get(i).add(new EdgeWeight(0.0));}
		//logger.info("Data has "+getRowCount()+" rows and "+getColumnCount()+" columns now.");
	}

	public String getRMatrix() {
		String m = "matrix(c(";
		for (int i=0; i < getRowCount(); i++) {
			for (int j=0; j < getColumnCount(); j++) {
				m += data.get(i).get(j)+((i == getRowCount() -1 && j == getColumnCount() -1)?"":",");
			}
		}
		m += "), nrow="+getRowCount()+")";
		return m;
	}

	public void removeAll() {
		for (int i=getRowCount()-1; i>=0; i--) {
			delRowCol(i);
		}		
	}

}
