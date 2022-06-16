package org.af.gMCP.gui.contrasts;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.rmi.RemoteException;
import java.util.List;

import javax.swing.JLabel;

import org.af.commons.widgets.MyJPopupMenu;
import org.af.commons.widgets.tables.ExtendedJTable;
import org.af.jhlir.call.RDataFrame;
import org.af.jhlir.call.RMatrixDouble;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ContrastTable extends ExtendedJTable implements ActionListener {

	private MyJPopupMenu popupMenu;
	private GroupingBox cbGroup;
	private LevelOrderSL slLevelOrder;
	private RDataFrame df;
	protected static final Log logger = LogFactory.getLog(ContrastTable.class);
	JLabel jlInfo;

    public ContrastTable(RDataFrame df, GroupingBox cbGroup, LevelOrderSL slLevelOrder, JLabel jlInfo) {
    	super();
    	setDefaultRenderer(Double.class, new ContrastColorRenderer());
    	cbGroup.addActionListener(this);
    	this.cbGroup = cbGroup;
    	this.slLevelOrder = slLevelOrder;
    	this.jlInfo = jlInfo;
    	this.df = df;
    	buildModel();

        setRowSelectionAllowed(true);

        popupMenu = new MyJPopupMenu(new String[]{"Add Line", "Delete Line"}, new String[]{"add_line", "del_line"}, this);
        setComponentPopupMenu(popupMenu);
        setPreferredWidth(800);
    }


    public void addMatrix(RMatrixDouble m) throws RemoteException {
    	logger.info("Adding matrix:\n "+m+".");
    	for (int i=0; i < m.nrow(); i++) {
    		logger.info("Adding row "+i+".");
    		getContrastModel().addLine(m.getRow(i));
    	}
    	logger.info("Contrast matrix successfully added.");
    }

    private void buildModel() {
    	List<String> groupLevels = slLevelOrder.getRight(); //((DataFrameColumnFactor) df.getColumn(cbGroup.getGrouping())).getLevels();
    	this.setModel(new ContrastTableModel(groupLevels, jlInfo));
	}

	public ContrastTableModel getContrastModel() {
        return (ContrastTableModel) super.getModel();
    }

    public void actionPerformed(ActionEvent e) {
        if (e.getActionCommand().equals("add_line")) {
            getContrastModel().addLine();
        } else if (e.getActionCommand().equals("del_line")) {
            int[] rows = getSelectedRows();
            for (int i : rows) {
                if (i >= 0 && i < getRowCount())
                    getContrastModel().removeLine(i);
            }
        } else if (e.getSource().equals(cbGroup)) {
        	buildModel();
        }
        getContrastModel().check();
    }

	public void scale() {
		getContrastModel().scale();
	}

	public String getMatrix() {
		return getContrastModel().getMatrix();
	}

	public void setRatio(boolean b) {
		getContrastModel().setRatio(b);
	}
}
