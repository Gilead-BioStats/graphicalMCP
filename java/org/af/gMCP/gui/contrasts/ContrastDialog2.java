package org.af.gMCP.gui.contrasts;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.rmi.RemoteException;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.af.commons.widgets.WidgetFactory;
import org.af.jhlir.call.RDataFrame;
import org.af.jhlir.call.RMatrixDouble;

public class ContrastDialog2 extends StatMethodDialog implements ActionListener {
    
    private GroupingBox cbGroup;
    private LevelOrderSL slLevelOrder;
    //private JComboBox jbcontrol;
    private ROptionBox<String> cbType;
    private ContrastTable pTable;
    private JButton jbAdd;
    private JButton jbScaleCM;
    public JLabel jlInfo; 
    
    public ContrastDialog2(JFrame window, RDataFrame df) {
        super(window, "Contrast Building", df);       
        cbGroup = AnalysisDialogFactory.makeGroupingComboBox(df);
        slLevelOrder = AnalysisDialogFactory.makeLevelOrderSL(df, cbGroup);
        slLevelOrder.allToRight();
        
        jbAdd = new JButton("Add Contrast");
        jbAdd.addActionListener(this); 
        jbScaleCM = new JButton("Scale contrasts");
        jbScaleCM.addActionListener(this);
        jlInfo = new JLabel("Please add contrasts.");
        
        pTable = new ContrastTable(df, cbGroup, slLevelOrder, jlInfo);
        
        String[] a = {"Dunnett", "Tukey", "Sequen", "AVE", 
                "Changepoint", "Williams", "Marcus", 
                "McDermott", "UmbrellaWilliams", "GrandMean"}; // This array you are allowed to rename!
        String[] b = {"Dunnett", "Tukey", "Sequen", "AVE", 
                "Changepoint", "Williams", "Marcus", 
                "McDermott", "UmbrellaWilliams", "GrandMean"};        
        cbType = new ROptionBox<String>("type", a, b);
        
        JPanel panel = new JPanel();
        panel.add(jbAdd);
        panel.add(jbScaleCM);
        
        String[] labels = {//"Group", "Level Order", //"Control", 
        		"Type", null, "Contrasts", null};
        
        Component[] comps = {//cbGroup, slLevelOrder, //jbcontrol, 
        		cbType, panel, new JScrollPane(pTable), jlInfo};

        setContent(labels, comps);
        setModal(true);
        doTheLayout();
        pack();
        WidgetFactory.showCompleteTitle(this);
        setLocationRelativeTo(getOwner()); 
        setVisible(true);
    }
    
    protected void checkConstraints() {        
        requiresFactVar();
    }
    
    public RMatrixDouble showAndGetMatrix() {
    	setVisible(true);
    	dispose();
    	return getMatrix();
    }
    
    public RMatrixDouble getMatrix() {
		return null;    	
		//return createMatrix(pTable.getMatrix(), "m"); 
    }

    /* protected void onOk() {
        String group = cbGroup.getSelectedItem().toString();
		List<String> levelOrder = slLevelOrder.getRight();
		//String contr = jbcontrol.getSelectedItem().toString();
		String type = cbType.getSelectedObject();
		dispose();
    }*/
    
    public void actionPerformed(ActionEvent e) {
        if (e.getSource().equals(jbAdd)) {        	
        	RMatrixDouble m = getContrastMatrix(cbGroup.getSelectedItem().toString(), cbType.getSelectedObject(), slLevelOrder.getRight());        	
        	try {
				pTable.addMatrix(m);
			} catch (RemoteException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
        	/* List<Double> l = new Vector<Double>();        	
        	pTable.addMatrix(getAnalysis().getColumns(l, m)); */
        } else if (e.getSource().equals(jbScaleCM)) {        	
        	pTable.scale();
        }        
    }
    

	/**
	 * @param groupvarname column of dataframe for which the contrast should be generated.
	 * @param type type of contrast, i.e. one of 
	 * "Dunnett", "Tukey", "Sequen", "AVE",
	 * "Changepoint", "Williams", "Marcus",
	 * "McDermott", "UmbrellaWilliams" or "GrandMean".
	 * @param list 
	 */
	 public RMatrixDouble getContrastMatrix(String groupvarname, String type, List<String> list) {
		//return new RMatrixDoubleREngine(MuTossControl.getR(), (REXPDouble) MuTossControl.getR().call("mutossGUI:::myContrMat", type, list, df, groupvarname).getWrapped());
		 return null;
	} 

    
	 protected void onOk() throws RemoteException {
		 //MuTossControl.getObj().setHypotheses(MuTossControl.getR().eval(pTable.getMatrix()));
		 dispose();
	 }

}

