package org.af.gMCP.gui.power;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.List;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.widgets.DesktopPaneBG;
import org.af.commons.widgets.validate.RealTextField;
import org.af.gMCP.gui.graph.Node;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class NCPCalculatorDialog extends JDialog implements ActionListener {

	JButton jbCalc = new JButton("Calculate");
	JButton jbReset = new JButton("Reset");
	JButton jbSaveClose = new JButton("Save and Close");
	JButton jbCancelClose = new JButton("Cancel and Close");
	JButton checkAll = new JButton("Check/Uncheck all"); 
	JButton jbHelp;
	List<RealTextField> mlV = new Vector<RealTextField>(); 
	List<RealTextField> mpV = new Vector<RealTextField>();
	List<RealTextField> ncpV = new Vector<RealTextField>();
	List<JCheckBox> saveV = new Vector<JCheckBox>();
	
	NCPRequestor ncpR;
	
	/**
	 * Constructor
	 * @param parent Parent JFrame
	 */
	public NCPCalculatorDialog(PDialog pd, NCPRequestor ncpR) {
		super(pd, "NCP Calculator - Marginal Power", true);
		this.ncpR = ncpR;
		setLocationRelativeTo(pd);
		Vector<Node> nodes = pd.parent.getGraphView().getNL().getNodes();
		
		String cols = "5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
        String rows = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        
        for (int i=0; i<Math.max(nodes.size(), 2); i++) {
        	cols += ", max(50dlu;pref), 5dlu";
        }
        
        getContentPane().setLayout(new FormLayout(cols, rows));
        CellConstraints cc = new CellConstraints();
		
		int row = 2;		
				
		for (int i=0; i<nodes.size(); i++) {
			getContentPane().add(new JLabel(nodes.get(i).getName()) , cc.xy(4+2*i, row));
		}
		
		row +=2;
		
		getContentPane().add(new JLabel("Marginal level") , cc.xy(2, row));
		
		for (int i=0; i<nodes.size(); i++) {
			RealTextField ml = new RealTextField("", 0, 1);
			ml.setText("0.025"); //TODO Save and restore default value.
			mlV.add(ml);
			getContentPane().add(ml, cc.xy(4+2*i, row));
		}
		
		row +=2;
		
		getContentPane().add(new JLabel("Marginal power") , cc.xy(2, row));
		
		for (int i=0; i<nodes.size(); i++) {
			RealTextField mp = new RealTextField("", 0, 1);
			mpV.add(mp);
			getContentPane().add(mp, cc.xy(4+2*i, row));
		}
		
		row +=2;
		
		getContentPane().add(new JLabel("NCP") , cc.xy(2, row));
		
		List<Double> oldNCP = ncpR.getOldNCP();
		
		for (int i=0; i<nodes.size(); i++) {
			RealTextField ncp = new RealTextField("", Double.MIN_VALUE, Double.MAX_VALUE);
			Double d = oldNCP.get(i);
			ncp.setText(d==null?"":d.toString());
			ncpV.add(ncp);			
			getContentPane().add(ncp, cc.xy(4+2*i, row));
		}
		
		row +=2;
		
		getContentPane().add(new JLabel("Save NCP settings") , cc.xy(2, row));
		
		for (int i=0; i<nodes.size(); i++) {
			JCheckBox save = new JCheckBox("");
			saveV.add(save);
			getContentPane().add(save, cc.xy(4+2*i, row));
		}
		getContentPane().add(checkAll, cc.xy(4+2*nodes.size(), row));
		
		row +=2;
		
		try {
			jbHelp = new JButton("Help", 
					new ImageIcon(ImageIO.read(DesktopPaneBG.class
							.getResource("/org/af/gMCP/gui/graph/images/questionmark.png"))));
		} catch (IOException e) {
			ErrorHandler.getInstance().makeErrDialog(e.getMessage(), e);
			jbHelp = new JButton("Help!");
		}
		
		HorizontalButtonPane bp = new HorizontalButtonPane(new JButton[] {jbHelp, jbCalc, jbReset, jbSaveClose, jbCancelClose}, false);		
		bp.addActionListener(this);
		
		getContentPane().add(bp, cc.xyw(2, row, 11));
		
		//TODO: config = new File(path, "gMCP-power-settings.xml");
		
		
        pack();
        
        setVisible(true);
		
	}
	
	public void setNCPS(List<Double> ncps) {
		for (int i=0; i<ncps.size(); i++) {			
			Double d = ncps.get(i);
			ncpV.get(i).setText(d==null?"":d.toString());			
		}
	}

	
	
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == jbSaveClose) {
			boolean somethingSelected = false;
			for (JCheckBox jc : saveV) {
				if (jc.isSelected()) somethingSelected = true;
			}
			if (!somethingSelected) {
				int answer = JOptionPane.showConfirmDialog(this, "No NCP to save selected.\n"
						+ "Do you really want to close this window?", 
						"No NCP selected", JOptionPane.YES_NO_OPTION);
				if (answer==JOptionPane.NO_OPTION) return; 
			}
			ncpR.setNCP(getNCPS());
			dispose();
		} else if (e.getSource() == jbCancelClose) {
			dispose();
		} else if (e.getSource() == jbReset) {
			for (JTextField jt : mlV) {
				jt.setText("");
			}
			for (JTextField jt : mpV) {
				jt.setText("");
			}
			setNCPS(ncpR.getOldNCP());
		} else if (e.getSource() == jbCalc) {
			//TODO
		} else if (e.getSource() == checkAll) {
			boolean somethingSelected = false;
			for (JCheckBox jc : saveV) {
				if (jc.isSelected()) somethingSelected = true;
			}
			for (JCheckBox jc : saveV) {
				jc.setSelected(!somethingSelected);
			}
		} else if (e.getSource() == jbHelp) {
			//TODO
		}
	}


	private List<Double> getNCPS() {
		Vector<Double> ncps = new Vector<Double>(); 
		for (int i=0; i<saveV.size(); i++) {
			if (saveV.get(i).isSelected()) {
				Double d = null;
				try {
					d = Double.parseDouble(ncpV.get(i).getText());
				} catch (Exception e) {
					// Nothing to do - really.
				}
				ncps.add(d);
			} else {
				ncps.add(null);
			}
		}
		return ncps;
	} 
	
}
