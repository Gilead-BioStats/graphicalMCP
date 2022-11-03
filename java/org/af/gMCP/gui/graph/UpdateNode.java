package org.af.gMCP.gui.graph;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;

import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.dialogs.ColorChooseDialog;
import org.apache.commons.lang.ArrayUtils;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class UpdateNode extends JDialog implements ActionListener {
	
	List<JTextField> tfList = new Vector<JTextField>();
	JTextField tfname;
	JButton jb = new JButton("Update Node");
	JButton jbDeleteNode = new JButton("Delete Node");
	Node node;
	NetList netzListe;
	GraphView gv;
	JTabbedPane tabbedPane = new JTabbedPane();
	JSpinner spinner;
	JButton jbColor = new JButton("Choose Color");
	
	public UpdateNode(Node node, GraphView gv) {
		super(gv.parent, "Updating Node "+node.getName(), true);
		this.node = node;
		this.gv = gv;
		this.netzListe = gv.getNL();
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
                
        spinner = new JSpinner(new SpinnerNumberModel(node.getRadius(), 1, 100, 1));
        
        FormLayout layout = new FormLayout(cols, rows);
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();
        
        tabbedPane.addTab("Weights & Names", getMainPanel());
        tabbedPane.addTab("Further Attributes", getSubPanel());
        getContentPane().add(tabbedPane, cc.xyw(1, 2, 5));

		int row = 4;
        
        jbDeleteNode.addActionListener(this);
        getContentPane().add(jbDeleteNode, cc.xy(2, row));
                
        jb.addActionListener(this);
        getContentPane().add(jb, cc.xy(4, row));
        
        pack();
        setSize(Math.max(this.getSize().width, 450), this.getSize().height);
        this.setLocation(300, 300);
        setVisible(true);
	}
	
	protected JPanel getMainPanel() {
		JPanel panel = new JPanel();
		
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref, 5dlu";
        
        for (double w : node.getWeight()) {
        	rows += ", pref, 5dlu";
        }
        
        FormLayout layout = new FormLayout(cols, rows);
        panel.setLayout(layout);
        CellConstraints cc = new CellConstraints();
		
        int row = 2;
        
        JTextField tf;
        for (double w : node.getWeight()) {
        	panel.add(new JLabel("Weight for node "+node.getName()), cc.xy(2, row));
        	tf = new JTextField("", 7);
        	tf.setText(""+RControl.getFraction(w));
        	tf.addActionListener(this);
        	tfList.add(tf);
        	panel.add(tf, cc.xy(4, row));
        	row += 2;
        }
        
        panel.add(new JLabel("New name"), cc.xy(2, row));

        tfname = new JTextField();
        tfname.addActionListener(this);
        tfname.setText(node.getName());
        panel.add(tfname, cc.xy(4, row));
        
		return panel;
	}

	JLabel colorLabel = new JLabel("     ");
	
	protected JPanel getSubPanel() {
		JPanel panel = new JPanel();
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref, 5dlu";
        
        FormLayout layout = new FormLayout(cols, rows);
        panel.setLayout(layout);
        CellConstraints cc = new CellConstraints();
        
        int row = 2;
        panel.add(new JLabel("Color:"), cc.xy(2, row));
		
        colorLabel.setOpaque(true);
        colorLabel.setBackground(node.getColor());
        panel.add(colorLabel, cc.xy(4, row));
        
        jbColor.addActionListener(this);
        panel.add(jbColor, cc.xy(6, row));
        
        row += 2;
        
        panel.add(new JLabel("Radius (for all nodes):"), cc.xy(2, row));
		
        panel.add(spinner, cc.xy(4, row));
        
		return panel;
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == jbColor) {
			ColorChooseDialog ccd = new ColorChooseDialog(this);
			node.setColor(ccd.getColor());
			colorLabel.setBackground(node.getColor());
			return;
		}
		if (e.getSource().equals(jbDeleteNode)) {
			netzListe.removeNode(node);
			dispose();		
			return;
		}	
		List<Double> wList = new Vector<Double>();
		for (JTextField tf : tfList) {
			try {			
				double w = RControl.getR().eval(tf.getText().replace(",", ".")).asRNumeric().getData()[0];		
				tf.setBackground(Color.WHITE);	
				wList.add(w);
			} catch (Exception nfe) {		
				tf.setBackground(Color.RED);
				JOptionPane.showMessageDialog(this, "The expression \""+tf.getText()+"\" is not a valid number.", "Not a valid number", JOptionPane.ERROR_MESSAGE);
				return;
			}
		}
		node.setWeight(ArrayUtils.toPrimitive((Double[])wList.toArray(new Double[0])), null);
		gv.getNL().setRadius(Integer.parseInt(spinner.getModel().getValue().toString()));		
		int which = netzListe.whichNode(tfname.getText());
		if (which == -1 || netzListe.getNodes().get(which) == node) {
			gv.renameNode(node, tfname.getText());			
			//TODO Change Name in PView and RDataFrameRef(which, tfname.getText())
			dispose();
		} else {
			JOptionPane.showMessageDialog(this, "There is already a node with name \""+tfname.getText()+"\"", "Node name already in use", JOptionPane.ERROR_MESSAGE);
		}		
		netzListe.repaint();				
	}
}
