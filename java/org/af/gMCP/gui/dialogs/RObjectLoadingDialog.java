package org.af.gMCP.gui.dialogs;

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.MenuBarMGraph;
import org.af.gMCP.gui.RControl;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class RObjectLoadingDialog extends JDialog implements ActionListener, ListSelectionListener, MouseListener {
	JButton ok = new JButton("Load");
	JButton cancel = new JButton("Cancel");

    CreateGraphGUI parent;    
    JList jlMatrices;
    JList jlGraphs;
    String[] matrices;
    String[] graphs;
    JTextArea jtInfo = new JTextArea(12, 40);
    
	public RObjectLoadingDialog(CreateGraphGUI parent) {
		super(parent, "Select an R object to load", true);
		this.parent = parent;
		jtInfo.setEditable(false);
		jtInfo.setFont(new Font("Monospaced", Font.PLAIN, 10));
		
		matrices = RControl.getR().eval("gMCP:::getAllQuadraticMatrices()").asRChar().getData();
		graphs = RControl.getR().eval("gMCP:::getAllGraphs()").asRChar().getData();		
				
		jlMatrices = new JList(matrices);
		jlMatrices.addListSelectionListener(this);
		jlMatrices.addMouseListener(this);
		jlMatrices.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		jlGraphs = new JList(graphs);
		jlGraphs.addListSelectionListener(this);
		jlGraphs.addMouseListener(this);
		jlGraphs.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		
		if (matrices.length==1 && matrices[0].equals("No quadratic matrices found.")) {
			jlMatrices.setEnabled(false);
		}

		if (graphs.length==1 && graphs[0].equals("No graphMCP objects found.")) {
			jlGraphs.setEnabled(false);
		}

		if (!jlGraphs.isEnabled() && !jlMatrices.isEnabled()) {
			JOptionPane.showMessageDialog(this, "No applicable R objects (quadratic matrices or graphMCP objects) found.", "No applicable R objects found.", JOptionPane.INFORMATION_MESSAGE);
			return;
		}		
		
        String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu, pref, 5dlu";
        
        FormLayout layout = new FormLayout(cols, rows);
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 2;

        getContentPane().add(new JLabel("Graph objects"), cc.xy(2, row));
        getContentPane().add(new JLabel("Quadratic matrices"), cc.xy(4, row));
        getContentPane().add(new JLabel("Object info"), cc.xy(6, row));

        row += 2;
        
        getContentPane().add(new JScrollPane(jlGraphs), cc.xy(2, row));
        getContentPane().add(new JScrollPane(jlMatrices), cc.xy(4, row));
        getContentPane().add(new JScrollPane(jtInfo), cc.xy(6, row));

        row += 2;
                        
        getContentPane().add(ok, cc.xy(4, row));
        getContentPane().add(cancel, cc.xy(6, row));
        ok.addActionListener(this);        
        cancel.addActionListener(this);

        pack();
        setSize(760,500);
		setLocationRelativeTo(parent);
        setVisible(true);
	}

	public void actionPerformed(ActionEvent e) {
		if (e != null && e.getSource()==ok) {
			if (jlMatrices.getSelectedIndex() == -1 && jlGraphs.getSelectedIndex() == -1) {
				JOptionPane.showMessageDialog(this, "Please select an R object for loading from one of the lists.", "Please select an object.", JOptionPane.INFORMATION_MESSAGE);
				return;
			}
			String object;
			if (jlMatrices.getSelectedIndex() != -1) {
				object = jlMatrices.getSelectedValue().toString();
			} else {
				object = jlGraphs.getSelectedValue().toString();
			}   
			((MenuBarMGraph)parent.getJMenuBar()).loadGraph(object, true);
			Configuration.getInstance().getGeneralConfig().addGraph("R Object: "+object);
		}
		dispose();
	}

	public void valueChanged(ListSelectionEvent e) {
		if (e.getSource()==jlMatrices && jlMatrices.getSelectedIndex() != -1) {
			jlGraphs.removeSelectionInterval(0, graphs.length-1);
			String info = RControl.getR().eval("gMCP:::getObjectInfo("+jlMatrices.getSelectedValue()+")", true).asRChar().getData()[0];
			jtInfo.setText(info);
		} else if (e.getSource()==jlGraphs && jlGraphs.getSelectedIndex() != -1) {
			jlMatrices.removeSelectionInterval(0, matrices.length-1);
			String info = RControl.getR().eval("gMCP:::getObjectInfo("+jlGraphs.getSelectedValue()+")", true).asRChar().getData()[0];
			jtInfo.setText(info);
		}
		jtInfo.setCaretPosition(0);		
	}

	 public void mouseClicked(MouseEvent e) {
        if (e.getClickCount() == 2) {
            int i = jlMatrices.locationToIndex(e.getPoint());
            if (jlMatrices.getCellBounds(i, i).contains(e.getPoint())) {            	
            	actionPerformed(null);
            }
            i = jlGraphs.locationToIndex(e.getPoint());
            if (jlGraphs.getCellBounds(i, i).contains(e.getPoint())) {            	
            	actionPerformed(null);
            }
         }
    }

	public void mouseEntered(MouseEvent e) {}
	public void mouseExited(MouseEvent e) {}
	public void mousePressed(MouseEvent e) {}
	public void mouseReleased(MouseEvent e) {}	
	
}