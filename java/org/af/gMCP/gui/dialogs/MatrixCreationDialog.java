package org.af.gMCP.gui.dialogs;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Constructor;
import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ListModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.datatable.DataTableModel;
import org.af.gMCP.gui.datatable.RDataFrameRef;
import org.af.gMCP.gui.datatable.SingleDataFramePanel;
import org.af.gMCP.gui.graph.EdgeWeight;
import org.af.gMCP.gui.graph.Node;
import org.af.jhlir.call.RChar;
import org.af.jhlir.call.RErrorException;
import org.af.jhlir.call.RInteger;
import org.af.jhlir.call.RList;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class MatrixCreationDialog extends JDialog implements ActionListener, ChangeListener {
	JButton ok = new JButton("Save matrix to R");

	JFrame parent;
    List<String> names;
    JTextArea jta = new JTextArea();
    public SingleDataFramePanel dfp;
    SingleDataFramePanel dfpDiag;
    SingleDataFramePanel dfpInterCor;
    SingleDataFramePanel dfpIntraCor;
    JTextField tfname = new JTextField();
    JList hypotheses;
    JLabel warning = new JLabel();
    protected static Log logger = LogFactory.getLog(MatrixCreationDialog.class);
    // Was some matrix created?
    public boolean created = false;
    
    JTabbedPane tabbedPane = new JTabbedPane(); 
    
    public static Vector<String> getNames(Vector<Node> nodes) {
    	Vector<String> v = new Vector<String>();
    	for (Node n: nodes) {
    		v.add(n.getName());
    	}
    	return v;
    }
    
    /**
     * Constructor
     * @param matrix String that specifies R object to be loaded.
     */
	public MatrixCreationDialog(String[] matrix, String mname, String[] names) {
		this(null, Arrays.asList(matrix), mname, Arrays.asList(names));
	}
    
    /**
     * Constructor
     * @param matrix String that specifies R object to be loaded.
     */
	public MatrixCreationDialog(JFrame parent, List<String> matrix, String mname, List<String> names) {
		super(parent, "Specify correlation matrix", true);
		setLocationRelativeTo(parent);
		this.names = new Vector<String>();
		for (String name : names) {
			this.names.add(name);
		}
		this.parent = parent;
		
		//System.out.println(matrix.getClass().getName());
		
		RDataFrameRef df = new RDataFrameRef();
		for (String n: names) {
			df.addRowCol(n);
			df.setValue(df.getColumnCount()-1, df.getColumnCount()-1, new EdgeWeight(1));
		}		
		dfp = new SingleDataFramePanel(df);
		dfp.getTable().getModel().diagEditable = false;
		dfp.getTable().getModel().setCheckRowSum(false);
		dfp.getTable().getModel().checkCorMat();
		
		setUpTabbedPane();
		getPossibleCorrelations();
		
		jta.setText("");
		warning.setForeground(Color.RED);

        ok.addActionListener(this);

        String cols = "5dlu, fill:min:grow, 5dlu, fill:min:grow, 5dlu";
        String rows = "5dlu, fill:min:grow, 5dlu, pref, 5dlu";
        
        FormLayout layout = new FormLayout(cols, rows);
        layout.setColumnGroups(new int[][]{ {2, 4} });
        
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 2;
        
		getContentPane().add(tabbedPane, cc.xy(2, row));
        
		getContentPane().add(new JScrollPane(dfp), cc.xy(4, row));
		
		row +=2;
		
		getContentPane().add(warning, cc.xy(2, row));
		getContentPane().add(ok, cc.xy(4, row));
		
		if (matrix ==null) {
			if (mname != null) {
				DataTableModel m = dfp.getTable().getModel();
				int n = m.getColumnCount();			
				String[] result = RControl.getR().eval("(function(x){ x[is.na(x)] <- \"NA\"; return(x)})("+mname+")").asRChar().getData();
				for (int i=0; i<n; i++) {
					for (int j=0; j<n; j++) {
						m.setValueAt(new EdgeWeight(result[i*n+j]), i, j);
					}
				}
			}
		} else {
			DataTableModel m = dfp.getTable().getModel();
			int n = m.getColumnCount();			
			for (int i=0; i<n; i++) {
				for (int j=0; j<n; j++) {
					m.setValueAt(new EdgeWeight(matrix.get(i*n+j)), i, j);
				}
			}
		}
		tfname.setText(mname);
		
        pack();
        Dimension d = getSize();        
        this.setSize((int)d.getWidth(), Math.min((int)(d.getHeight()*1.7),(int)(Toolkit.getDefaultToolkit().getScreenSize().getHeight()-80)));
        setLocationRelativeTo(parent);
        setVisible(true);
	}

	private void setUpTabbedPane() {
		tabbedPane.add("General", getSortPane());
		tabbedPane.add("Block Diagonal", getBlockPane());
		tabbedPane.add("Treatments and Endpoints", getTEPane());		
	}
	
	JButton reorder = new JButton("Apply reordering");
	JButton toggleNA = new JButton("Change 0 in matrix to NA");
	JButton resetDiag = new JButton("Reset to identity matrix");

	private JPanel getSortPane() {
		JPanel panel = new JPanel();		
		
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, fill:min:grow, 5dlu, pref, 5dlu, pref, 5dlu";
		
        panel.setLayout(new FormLayout(cols, rows));
        CellConstraints cc = new CellConstraints();
		
		int row = 2;
		
		tfname.setText("corMat");
		
		panel.add(new JLabel("Save matrix as:"), cc.xy(2, row));
        panel.add(tfname, cc.xy(4, row));

        row +=2;
        
        resetDiag.addActionListener(this);
        panel.add(resetDiag, cc.xy(4, row));
       
        row +=2;
        
        toggleNA.addActionListener(this);
        panel.add(toggleNA, cc.xy(4, row));
        
        row +=2;
        
        panel.add(new JLabel("You can reorder the hypotheses by drag'n'drop:"), cc.xyw(2, row, 3));
        
        row +=2;
        
        DefaultListModel lm = new DefaultListModel();
        for (String n: names) {
			lm.addElement(n);
		}
        
		try {
        	Class cls = Class.forName("org.af.commons.widgets.JListDnD");
        	Constructor ct = cls.getConstructor(new Class[] {ListModel.class});
        	hypotheses = (JList) ct.newInstance(lm);        	
        	panel.add(new JScrollPane(hypotheses), cc.xyw(2, row, 3));
        } catch (Exception e) {
        	// Java 5 will throw an exception.
        	// In this case we set hypotheses to an ordinary JList.
        	logger.warn(e);        
            hypotheses = new JList(lm);
            panel.add(new JScrollPane(hypotheses), cc.xyw(2, row, 3));
            row +=2;
            panel.add(new JLabel("Reordering does currently not work for you due to Java 5"), cc.xyw(2, row, 3));
        }
        
        row +=2;
        
        reorder.addActionListener(this);
        panel.add(reorder, cc.xy(4, row));
        
        row +=2;
        
        
        		
		return panel;
	}
	
	JSpinner spinnerN;
	JSpinner spinnerN2;
	JButton jbAdd;
	JLabel jlBlock = new JLabel();
	
	private JPanel getBlockPane() {
		JPanel panel = new JPanel();
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, fill:min:grow, 5dlu, pref, 5dlu";
        
        panel.setLayout(new FormLayout(cols, rows));
        CellConstraints cc = new CellConstraints();
		
		int row = 2;
		
		spinnerN = new JSpinner(new SpinnerNumberModel(2, 1, names.size(), 1));    	
    	spinnerN.addChangeListener(this);
    	
    	panel.add(new JLabel("Insert matrix of size:"), cc.xy(2, row));
        panel.add(spinnerN, cc.xy(4, row));
        
        row +=2;
        
        spinnerN2 = new JSpinner(new SpinnerNumberModel(1, 1, names.size()-1, 1));    	
    	spinnerN2.addChangeListener(this);
    	
    	panel.add(new JLabel("Insert matrix at position:"), cc.xy(2, row));
        panel.add(spinnerN2, cc.xy(4, row));        
    	
		row += 2;
		
		panel.add(new JLabel("Use standard design:"), cc.xy(2, row));
        panel.add(jcbCorString2, cc.xy(4, row));
        jcbCorString2.addActionListener(this);
        
    	row +=2;
    	
    	panel.add(jlBlock, cc.xyw(2, row, 3));  
    	
    	row +=2;
    	
    	RDataFrameRef df = new RDataFrameRef();
		for (int i=0; i<2; i++) {
			df.addRowCol(names.get(i));
			df.setValue(df.getColumnCount()-1, df.getColumnCount()-1, new EdgeWeight(1));
		}		
		dfpDiag = new SingleDataFramePanel(df);
		dfpDiag.getTable().getModel().setCheckRowSum(false);
		
		panel.add(new JScrollPane(dfpDiag), cc.xyw(2, row, 3));
		
        row +=2;
        
        jbAdd = new JButton("Add matrix on diagonal");
        jbAdd.addActionListener(this);
        panel.add(jbAdd, cc.xyw(2, row, 3));
		
		return panel;
	}
	
	JButton applyTE = new JButton("Calculate overall correlation");
	
	private JPanel getTEPane() {
		JPanel panel = new JPanel();
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, fill:min:grow, 5dlu, fill:min:grow, 5dlu, pref, 5dlu";
        
        panel.setLayout(new FormLayout(cols, rows));
        CellConstraints cc = new CellConstraints();
		
		int row = 2;
		
		panel.add(getTEPane1(), cc.xyw(2, row, 3));
		
		row += 2;
		
		panel.add(getTEPane2(), cc.xyw(2, row, 3));
		
		row += 2;
		
		applyTE.addActionListener(this);
		panel.add(applyTE, cc.xyw(2, row, 3));
		
		return panel;
	}
	
	JSpinner spinnerNT;
	JSpinner spinnerNE;	
	
	private JPanel getTEPane1() {
		JPanel panel = new JPanel();
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref, 5dlu, fill:min:grow, 5dlu";
        
        panel.setLayout(new FormLayout(cols, rows));
        CellConstraints cc = new CellConstraints();
		
		int row = 2;
		
		spinnerNT = new JSpinner(new SpinnerNumberModel(Math.max(2,names.size()/2), 1, names.size(), 1));    	
    	spinnerNT.addChangeListener(this);
    	
    	panel.add(new JLabel("Number of Treatment Comparisons:"), cc.xy(2, row));
        panel.add(spinnerNT, cc.xy(4, row));
		
		row += 2;
		
		panel.add(new JLabel("Use standard design:"), cc.xy(2, row));
        panel.add(jcbCorString, cc.xy(4, row));
        jcbCorString.addActionListener(this);
		
		row += 2;
		
		RDataFrameRef df = new RDataFrameRef();
		for (int i=0; i<Math.max(2,names.size()/2); i++) {
			df.addRowCol("T"+(i+1));
			df.setValue(df.getColumnCount()-1, df.getColumnCount()-1, new EdgeWeight(1));
		}		
		dfpIntraCor = new SingleDataFramePanel(df);
		dfpIntraCor.getTable().getModel().setCheckRowSum(false);
		
		panel.add(new JScrollPane(dfpIntraCor), cc.xyw(2, row, 3));
		
		TitledBorder title = BorderFactory.createTitledBorder("Treatment correlation.");
		panel.setBorder(title);	
		
		return panel;
	}
	
	private JPanel getTEPane2() {
		JPanel panel = new JPanel();
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, fill:min:grow, 5dlu";
        
        panel.setLayout(new FormLayout(cols, rows));
        CellConstraints cc = new CellConstraints();
		
		int row = 2;
		
		spinnerNE = new JSpinner(new SpinnerNumberModel(Math.max(2,names.size()/2), 1, names.size(), 1));    	
    	spinnerNE.addChangeListener(this);
    	
    	panel.add(new JLabel("Number of Endpoints:"), cc.xy(2, row));
        panel.add(spinnerNE, cc.xy(4, row));
		
		row += 2;
		
		RDataFrameRef df = new RDataFrameRef();
		for (int i=0; i<Math.max(2,names.size()/2); i++) {
			df.addRowCol("E"+(i+1));
			df.setValue(df.getColumnCount()-1, df.getColumnCount()-1, new EdgeWeight(1));
		}		
		dfpInterCor = new SingleDataFramePanel(df);
		dfpInterCor.getTable().getModel().setCheckRowSum(false);
		
		panel.add(new JScrollPane(dfpInterCor), cc.xyw(2, row, 3));
		
		TitledBorder title = BorderFactory.createTitledBorder("Correlation between endpoints");
		panel.setBorder(title);	
		
		return panel;
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==ok) {
			String name = RControl.getR().eval("make.names(\""+tfname.getText()+"\")").asRChar().getData()[0];
			RControl.getR().evalInGlobalEnv(name+" <- "+dfp.getTable().getModel().getDataFrame().getRMatrix());
			created = true;
			dispose();
		} else if (e.getSource()==jbAdd) {
			int k = Integer.parseInt(spinnerN2.getModel().getValue().toString());
			int n = Integer.parseInt(spinnerN.getModel().getValue().toString());
			DataTableModel m = dfpDiag.getTable().getModel();
			DataTableModel m2 = dfp.getTable().getModel();
			for (int i=0; i<n; i++) {
				for (int j=0; j<n; j++) {
					m2.setValueAt(m.getValueAt(i, j), i+k-1, j+k-1);
				}
			}
		} else if (e.getSource()==toggleNA) {
			DataTableModel m2 = dfp.getTable().getModel();
			int n = m2.getColumnCount();
			for (int i=0; i<n; i++) {
				for (int j=0; j<n; j++) {
					if (m2.getValueAt(i, j).getWeight(null)==0) {
						m2.setValueAt(new EdgeWeight("NA"), i, j);
					}
				}
			}
		} else if (e.getSource()==reorder) {
			names.clear();
			for (int i=0; i<hypotheses.getModel().getSize(); i++) {
				names.add((String) hypotheses.getModel().getElementAt(i));
			}			
			createMDiag();
			List<String> names2 = new Vector<String>();
			for (String n : names) {
				names2.add(n);
			}
			dfp.getTable().getModel().getDataFrame().setNames(names2);
			dfp.getTable().update();
		} else if (e.getSource()==resetDiag) {
			DataTableModel m2 = dfp.getTable().getModel();
			int n = m2.getColumnCount();
			for (int i=0; i<n; i++) {
				for (int j=0; j<n; j++) {
					if (i==j) {
						m2.setValueAt(new EdgeWeight(1), i, j);
					} else {
						m2.setValueAt(new EdgeWeight(0), i, j);
					}
				}
			}
		} else if (e.getSource()==applyTE) {
			DataTableModel m2 = dfp.getTable().getModel();
			int n = m2.getColumnCount();
			int a = dfpIntraCor.getTable().getRowCount();
			int b = dfpInterCor.getTable().getRowCount();
			if (a*b!=n) {
				JOptionPane.showMessageDialog(this, "Wrong dimensions: "+a+"+"+b+"!="+n, "Wrong dimension", JOptionPane.ERROR_MESSAGE);
				return;
			}
			double[] result = RControl.getR().eval("as.numeric(kronecker("
					+ dfpInterCor.getTable().getRMatrix()
					+","					
					+ dfpIntraCor.getTable().getRMatrix()
					+"))").asRNumeric().getData();
			for (int i=0; i<n; i++) {
				for (int j=0; j<n; j++) {
					m2.setValueAt(new EdgeWeight(result[i*n+j]), i, j);
				}
			}
		} else if (e.getSource()==jcbCorString2) {
			if (jcbCorString2.getSelectedItem()==null || jcbCorString2.getSelectedItem().toString().equals(NO_SD)) return;
			DataTableModel m = dfpDiag.getTable().getModel();
			int n = Integer.parseInt(spinnerN.getModel().getValue().toString());
			String s = jcbCorString2.getSelectedItem().toString();
			setMatrix(m, s, n);
		} else if (e.getSource()==jcbCorString) {	
			if (jcbCorString.getSelectedItem()==null || jcbCorString.getSelectedItem().toString().equals(NO_SD)) return;
			DataTableModel m = dfpIntraCor.getTable().getModel();
			int n = Integer.parseInt(spinnerNT.getModel().getValue().toString());
			String s = jcbCorString.getSelectedItem().toString();
			setMatrix(m, s, n);
		}
		try {
			warning.setText(RControl.getR().eval("gMCP:::checkPSD("+dfp.getTable().getRMatrix()+")").asRChar().getData()[0]);
		} catch (RErrorException error) {
			warning.setText("Matrix could not be evaluated! Are all entries numeric?");
		}
	}

	private void setMatrix(DataTableModel m, String s, int n) {
		String design = s.substring(0, s.indexOf(" "));
		String groups = s.substring(s.indexOf("(")+1, s.indexOf("groups")-1);				
		GroupDialog gd = new GroupDialog(parent, Integer.parseInt(groups));
		String command = "gMCP:::getCorrMat(n="+gd.getGroups()+", type =\""+ design+"\")";
		double[] m2 = RControl.getR().eval(command).asRNumeric().getData();
		for (int i=0; i<n; i++) {
			for (int j=0; j<n; j++) {
				m.setValueAt(new EdgeWeight(m2[i*n+j]), i, j);
			}
		}		
	}

	public void stateChanged(ChangeEvent e) {
		if (e.getSource()==spinnerN || e.getSource()==spinnerN2) {
			createMDiag();
			getPossibleCorrelations();
		} else if (e.getSource()==spinnerNT) {	
			createMT();			
			getPossibleCorrelations();
		} else if (e.getSource()==spinnerNE) {
			createME();			
			getPossibleCorrelations();
		}
		
	}
	
	private void createMT() {
		int n = Integer.parseInt(spinnerNT.getModel().getValue().toString());
		DataTableModel m = dfpIntraCor.getTable().getModel();
		m.removeAll();
		for (int i=0; i<n; i++) {
			m.addRowCol("T"+(i+1));
			m.setValueAt(new EdgeWeight(1), m.getColumnCount()-1, m.getColumnCount()-1);
		}
	}

	private void createME() {
		int n = Integer.parseInt(spinnerNE.getModel().getValue().toString());
		DataTableModel m = dfpInterCor.getTable().getModel();
		m.removeAll();
		for (int i=0; i<n; i++) {
			m.addRowCol("E"+(i+1));
			m.setValueAt(new EdgeWeight(1), m.getColumnCount()-1, m.getColumnCount()-1);
		}
	}

	private void createMDiag() {
		int n = Integer.parseInt(spinnerN.getModel().getValue().toString());
		int j = Integer.parseInt(spinnerN2.getModel().getValue().toString());
		DataTableModel m = dfpDiag.getTable().getModel();
		m.removeAll();
		if (n+j-1>names.size()) {
			JOptionPane.showMessageDialog(parent, "The selected values "+n+"+"+j+" exceed the number of nodes+1.", "Impossible parameter combination", JOptionPane.ERROR_MESSAGE);
			return;
		}
		for (int i=j-1; i<j-1+n; i++) {
			m.addRowCol(names.get(i));
			m.setValueAt(new EdgeWeight(1), m.getColumnCount()-1, m.getColumnCount()-1);
		}
	}

	protected JComboBox jcbCorString = new JComboBox(new String[] {NO_SD});
	protected JComboBox jcbCorString2 = new JComboBox(new String[] {NO_SD});
	
	/* Note: the following string must have a certain length, otherwise 
	 * e.g. "UmbrellaWilliams (3 groups)" will cause layout problems. 
	 */
	final static String NO_SD = "User defined design (edit the matrix)";
	
	private void getPossibleCorrelations() {
		jcbCorString.removeAllItems();
		jcbCorString2.removeAllItems();
		int n = Integer.parseInt(spinnerNT.getModel().getValue().toString());
		int n2 = Integer.parseInt(spinnerN.getModel().getValue().toString());
		if (n!=0) {
			RList list = RControl.getR().eval("gMCP:::getAvailableStandardDesigns("+n+")").asRList();
			RChar designs = list.get(0).asRChar();
			RInteger groups = list.get(1).asRInteger();
			jcbCorString.addItem(NO_SD);
			for (int i=0; i<designs.getLength(); i++) {
				jcbCorString.addItem(designs.getData()[i] + " ("+ groups.getData()[i]+" groups)"); 
			}		
		}
		if (n2!=0) {
			RList list = RControl.getR().eval("gMCP:::getAvailableStandardDesigns("+n2+")").asRList();
			RChar designs = list.get(0).asRChar();
			RInteger groups = list.get(1).asRInteger();
			jcbCorString2.addItem(NO_SD);
			for (int i=0; i<designs.getLength(); i++) {
				jcbCorString2.addItem(designs.getData()[i] + " ("+ groups.getData()[i]+" groups)"); 
			}		
		}
	}
	
}