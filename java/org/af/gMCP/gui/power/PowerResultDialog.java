package org.af.gMCP.gui.power;

import java.awt.Font;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Constructor;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.table.DefaultTableModel;

import org.af.commons.widgets.RightClickTextMenuListener;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.jhlir.call.RDataFrame;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class PowerResultDialog extends JDialog implements ActionListener {
	
	public String NEW_CELL = "\t";
	public String NEW_LINE = "\n";    
	
	JButton jbCopyTable = new JButton("Copy Table to Clipboard");
	JButton jbCopyText = new JButton("Copy Text to Clipboard");
	JButton jbOk = new JButton("Ok");
	Object[][] data;
	String[] colnames;
	JTextArea jta = new JTextArea();
	
	
	
	public PowerResultDialog(CreateGraphGUI parent, String title, RDataFrame result, String[] colnames, String command, Class c) {
		super(parent, title);
		this.colnames = colnames;		

		data = new Object[result.getRowCount()][result.getColumnCount()];
		for (int i=0; i<result.getRowCount(); i++) {
			for (int j=0; j<result.getColumnCount(); j++) {
				data[i][j] = result.get(i, j);
			}
		}
		Constructor<?> ct;
		Object object = new DefaultTableModel(data, colnames);
		try {
			ct = c.getConstructor(Object[][].class, String[].class);
			object = ct.newInstance(data, colnames);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		
		JTable jt = new JTable((DefaultTableModel)object);
		jt.setAutoCreateRowSorter(true);
		
		/*for (int i=0; i < jt.getColumnCount(); i++) {
			jt.getColumnModel().getColumn(i).setMinWidth(110);

		} */  
		
		jt.setAutoResizeMode( JTable.AUTO_RESIZE_OFF );
		
		String cols = "5dlu, fill:pref:grow, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";
        String rows = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu, pref, 5dlu";
        
        FormLayout layout = new FormLayout(cols, rows);
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();		
        
		JScrollPane jsp = new JScrollPane(jt);
		
		int row = 2;
		
		//getContentPane().add(jsp, cc.xyw(2, row, 5));
		
		jta.setText(command);
		jta.setMargin(new Insets(4,4,4,4));
		jta.setFont(new Font("Monospaced", Font.PLAIN, 12));
		jta.setLineWrap(false);
		//jta.setWrapStyleWord(true);
		
		jta.addMouseListener(new RightClickTextMenuListener(jta));
		//getContentPane().add(new JScrollPane(jta), cc.xyw(2, row, 3));
		JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
				jsp, new JScrollPane(jta));
		
		getContentPane().add(splitPane, cc.xywh(2, row, 7, 3));		
		
		row += 2;
		row += 2;		
		
		jbCopyTable.addActionListener(this);
		jbCopyText.addActionListener(this);
		jbOk.addActionListener(this);
		getContentPane().add(jbCopyTable, cc.xy(4, row));
		getContentPane().add(jbCopyText, cc.xy(6, row));
		getContentPane().add(jbOk, cc.xy(8, row));
		
		pack();
		//Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		//setSize(screenSize.width-200, getHeight());
		setLocationRelativeTo(parent);
		
		setVisible(true);
		
	}

	public void copyTableToClipboard() {
		StringBuffer s = new StringBuffer();

		for (int j=0; j<colnames.length; j++) {
			s.append(colnames[j]+NEW_CELL);
		}
		s.append(NEW_LINE);

		for (int i=0; i<data.length; i++) {
			for (int j=0; j<data[0].length; j++) {
				s.append(data[i][j].toString()+NEW_CELL);
			}
			s.append(NEW_LINE);
		}
		StringSelection stringTable  = new StringSelection(s.toString()); 
		Toolkit.getDefaultToolkit().getSystemClipboard().setContents(stringTable, stringTable);
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==jbOk) {
			dispose();
		}
		if (e.getSource()==jbCopyTable) {
			copyTableToClipboard();
		}
		if (e.getSource()==jbCopyText) {
			StringSelection stringText  = new StringSelection(jta.getText()); 
			Toolkit.getDefaultToolkit().getSystemClipboard().setContents(stringText, stringText);
		}
	}
	
	
}
