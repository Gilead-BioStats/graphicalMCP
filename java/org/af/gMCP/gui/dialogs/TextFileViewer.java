package org.af.gMCP.gui.dialogs;

import java.awt.Component;
import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.af.commons.io.FileTools;
import org.af.commons.widgets.RightClickTextMenuListener;
import org.af.gMCP.config.Configuration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class TextFileViewer extends JDialog implements ActionListener {
	
	JTextArea jta;
	JButton save = new JButton("Save");
	 private static final Log logger = LogFactory.getLog(TextFileViewer.class);
	
	public TextFileViewer(JFrame p, File file) {		
		super(p, file.getName());	
		String text;
		try {
			text = FileTools.readFileAsString(file);
		} catch (IOException e) {
			JOptionPane.showMessageDialog(p, "File \""+file.getAbsolutePath()+"\" not found!", "File not found", JOptionPane.ERROR_MESSAGE);
			dispose();
			return;
		}
		setUp(text, null);	
		jta.addMouseListener(new RightClickTextMenuListener(jta));
	}
	
	public TextFileViewer(JFrame p, String title, String text) {
		super(p, title);
		setUp(text, null);
		jta.addMouseListener(new RightClickTextMenuListener(jta));
	}
	
	public TextFileViewer(JFrame p, String title, String text, boolean save) {
		super(p, title);
		if (save) {
		this.save.addActionListener(this);
			setUp(text, this.save, 2, 6, 1);
		} else {
			setUp(text, null);
		}
		jta.addMouseListener(new RightClickTextMenuListener(jta));
	}
	
	public TextFileViewer(JFrame p, String title, String text, String label) {
		super(p, title);
		JTextArea jlabel = new JTextArea(label);
		jlabel.setOpaque(false);
		jlabel.setEditable(false);
		setUp(text, jlabel);
		jta.addMouseListener(new RightClickTextMenuListener(jta));
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==save) {
			File f = null;
			try {
				JFileChooser fc = new JFileChooser(Configuration.getInstance().getClassProperty(this.getClass(), "SaveDirectory"));
				fc.setDialogType(JFileChooser.SAVE_DIALOG);				
				int returnVal = fc.showSaveDialog(this);
				if (returnVal == JFileChooser.APPROVE_OPTION) {			
					f = fc.getSelectedFile();
					Configuration.getInstance().setClassProperty(this.getClass(), "SaveDirectory", f.getParent());
					if (!f.getName().toLowerCase().endsWith(".txt")) {
		            	f = new File(f.getAbsolutePath()+".txt");
		            }
					logger.info("Export to: " + f.getAbsolutePath() + ".");
				} else {
					return;
				}
				BufferedWriter out = new BufferedWriter(new FileWriter(f));
				String output = jta.getText().replaceAll("\n", System.getProperty("line.separator"));
				out.write(output);
				out.close();
			} catch (IOException ioe) {
				JOptionPane.showMessageDialog(this, "Saving to '" + f.getAbsolutePath() + "' failed: " + ioe.getMessage(), "Saving failed.", JOptionPane.ERROR_MESSAGE);
			}
			return;
		}
		dispose();
	}
	
	private void setUp(String text, Component comp) {
		setUp(text, comp, 2, 4, 3);
	}
	
	private void setUp(String text, Component comp, int row, int col, int w) {
		jta = new JTextArea(text);
		jta.setFont(new Font("Monospaced", Font.PLAIN, 12));
		jta.setLineWrap(true);
		jta.setWrapStyleWord(true);
		jta.setMargin(new Insets(4,4,4,4));
		
		String cols = "5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
        String rows = "5dlu, fill:200dlu:grow, 5dlu, pref, 5dlu, pref, 5dlu";
        
        FormLayout layout = new FormLayout(cols, rows);
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();
		
		JScrollPane jsp = new JScrollPane(jta);
		//jsp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		
		getContentPane().add(jsp, cc.xyw(2, 2, 3));
		
		if (comp!=null) {			
			getContentPane().add(comp, cc.xyw(row, col, w));
		}
		
		JButton jb = new JButton("OK");
		jb.addActionListener(this);
		getContentPane().add(jb, cc.xy(4, 6));
		
		pack();
		setSize(800,600);
		setLocationRelativeTo(this.getParent());
		
		setVisible(true);
	}

}
