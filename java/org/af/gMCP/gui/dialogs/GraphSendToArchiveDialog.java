package org.af.gMCP.gui.dialogs;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Random;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import org.af.commons.errorhandling.HTTPPoster;
import org.af.commons.tools.StringTools;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.ErrorDialogGMCP;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.graph.GraphView;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class GraphSendToArchiveDialog extends JDialog implements ActionListener {
	JButton ok = new JButton("Submit");
	JButton cancel = new JButton("Cancel");

    CreateGraphGUI parent;
    GraphView control;
    JTextArea jtDescr = new JTextArea(6, 40);
    JTextArea jtLiterature = new JTextArea(6, 40);
    JTextField jtTitle = new JTextField();
    JTextField jtName = new JTextField();
    JTextField jtEmail = new JTextField();
    
    Hashtable<String,String> table = new Hashtable<String,String>();
    Hashtable<String,File> files = new Hashtable<String,File>();
    String urlString = "http://algorithm-forge.com/gMCP/" +
    		"submitGraph.php";
    JPanel canvas;
    
    
    final static int MAXWIDTH = 500;
    final static int MAXHEIGHT = 300;
    
	public GraphSendToArchiveDialog(CreateGraphGUI parent, GraphView control) {
		super(parent, "Submit your graph", true);
		this.parent = parent;
		this.control = control;

		getContentPane().setLayout(new GridBagLayout());
		
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=10; c.ipady=10;
		c.weightx=1; c.weighty=0;

		BufferedImage img = control.nl.getImage(1d, false);
		
		if (MAXWIDTH/img.getWidth()<1 || MAXHEIGHT/img.getHeight()<1) {
			if (MAXWIDTH/img.getWidth()>MAXHEIGHT/img.getHeight()) {
				canvas = new ImagePanel(img.getScaledInstance(-1, MAXHEIGHT, Image.SCALE_SMOOTH));
			} else {
				canvas = new ImagePanel(img.getScaledInstance(MAXWIDTH, -1, Image.SCALE_SMOOTH));
			}
		} else {
			canvas = new ImagePanel(img);
		}
		
		                       
		getContentPane().add(canvas, c);
		
		c.gridy++; c.weighty=1;
		
		getContentPane().add(getInfoPanel(), c);
		
		c.gridy++; c.weighty=0;
		
        getContentPane().add(ok, c);
        ok.addActionListener(this);        

        pack();
        //setSize(500,800);
		setLocationRelativeTo(parent);
        setVisible(true);
	}
	
	public JPanel getInfoPanel() {
		JPanel panel = new JPanel();
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref,"+
        	//"5dlu, pref, 5dlu, pref,"+
        	"5dlu, pref, 5dlu, fill:pref:grow,"+
        	//"5dlu, pref, 5dlu, fill:pref:grow,"+
        	"5dlu, pref, 5dlu, pref,"+
        	"5dlu, pref, 5dlu, pref";
        
        FormLayout layout = new FormLayout(cols, rows);
        panel.setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 2;

        panel.add(new JLabel("Title"), cc.xy(2, row));
        row += 2;
        panel.add(new JTextField(), cc.xy(2, row));
        row += 2;        
        panel.add(new JLabel("Description and References"), cc.xy(2, row));
        row += 2;
        panel.add(new JScrollPane(jtDescr), cc.xy(2, row));
        row += 2;       
        /*panel.add(new JLabel("Literature (optional)"), cc.xy(2, row));
        row += 2;
        panel.add(new JScrollPane(jtLiterature), cc.xy(2, row));
        row += 2;*/        
        panel.add(new JLabel("Your name"), cc.xy(2, row));
        row += 2;
        panel.add(jtName, cc.xy(2, row));
        row += 2;        
        panel.add(new JLabel("Optional email (will not be published - just for feedback from us)"), cc.xy(2, row));
        row += 2;
        panel.add(jtEmail, cc.xy(2, row));
		return panel;
	}

	public void actionPerformed(ActionEvent e) {
		try {
			table.put("Description", jtDescr.getText());
			table.put("Title", jtTitle.getText());
			table.put("Name", jtName.getText());
			table.put("Email", jtEmail.getText());
			table.put("Subject", "New graph from "+jtName.getText());			
			File file = new File(Configuration.getInstance().getGeneralConfig().getTempDir(), "Graph"+(new Random()).nextInt()+".jpg");
			control.saveGraphImage(file, true, true, true);
			files.put("Image", file);
			files.put("Graph", ErrorDialogGMCP.makeLogFile("graph.txt", StringTools.collapseStringArray(RControl.getR().eval("gMCP:::getDebugInfo()").asRChar().getData())));
			(new HTTPPoster()).post(urlString, table, files);
		} catch (IOException e1) {
			new ErrorDialogGMCP("An error occured submitting the graph.", e1, false).showDialog();
		}
		dispose();
	}

}