package org.af.gMCP.gui.dialogs;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.filechooser.FileFilter;

import org.af.commons.tools.OSTools;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.graph.GraphView;
import org.af.gMCP.gui.graph.Node;
import org.mutoss.gui.TransferableImage;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class ImageExportDialog extends JDialog implements ActionListener, ChangeListener {

	JButton ok = new JButton("Save file");
	JTextField tfFile = new JTextField();
	JCheckBox cbColored = new JCheckBox();
	JCheckBox cbTransparent = new JCheckBox();
	JButton selectFile = new JButton("Choose file:");
	ImagePanel ip;
	JSpinner spinnerRadius;
	JCheckBox markEpsilon;	
	JCheckBox drawHypNames = new JCheckBox("Draw names of hypotheses");
	JCheckBox drawHypWeights = new JCheckBox("Draw weights of hypotheses");
	JCheckBox drawEdgeWeights = new JCheckBox("Draw weights of edges");
	JButton copyToClipboard = new JButton("Copy to clipboard");
	
	CreateGraphGUI parent;
	GraphView control;
	
	public ImageExportDialog(CreateGraphGUI parent, boolean saveToFile) {
		super(parent, "Export Image", true);
		setLocationRelativeTo(parent);
		this.parent = parent;
		control = parent.getGraphView();		
		
        String cols = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
        String rows = "5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
        if (saveToFile) {
        	rows += ", pref, 5dlu";
        }
               
        FormLayout layout = new FormLayout(cols, rows);
        getContentPane().setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 2;
        
        getContentPane().add(getOptionsPanel(), cc.xy(6, row));
        
        ip = new ImagePanel(getImage(), 0, 0, 50, 50);        
        getContentPane().add(ip, cc.xyw(2, row, 3));  
    	        
        row += 2;
        
        if (saveToFile) {

        	selectFile.addActionListener(this);
        	tfFile.setText(Configuration.getInstance().getClassProperty(this.getClass(), "LastImage", ""));
        	getContentPane().add(selectFile, cc.xy(2, row));
        	getContentPane().add(tfFile, cc.xyw(4, row, 3));        

        	row += 2;

        	getContentPane().add(ok, cc.xy(6, row));        
        	ok.addActionListener(this);

        } else {
        	
        	getContentPane().add(copyToClipboard, cc.xy(6, row));        
        	copyToClipboard.addActionListener(this);
        }
        
        pack();
        setVisible(true);
	}

	// TODO Layer
	public JPanel getOptionsPanel() {
		JPanel panel = new JPanel();
        String cols = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";    
               
        FormLayout layout = new FormLayout(cols, rows);
        panel.setLayout(layout);
        CellConstraints cc = new CellConstraints();
        
        int row = 2;
        
        cbColored = new JCheckBox("Colored graph");
        cbColored.addActionListener(this);
        cbColored.setSelected(Configuration.getInstance().getGeneralConfig().getColoredImages());
        panel.add(cbColored, cc.xyw(2, row, 3));        
        
        row += 2;
        
        cbTransparent = new JCheckBox("Transparent background (recommended)");
        cbTransparent.addActionListener(this);
        cbTransparent.setSelected(Configuration.getInstance().getGeneralConfig().exportTransparent());
        panel.add(cbTransparent, cc.xyw(2, row, 3));     
        
        row += 2;
        
        markEpsilon = new JCheckBox("Show epsilon edges as dashed lines.");
        markEpsilon.setSelected(Configuration.getInstance().getGeneralConfig().markEpsilon());
        markEpsilon.setToolTipText("<html>" +
        		"You can set whether epsilon edges should<br>" +
        		"been shown as dashed or solid lines." +
        		"</html>");
        

        row += 2;
        
        drawHypNames.addActionListener(this);
        drawHypNames.setSelected(Configuration.getInstance().getGeneralConfig().exportTransparent());
        panel.add(drawHypNames, cc.xyw(2, row, 3));     
        
        row += 2;
        
        drawHypWeights.addActionListener(this);
        drawHypWeights.setSelected(Configuration.getInstance().getGeneralConfig().exportTransparent());
        panel.add(drawHypWeights, cc.xyw(2, row, 3));     
        
        row += 2;
        
        drawEdgeWeights.addActionListener(this);
        drawEdgeWeights.setSelected(Configuration.getInstance().getGeneralConfig().exportTransparent());
        panel.add(drawEdgeWeights, cc.xyw(2, row, 3));     
        
        row += 2;
        
        spinnerRadius = new JSpinner(new SpinnerNumberModel(Node.getRadius(), 2, 1000, 1));    	
        spinnerRadius.addChangeListener(this);
        panel.add(new JLabel("Node radius"), cc.xy(2, row));
        panel.add(spinnerRadius, cc.xyw(4, row, 3));
		
        return panel;
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==ok) {	
			File f = new File(tfFile.getText());
			if (f.exists()) {
				int answer = JOptionPane.showConfirmDialog(this, "File exists. Overwrite file?", "File exists", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
				if (answer==JOptionPane.NO_OPTION) return;
			}
			if (f.isDirectory()) {
				JOptionPane.showMessageDialog(this, "You only specified a directory.\nPlease enter also a file name for saving the image.", "Can not save file", JOptionPane.ERROR_MESSAGE);
				return;
			}
			if (f.getParentFile()==null || !f.getParentFile().exists()) {
				JOptionPane.showMessageDialog(this, "You either did not specify a file or the specified parent directory does not exist.\nPlease choose another file for saving the image.", "Can not save file", JOptionPane.ERROR_MESSAGE);
				return;
			}
            control.saveGraphImage(f, drawHypNames.isSelected(), drawHypWeights.isSelected(), drawEdgeWeights.isSelected());
            parent.getMBar().showFile(f);
			dispose();
		} else if (e.getSource() == copyToClipboard) {
			if (OSTools.isLinux() && !Configuration.getInstance().getClassProperty(this.getClass(), "showClipboardInfo", "yes").equals("no")) {
				String jsv = System.getProperty("java.specification.version");
				if (jsv.equals("1.5") || jsv.equals("1.6")) {
					String message = "An old bug from 2007 that is widely known but never\n" +
							"fixed by Sun/Oracle in Java will most likely prevent this\n" +
							"feature to work on a Linux machine.\n" +
							"We are sorry…";
					JCheckBox tellMeAgain = new JCheckBox("Don't show me this info again.");			
					JOptionPane.showMessageDialog(parent, new Object[] {message, tellMeAgain}, "Will most likely not work under Linux", JOptionPane.WARNING_MESSAGE);
					if (tellMeAgain.isSelected()) {
						Configuration.getInstance().setClassProperty(this.getClass(), "showClipboardInfo", "no");
					}
				}
			}
			TransferableImage.copyImageToClipboard(control.getNL().getImage(Configuration.getInstance().getGeneralConfig().getExportZoom(), Configuration.getInstance().getGeneralConfig().getColoredImages(), drawHypNames.isSelected(), drawHypWeights.isSelected(), drawEdgeWeights.isSelected()));
			dispose();
		} else if (e.getSource()==cbColored) {		
			Configuration.getInstance().getGeneralConfig().setColoredImages(cbColored.isSelected());
		} else if (e.getSource()==cbTransparent) {
			Configuration.getInstance().getGeneralConfig().setExportTransparent(cbTransparent.isSelected());
		} else if (e.getSource()==selectFile) {
			JFileChooser fc = new JFileChooser(Configuration.getInstance().getClassProperty(this.getClass(), "ImageDirectory"));		
	        fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
	        fc.setDialogType(JFileChooser.SAVE_DIALOG);
	        fc.setFileFilter(new FileFilter() {
				public boolean accept(File f) {
					if (f.isDirectory()) return true;
					return f.getName().toLowerCase().endsWith(".png");
				}
				public String getDescription () { return "PNG image files"; }  
			});
	        int returnVal = fc.showOpenDialog(this);
	        if (returnVal == JFileChooser.APPROVE_OPTION) {
	            File f = fc.getSelectedFile();
	            Configuration.getInstance().setClassProperty(this.getClass(), "ImageDirectory", f.getParent());
	            if (!f.getName().toLowerCase().endsWith(".png")) {
	            	f = new File(f.getAbsolutePath()+".png");
	            }
	            tfFile.setText(f.getAbsolutePath());
	            Configuration.getInstance().setClassProperty(this.getClass(), "LastImage", f.getAbsolutePath());
	        }		
		}
		ip.setImage(getImage());
		repaint();
	}
	
	public BufferedImage getImage() {
		//TODO Zoom? Configuration.getInstance().getGeneralConfig().getExportZoom();
		return control.getNL().getImage(1d, Configuration.getInstance().getGeneralConfig().getColoredImages(), drawHypNames.isSelected(), drawHypWeights.isSelected(), drawEdgeWeights.isSelected());
	}
	
	public void save() {
		

		
	}

	public void stateChanged(ChangeEvent e) {		
		control.getNL().setRadius(Integer.parseInt(spinnerRadius.getModel().getValue().toString()));
		ip.setImage(getImage());
		repaint();
		parent.repaint();
	}

}
