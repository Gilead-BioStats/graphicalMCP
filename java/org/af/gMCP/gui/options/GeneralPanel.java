package org.af.gMCP.gui.options;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import org.af.commons.widgets.WidgetFactory;
import org.af.commons.widgets.lists.IntegerJComboBox;
import org.af.gMCP.config.Configuration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

/**
 * OptionsPanel for general settings.
 */
public class GeneralPanel extends OptionsPanel implements ActionListener {
    private static final Log logger = LogFactory.getLog(GeneralPanel.class);

    private IntegerJComboBox cbFontSize;
    private JComboBox cbLookAndFeel;
    private JTextField jtfGrid;
    private JTextField jtfNumberOfDigits;
    private JTextField jtfLineWidth;
    
    private Configuration conf;
    private OptionsDialog odialog;
    private JCheckBox colorImages;
    private JCheckBox showRejected;
    private JCheckBox showFractions;
    private JCheckBox useJLaTeXMath;
    private JCheckBox markEpsilon;
    
	JFrame parent;

    public GeneralPanel(JFrame parent, OptionsDialog odialog) {
        this.conf = Configuration.getInstance();
        this.odialog = odialog;
        this.parent = parent;

        makeComponents();
        doTheLayout();
    }

    /**
     * Instantiation of Swing-Components.
     */
    private void makeComponents() {
        cbFontSize = new IntegerJComboBox(8, 20);
        cbFontSize.setSelectedObject(conf.getGeneralConfig().getFontSize());
        cbFontSize.setToolTipText("<html>" +
        		"Font size of the text in the GUI widgets.</html>");
        jtfGrid = new JTextField(30);
        jtfGrid.setText(""+conf.getGeneralConfig().getGridSize());
        jtfGrid.setToolTipText("<html>" +
        		"For easier placement of nodes a grid can be used that aligns the<br>" +
        		"nodes to its intersections. You can specify a positive integer<br>" +
        		"that sets the grid size, i.e. the width in pixels between two<br>" +
        		"proximate parallel lines. If you set the grid size to 1 this would<br>" +
        		"allow unrestricted placement and therefore disables the grid.</html>");
        jtfNumberOfDigits = new JTextField(30);
        jtfNumberOfDigits.setText(""+conf.getGeneralConfig().getDigits());
        jtfNumberOfDigits.setToolTipText("<html>" +
        		"Number of digits to be shown at various places.<br>" +
        		"In this version not every part of the GUI will use<br>" +
        		"this value, but this will improve in further versions.</html>");
        jtfLineWidth = new JTextField(30);
        jtfLineWidth.setText(""+conf.getGeneralConfig().getLineWidth());
        jtfLineWidth.setToolTipText("<html>" +
        		"Especially if you want to use exported PNG graphics in other<br>" +
        		"documents, you may want to adjust the line width of edges<br>" +
        		"and nodes, when borders look to thin or thick.</html>");
        
        Vector<String> looknfeel = new Vector<String>();
        looknfeel.add("System");
        looknfeel.add("Windows");
        looknfeel.add("Mac OS");        
        looknfeel.add("Motif");
        looknfeel.add("Metal (highly recommended default)");

        cbLookAndFeel = new JComboBox(looknfeel);
        String currentLookNFeel = conf.getJavaConfig().getLooknFeel();
        logger.info("LooknFeel is " + currentLookNFeel + ".");
        for (int i = 0; i < looknfeel.size(); i++) {
            cbLookAndFeel.setSelectedIndex(i);
            if (getLooknFeel().equals(currentLookNFeel)) break;
            logger.debug("Not " + getLooknFeel());
        }
        cbLookAndFeel.setToolTipText("<html>" +
        		"The way the widgets of a GUI look and how they behave is<br>" +
        		"called \"look and feel\" in Java. Depending on your operating<br>" +
        		"system and classpath several Look'n'Feel implementations may<br>" +
        		"be available (e.g. Metal (Java default), Windows, Mac OS,<br>" +
        		"Motif and/or System/GTK).<br>" +
        		"If you are used to a particular Look'n'Feel, you can select<br>" +
        		"it here. But if you have problems with the graphical interface,<br>" +
        		"please try to use the default Metal theme to check whether it<br>" +
        		"could be a problem with the selected Look'n'Feel.</html>");
        
        colorImages = new JCheckBox("Colored image files and LaTeX/docx reports");
        colorImages.setSelected(conf.getGeneralConfig().getColoredImages());
        colorImages.setToolTipText("<html>" +
        		"Colors are used to highlight different conditions in the graph<br>" +
        		"like hypotheses that could be rejected. While these colors are<br>" +
        		"helpful in the GUI, you perhaps prefer black and white PNG image<br>" +
        		"files and LaTeX/docx reports.</html>");
        
        showFractions = new JCheckBox("Show fractions instead of decimal numbers");
        showFractions.setSelected(conf.getGeneralConfig().showFractions());
        showFractions.setToolTipText("<html>" +
        		"Floating point numbers are used for all calculations and<br>" +
        		"values like 1/3 would be normally shown as 0.3333333.<br>" +
        		"When this option is active the method fractions from package<br>" +
        		"MASS is used to display fractions whenever the floating point<br>" +
        		"numbers are close to a fraction that looks right.</html>");
        
        showRejected = new JCheckBox("Show rejected nodes in GUI");
        showRejected.setSelected(conf.getGeneralConfig().showRejected());
        showRejected.setToolTipText("<html>" +
        		"When using the GUI to for stepwise rejection of hypotheses,<br>" +
        		"this options determines whether rejected nodes should<br>" +
        		"\"disappear\" or whether they remain on the screen and are<br>" +
        		"only marked as rejected.</html>");
        
        useJLaTeXMath = new JCheckBox("Use JLaTeXMath");
        useJLaTeXMath.setSelected(conf.getGeneralConfig().useJLaTeXMath());
        useJLaTeXMath.setToolTipText("<html>" +
        		"There are not many reasons not to use the free Java library<br>" +
        		"JLaTeXMath to render numbers, symbols and formulas in the<br>" +
        		"GUI. The option is mainly provided in case that errors occur<br>" +
        		"displaying the numbers and formulas.</html>");
        
        markEpsilon = new JCheckBox("Show epsilon edges as dashed lines.");
        markEpsilon.setSelected(conf.getGeneralConfig().markEpsilon());
        markEpsilon.setToolTipText("<html>" +
        		"You can set whether epsilon edges should<br>" +
        		"been shown as dashed or solid lines." +
        		"</html>");

    }

    private void doTheLayout() {
        JPanel p1 = new JPanel();
        String cols = "pref, 5dlu, fill:pref:grow";
        String rows = "pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";
        
        FormLayout layout = new FormLayout(cols, rows);
        p1.setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 1;
        
        p1.add(new JLabel("Grid:"),     cc.xy(1, row));
        p1.add(jtfGrid, cc.xy(3, row));        
        
        row += 2;
        
        p1.add(new JLabel("Number of digits:"),     cc.xy(1, row));
        p1.add(jtfNumberOfDigits, cc.xy(3, row));        
        
        row += 2;
        
        p1.add(new JLabel("Line width:"),     cc.xy(1, row));
        p1.add(jtfLineWidth, cc.xy(3, row));        
        
        row += 2;
        
        p1.add(new JLabel("Font Size"),     cc.xy(1, row));
        p1.add(cbFontSize, cc.xy(3, row));
        
        row += 2;

        p1.add(new JLabel("Look'n'Feel"),           cc.xy(1, row));
        p1.add(cbLookAndFeel, cc.xy(3, row));
        
        row += 2;
        
        p1.add(colorImages, cc.xyw(1, row, 3));
        
        row += 2;        
        
        p1.add(showRejected, cc.xyw(1, row, 3));        
        
        row += 2;        
        
        p1.add(useJLaTeXMath, cc.xyw(1, row, 3));        
        
        row += 2;        
        
        p1.add(showFractions, cc.xyw(1, row, 3));    
        
        row += 2;
        
        p1.add(markEpsilon, cc.xyw(1, row, 3));    
        
        add(p1);
    }


    private String lfID2FullName(String id) {
        if (id.equals("Metal (highly recommended default)")) {
            return UIManager.getCrossPlatformLookAndFeelClassName();
        } else if (id.equals("System")) {
            return UIManager.getSystemLookAndFeelClassName();
        } else if (id.equals("Motif")) {
            return "com.sun.java.swing.plaf.motif.MotifLookAndFeel";
        } else if (id.equals("Windows")) {
            return "com.sun.java.swing.plaf.windows.WindowsLookAndFeel";
        } else if (id.equals("Mac OS")) {
            return "javax.swing.plaf.mac.MacLookAndFeel";
        }
        return null;
    }

    private String getLooknFeel() {
        String lf = lfID2FullName(cbLookAndFeel.getSelectedItem().toString());
        if (lf == null)
            logger.warn("No LooknFeel selected! How can this be?");
        return lf;
    }

    public void setProperties() throws SetLookAndFeelException {

        try {
        	int grid = Integer.parseInt(jtfGrid.getText());
        	conf.getGeneralConfig().setGridSize(grid);
        } catch (NumberFormatException e) {
        	JOptionPane.showMessageDialog(this, "\""+jtfGrid.getText()+"\" is not a valid integer for grid size.", "Invalid input", JOptionPane.ERROR_MESSAGE);
        }
        try {
        	int lw = Integer.parseInt(jtfLineWidth.getText());
        	conf.getGeneralConfig().setLineWidth(lw);
        } catch (NumberFormatException e) {
        	JOptionPane.showMessageDialog(this, "\""+jtfLineWidth.getText()+"\" is not a valid integer for line width.", "Invalid input", JOptionPane.ERROR_MESSAGE);
        }
        try {
        	int digits = Integer.parseInt(jtfNumberOfDigits.getText());
        	conf.getGeneralConfig().setDigits(digits);
        } catch (NumberFormatException e) {
        	JOptionPane.showMessageDialog(this, "\""+jtfNumberOfDigits.getText()+"\" is not a valid integer for the number of digits.", "Invalid input", JOptionPane.ERROR_MESSAGE);
        }
        
       	conf.getGeneralConfig().setColoredImages(colorImages.isSelected());
       	conf.getGeneralConfig().setShowRejected(showRejected.isSelected());
       	conf.getGeneralConfig().setShowFractions(showFractions.isSelected());
       	conf.getGeneralConfig().setUseJLaTeXMath(useJLaTeXMath.isSelected());
       	conf.getGeneralConfig().setMarkEpsilon(markEpsilon.isSelected());

       	try {
       		String currentLF = conf.getJavaConfig().getLooknFeel(); // UIManager.getLookAndFeel();
       		logger.info("Selected LooknFeel:" + getLooknFeel());

       		if (!getLooknFeel().equals(currentLF)) {
           		setLooknFeel(getLooknFeel());
       			int n = JOptionPane.showConfirmDialog(parent,
       					"Keep this Look'n'Feel?",
       					"Keep this Look'n'Feel?",
       					JOptionPane.YES_NO_OPTION);

       			if (n == JOptionPane.YES_OPTION) {
       				conf.getJavaConfig().setLooknFeel(getLooknFeel());
       			} else {
       				setLooknFeel(currentLF);
       			}
       		}
       	} catch (Exception exc) {
       		// look&feel exception
       		//throw new SetLookAndFeelException(exc);
       		JOptionPane.showMessageDialog(parent, "The selected LooknFeel is not available.", "Selected LooknFeel not available.", JOptionPane.WARNING_MESSAGE);
       	}
       	
       	/* Font size: */ 
        int fontSize = cbFontSize.getSelectedObject();        
       	if (conf.getGeneralConfig().getFontSize()!=fontSize) {
       		conf.getGeneralConfig().setFontSize(fontSize);
            WidgetFactory.setFontSizeGlobal(conf.getGeneralConfig().getFontSize());
            SwingUtilities.updateComponentTreeUI(parent);
            SwingUtilities.updateComponentTreeUI(odialog);
            odialog.pack();
       	}
    }

    private void setLooknFeel(String id) throws ClassNotFoundException, IllegalAccessException,
            InstantiationException, UnsupportedLookAndFeelException {
        UIManager.setLookAndFeel(id);
        SwingUtilities.updateComponentTreeUI(parent);
        SwingUtilities.updateComponentTreeUI(odialog);
        odialog.pack();
    }

    private void setLooknFeel(LookAndFeel lf) throws ClassNotFoundException, IllegalAccessException,
            InstantiationException, UnsupportedLookAndFeelException {
        setLooknFeel(lfID2FullName(lf.getID()));
    }

	public void actionPerformed(ActionEvent e) {	
		// Not used in the moment.
	}
}

