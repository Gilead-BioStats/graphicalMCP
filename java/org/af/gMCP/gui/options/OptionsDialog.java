package org.af.gMCP.gui.options;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.border.EmptyBorder;

import org.af.commons.Localizer;
import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.widgets.DesktopPaneBG;
import org.af.commons.widgets.buttons.OkApplyCancelButtonPane;
import org.af.commons.widgets.validate.ValidationException;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.MenuBarMGraph;
import org.af.gMCP.gui.graph.Edge;
import org.af.gMCP.gui.graph.Node;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

/**
 * Dialog for configuring various settings.
 */
public class OptionsDialog extends JDialog implements ActionListener {
	private static final Log logger = LogFactory.getLog(OptionsDialog.class);

	private JTabbedPane tabbedPane;
    private GeneralPanel visualPanel;
    private NumericPanel numericPanel;
    private MiscPanel miscPanel;
    private JPanel bp;

    private Configuration conf;

    CreateGraphGUI parent;
    
    JButton jbHelp;
    
    public final static String HELP = "HELP";
    public final static String NUMERIC = "Numeric";
    public final static String MISC = "Misc";
    
    /**
     * Standard constructor
     */
    public OptionsDialog(CreateGraphGUI p) {
    	this(p, true);
    }

    public OptionsDialog(CreateGraphGUI parent, String string) {
		this(parent, false);
		if (string.equals(NUMERIC)) {
			tabbedPane.setSelectedIndex(1);
		}
		if (string.equals(MISC)) {
			tabbedPane.setSelectedIndex(2);
		}
		setVisible(true);
	}

	public OptionsDialog(CreateGraphGUI p, boolean setVisible) {
    	super(p);
    	this.parent = p;
        this.conf = Configuration.getInstance();
        setModal(true);
        setTitle("Options");

        setDefaultCloseOperation(DISPOSE_ON_CLOSE);

        makeComponents();
        doTheLayout();
        
        pack();
        setLocationRelativeTo(p);
        if (setVisible) setVisible(true);
	}

	/**
     * Instantiation of Swing-Components.
     */
    private void makeComponents() {
        tabbedPane = new JTabbedPane();
        visualPanel = new GeneralPanel(parent, this);
        numericPanel = new NumericPanel(conf);
        miscPanel = new MiscPanel(conf); 
        
		
        bp = getButtonPane();
    }
    
    private JPanel getButtonPane() {
    	JPanel panel = new JPanel();    	
    	panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    	try {
			jbHelp = new JButton("Help", 
					new ImageIcon(ImageIO.read(DesktopPaneBG.class
							.getResource("/org/af/gMCP/gui/graph/images/questionmark.png"))));
		} catch (IOException e) {
			ErrorHandler.getInstance().makeErrDialog(e.getMessage(), e);
			jbHelp = new JButton("Help!");
		}
		jbHelp.addActionListener(this);
		jbHelp.setActionCommand(HELP);
    	panel.add(jbHelp);
        panel.add(Box.createHorizontalStrut(5));    	
        JButton cancel = new JButton(Localizer.getInstance().getString("AFCOMMONS_WIDGETS_BUTTONS_CANCEL"));
    	cancel.setActionCommand(OkApplyCancelButtonPane.CANCEL_CMD);
    	panel.add(cancel);
    	cancel.addActionListener(this);
        panel.add(Box.createHorizontalStrut(5));
        
        JButton apply = new JButton(Localizer.getInstance().getString("AFCOMMONS_WIDGETS_BUTTONS_APPLY"));
        apply.addActionListener(this);
    	apply.setActionCommand(OkApplyCancelButtonPane.APPLY_CMD);
    	panel.add(apply);
        panel.add(Box.createHorizontalStrut(5));
    	
        JButton ok = new JButton(Localizer.getInstance().getString("AFCOMMONS_WIDGETS_BUTTONS_OK"));
    	ok.setActionCommand(OkApplyCancelButtonPane.OK_CMD);
    	ok.addActionListener(this);
    	panel.add(ok);
        panel.add(Box.createHorizontalStrut(5));
    	
    	return panel;
    }
    
    public static JPanel makeDialogPanelWithButtons (Container content, JPanel hbp) {        
        JPanel p = new JPanel();
        String cols = "fill:pref:grow";
        String rows = "fill:pref:grow, 5dlu, bottom:pref:n";
        FormLayout layout = new FormLayout(cols, rows);
        p.setLayout(layout);

        CellConstraints cc = new CellConstraints();
        p.add(content, cc.xy(1,1));
        p.add(hbp, cc.xy(1,3, "right, bottom"));
        p.setBorder(new EmptyBorder(5, 5, 5, 5));
        return p;
    }

    /**
     * Do the layout.
     */
    private void doTheLayout() {
        tabbedPane.addTab("Visual", visualPanel);
        tabbedPane.addTab("Numeric", numericPanel);
        tabbedPane.addTab("Misc", miscPanel);
        Container cp = getContentPane();
        cp.add(tabbedPane);
        cp = makeDialogPanelWithButtons(cp, bp);
        setContentPane(cp);
    }

    /**
     * Calls setProperties of the selected OptionsPanel of the TabbedPane on OK.
     * @param e ActionEvent to process.
     */
    public void actionPerformed(ActionEvent e) {
    	logger.debug("Got ActionCommand "+e.getActionCommand());
        if ( (e.getActionCommand().equals(OkApplyCancelButtonPane.OK_CMD)) ||
        		(e.getActionCommand().equals(OkApplyCancelButtonPane.APPLY_CMD)) ) {
            try {
            	visualPanel.setProperties();
            	numericPanel.setProperties();
            	miscPanel.setProperties();
                if  (e.getActionCommand().equals(OkApplyCancelButtonPane.OK_CMD)) {
                	dispose();
                }
                for (Edge edge : parent.getGraphView().getNL().getEdges()) {                	
                	//TODO Update edge weights
                }
                for (Node node : parent.getGraphView().getNL().getNodes()) {
                	node.createWeightIcons();
                }
                parent.repaint();
            } catch (ValidationException exc) {
                JOptionPane.showMessageDialog(this, exc.getMessage());
            } catch (SetLookAndFeelException exc) {
                ErrorHandler.getInstance().makeErrDialog(exc.getMessage(), exc);
            }
        }
        if (e.getActionCommand().equals(OkApplyCancelButtonPane.CANCEL_CMD)) {
            dispose();
        }
        if (e.getActionCommand().equals(HELP)) {
        	parent.openHelp("options");
        }
        ((MenuBarMGraph)parent.getJMenuBar()).createExampleMenu();
        ((MenuBarMGraph)parent.getJMenuBar()).createExtraMenu();
    }

}


