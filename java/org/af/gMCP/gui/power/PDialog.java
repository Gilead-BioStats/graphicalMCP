package org.af.gMCP.gui.power;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.File;
import java.util.List;
import java.util.Vector;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;

import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.dialogs.PowerOptionsPanel;
import org.af.gMCP.gui.graph.Node;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class PDialog extends JDialog {

	CreateGraphGUI parent;
    Vector<Node> nodes;
    
    ScenarioPanelInterface pNCP;    
    UserDefinedPanel userDefinedFunctions;
    CVPanel cvPanel;
	VariablePanel vp = null;
    PowerOptionsPanel oPanel;
    
	File config;
	/** Path to save config files. */
	File path = null;
	/** Are config files only saved temporarily? */
	boolean tmp = false;
	String rCommand = "";
	String longRCommand = "";
	
	JTabbedPane tPanel = new JTabbedPane();
	
	GridBagConstraints c = getDefaultGridBagConstraints();
	
	public PDialog(CreateGraphGUI parent, String string, boolean b) {
		super(parent, string, b);
		setLocationRelativeTo(parent);
		this.parent = parent;
		
		if (Configuration.getInstance().getGeneralConfig().usePersistentConfigFile()) {
			path = new File(Configuration.getInstance().getGeneralConfig().getConfigDir());			
		} else {
			path = new File(RControl.getR().eval("tempdir()").asRChar().getData()[0]);
			tmp = true;
		}
		
		if (!path.exists()) {
			JOptionPane.showMessageDialog(this, "Directory for saving the settings does not exist:\n"+path.exists()+"\nPlease choose another directory in the options.\nFor now settings are only saved temporarily.", "Directory does not exist.", JOptionPane.ERROR_MESSAGE);
			path = new File(RControl.getR().eval("tempdir()").asRChar().getData()[0]);
			tmp = true;
		}
		
		parent.getPView().getParameters();
		
		getContentPane().setLayout(new GridBagLayout());
		
		nodes = parent.getGraphView().getNL().getNodes();
		
	}
	
	public Vector<Node> getNodes() {		
		return nodes;
	}
	
	 public CreateGraphGUI getParent() {
		 return parent;
	 }

	 public void loadConfig(Element root) {
		pNCP.loadConfig((Element) root.getElementsByTagName("scenarios").item(0));
		if (!parent.getPView().jrbRCorrelation.isSelected()) {
			cvPanel.loadConfig((Element) root.getChildNodes().item(1));
		}
		if (userDefinedFunctions!=null) userDefinedFunctions.loadConfig((Element) root.getChildNodes().item(2));
		if (vp!=null) vp.loadConfig((Element) root.getChildNodes().item(3));
	 }

	public List<Element> getConfigurationNodes(Document document) {
		Vector<Element> v = new Vector<Element>();
		v.add(pNCP.getConfigNode(document));
		v.add(cvPanel.getConfigNode(document));
		if (userDefinedFunctions!=null) v.add(userDefinedFunctions.getConfigNode(document));
		if (vp!=null) v.add(vp.getConfigNode(document));
		return v;
	}
	
	public static GridBagConstraints getDefaultGridBagConstraints() {
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=10; c.ipady=10;
		c.weightx=1; c.weighty=1;
		return c;
	}
	
	 
}
