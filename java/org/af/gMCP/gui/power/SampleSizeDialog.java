package org.af.gMCP.gui.power;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Set;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.widgets.DesktopPaneBG;
import org.af.commons.widgets.buttons.HorizontalButtonPane;
import org.af.commons.widgets.validate.ValidationException;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.dialogs.PowerOptionsPanel;
import org.af.gMCP.gui.dialogs.TextFileViewer;
import org.af.jhlir.call.RDataFrame;
import org.jdesktop.swingworker.SwingWorker;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class SampleSizeDialog extends PDialog implements ActionListener {

    RandomizationPanel randomizationPanel;
    PowerReqPanel prPanel;
    
	JButton jbHelp;

    //  Theta hat: θ\u0302

	/**
	 * Constructor
	 * @param parent Parent CreateGraphGUI
	 */
	public SampleSizeDialog(CreateGraphGUI parent) {
		super(parent, "Sample Size Calculations", true);
		
		randomizationPanel = new RandomizationPanel(this);
		tPanel.addTab("Randomization", randomizationPanel);
		pNCP = new ScenarioPanel2(this);
		tPanel.addTab(/*"Standardized "+*/"Effect Size", (Component) pNCP);		
		prPanel = new PowerReqPanel(this);
		tPanel.addTab("Power Requirements", prPanel);
		cvPanel = new CVPanel(this);
		tPanel.addTab("Correlation Matrix", cvPanel);
		oPanel = new PowerOptionsPanel(parent);
		tPanel.addTab("Options", oPanel);
		
		//TODO: Do we want scrollable tabs? 
		//tPanel.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
		
		Set<String> variables = parent.getGraphView().getNL().getAllVariables();
		if (!Configuration.getInstance().getGeneralConfig().useEpsApprox())	{
			variables.remove("ε");
		}
		
		getContentPane().add(tPanel, c);
		
		c.weighty=0; c.gridy++; c.weightx=0; c.fill=GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.EAST;
		//HorizontalButtonPane bp = new OkCancelButtonPane();
		
		try {
			jbHelp = new JButton(
					new ImageIcon(ImageIO.read(DesktopPaneBG.class
							.getResource("/org/af/gMCP/gui/graph/images/questionmark32.png"))));
		} catch (IOException e) {
			ErrorHandler.getInstance().makeErrDialog(e.getMessage(), e);
			jbHelp = new JButton("Help!");
		}
		jbHelp.addActionListener(this);
		
		HorizontalButtonPane bp = new HorizontalButtonPane(new String[] {"Help", "Ok", "Cancel"}, new String[] {"help", HorizontalButtonPane.OK_CMD, HorizontalButtonPane.CANCEL_CMD});
		getContentPane().add(bp, c);
		bp.addActionListener(this);		
		
		config = new File(path, "gMCP-samplesize-settings.xml");
		if (config.exists()) {
			SettingsToXML.loadConfigFromXML(config, this);
		}
		// After loading the config we can attach this ActionListener:
		prPanel.setActionListener();
		
        pack();
        // Adding space for further arms or scenarios:
        Dimension d = this.getSize();
        this.setSize(d.width, d.height+100);
        setLocationRelativeTo(parent);
        setVisible(true);
	} 
	
	 public void loadConfig(Element root) {
		 super.loadConfig(root);
		 randomizationPanel.loadConfig((Element) root.getElementsByTagName("randomization").item(0));
		 prPanel.loadConfig((Element) root.getElementsByTagName("powerreq").item(0));
	 }

	public List<Element> getConfigurationNodes(Document document) {
		List<Element> v = super.getConfigurationNodes(document);
		v.add(randomizationPanel.getConfigNode(document));
		v.add(prPanel.getConfigNode(document));
		return v;
	}	
	
	public void actionPerformed(ActionEvent e) {
		String weights = parent.getGraphView().getNL().getGraphName() + "@weights";
		double alpha;
		try {
			alpha = parent.getPView().getTotalAlpha();
		} catch (Exception e1) {
			return;
		}
		String graph = parent.getGraphView().getNL().getGraphName();

		// TODO: Do we still need sometimes something as parse2numeric? I guess yes.
		//RControl.getR().eval(parent.getGraphView().getNL().getGraphName()+"<-gMCP:::parse2numeric("+parent.getGraphView().getNL().getGraphName()+")");

		if (e.getActionCommand().equals(HorizontalButtonPane.OK_CMD)) {

			SettingsToXML.saveSettingsToXML(config, this);
			
			try {
				rCommand = "sampSize(graph=" + graph						
						+", effSize=" + pNCP.getEffSizeString()
						+", esf=" + randomizationPanel.getESF()
						+", powerReqFunc=" + prPanel.getPowerFunctions()
						+", target="+prPanel.getPowerTargets()					 
						+ ", corr.sim = " + cvPanel.getSigma() //diag(length(mean)),corr = NULL,"+
						+", alpha=" + alpha
						+ cvPanel.getMatrixForParametricTest()
						+ ", type = \""+Configuration.getInstance().getGeneralConfig().getTypeOfRandom()+"\""
						+ ", upscale = "+(Configuration.getInstance().getGeneralConfig().getUpscale()?"TRUE":"FALSE")
						+ ", n.sim = "+Configuration.getInstance().getGeneralConfig().getNumberOfSimulations()
						+ ")";
			} catch (ValidationException e1) {
				JOptionPane.showMessageDialog(this, "An error was detected in the input:\n"+e1.getMessage(), "Error detected in input", JOptionPane.ERROR_MESSAGE);
				return;
			}				

			System.out.println("The following R will be executed:\n\n" + rCommand);	
			//rCommand = "paste(capture.output("+rCommand+"), collapse='\n')";					
			
			parent.glassPane.start();
			SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {

				@Override
				protected Void doInBackground() throws Exception {					
					try {
						RControl.setSeed();
						RDataFrame result = RControl.getR().eval(rCommand).asRDataFrame();
						String[] colnames = new String[] {"Scenario", "PowerFunc", "target", "sampSize"};
						new PowerResultDialog(parent, "SampleSize Results", result, colnames, rCommand, SampSizeResultTableModel.class);
					} catch (Exception e) {
						ErrorHandler.getInstance().makeErrDialog(e.getMessage(), e, false);						
					} finally {
						parent.glassPane.stop();
					}
					return null;
				}					 
			};
			worker.execute();				
		}
		if (e.getActionCommand().equals("help")) {
			if (tPanel.getSelectedComponent()==randomizationPanel) {
				parent.openHelp("randomization");
			} else if (tPanel.getSelectedComponent()==pNCP) {
				parent.openHelp("ses");
			} else if (tPanel.getSelectedComponent()==prPanel) {
				parent.openHelp("powerreq");
			} else if (tPanel.getSelectedComponent()==cvPanel) {
				parent.openHelp("cormat2");
			} else if (tPanel.getSelectedComponent()==oPanel) {
				parent.openHelp("options");
			} else {
				parent.openHelp("power");
			}
		} else {		
			dispose();
		}
	}

}
