package org.af.gMCP.gui.power;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Set;

import javax.swing.JCheckBox;
import javax.swing.JOptionPane;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.widgets.buttons.HorizontalButtonPane;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.dialogs.PowerOptionsPanel;
import org.af.gMCP.gui.options.OptionsDialog;
import org.af.jhlir.call.RList;
import org.jdesktop.swingworker.SwingWorker;

public class PowerDialog extends PDialog implements ActionListener {

	/**
	 * Constructor
	 * @param parent Parent JFrame
	 */
	public PowerDialog(CreateGraphGUI parent) {
		super(parent, "Power Simulation - specify probability distribution of test statistics", true);
		
		config = new File(path, "gMCP-power-settings.xml");
		
		pNCP = new ScenarioPanel(this);
		tPanel.addTab("Noncentrality Parameter (NCP) Settings", (Component) pNCP);
		cvPanel = new CVPanel(this);
		tPanel.addTab("Correlation Matrix", cvPanel);
		userDefinedFunctions = new UserDefinedPanel(this, nodes);
		tPanel.addTab("User defined power function", userDefinedFunctions);
		oPanel = new PowerOptionsPanel(parent);
		tPanel.addTab("Options", oPanel);
		Set<String> variables = parent.getGraphView().getNL().getAllVariables();
		if (!Configuration.getInstance().getGeneralConfig().useEpsApprox())	{
			variables.remove("ε");
		}
		if (variables.size()>0) {
			vp = new VariablePanel(variables);
			tPanel.addTab("Variables", vp);
		}
		
		getContentPane().add(tPanel, c);
		
		c.weighty=0; c.gridy++; c.weightx=0; c.fill=GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.EAST;
		
		HorizontalButtonPane bp = new HorizontalButtonPane(new String[] {"Help", "Ok", "Cancel"}, new String[] {"help", HorizontalButtonPane.OK_CMD, HorizontalButtonPane.CANCEL_CMD});
		getContentPane().add(bp, c);
		bp.addActionListener(this);		
		
		if (config.exists()) {
			SettingsToXML.loadConfigFromXML(config, this);
		}
		
        pack();
        setLocationRelativeTo(parent);
        setSize(Math.max(870, getWidth()), getHeight());
        
        int answer = JOptionPane.NO_OPTION;
        
		if (tmp && !Configuration.getInstance().getClassProperty(this.getClass(), "tellAboutFiles", "yes").equals("no")) {
			JCheckBox tellMeAgain = new JCheckBox("Don't show me this info again.");			
			String message = "The settings in this dialog will be saved for further runs in this session.\n" +
							 "If you want these settings to be automatically saved not only temporarily,\n" +
							 "but even between sessions, please specify a directory for saving these\n" +
							 "files in the options.\n" +
							 "Do you want to open the options dialog now?";
			answer = JOptionPane.showConfirmDialog(parent, new Object[] {message, tellMeAgain}, "Info", JOptionPane.YES_NO_OPTION);
			if (tellMeAgain.isSelected()) {
				Configuration.getInstance().setClassProperty(this.getClass(), "tellAboutFiles", "no");
			}
		}
		
		if (answer==JOptionPane.YES_OPTION) {
			new OptionsDialog(parent, OptionsDialog.MISC);
			dispose();
			return;
		}
        
        setVisible(true);
		
	} 
	
	public void actionPerformed(ActionEvent e) {

		double alpha;
		try {
			alpha = parent.getPView().getTotalAlpha();
		} catch (Exception e1) {
			return;
		}

		// TODO: Do we still need sometimes something as parse2numeric? I guess yes.
		//RControl.getR().eval(parent.getGraphView().getNL().getGraphName()+"<-gMCP:::parse2numeric("+parent.getGraphView().getNL().getGraphName()+")");

		if (e.getActionCommand().equals(HorizontalButtonPane.OK_CMD)) {
			
			// If there is still some user-defined power function in the jtUserDefined JTextField, this will add it to the list: 
			userDefinedFunctions.actionPerformed(null);
			
			if (RControl.getR().eval("any(is.na("+cvPanel.getSigma()+"))").asRLogical().getData()[0]) {
				JOptionPane.showMessageDialog(this, "Correlation matrix for simulation can not contain NAs.", "No NAs allowed", JOptionPane.ERROR_MESSAGE);
				tPanel.setSelectedComponent(cvPanel);
				return;
			}
			
			SettingsToXML.saveSettingsToXML(config, this);

			createLongRCommand(alpha);
			
			rCommand = "gMCP:::calcMultiPower(graph="+parent.getGraphView().getNL().getGraphName()+", alpha="+alpha+", ncpL="+pNCP.getNCPString()
					+ ","+"corr.sim = " + cvPanel.getSigma() //diag(length(mean)),corr = NULL,"+
					+ cvPanel.getMatrixForParametricTest()
					+ ", f = "+userDefinedFunctions.getUserDefined()
					+ ", n.sim = "+Configuration.getInstance().getGeneralConfig().getNumberOfSimulations()
					+ ", type = \""+Configuration.getInstance().getGeneralConfig().getTypeOfRandom()+"\""
					+ ", upscale = "+(Configuration.getInstance().getGeneralConfig().getUpscale()?"TRUE":"FALSE")
					+ getVariables(false)
					+ ", digits=4"
					+ ")";				

			parent.glassPane.start(); //TODO Why is the glasspane not shown?			
			SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {

				@Override
				protected Void doInBackground() throws Exception {					
					try {
						RControl.setSeed();
						RList result = RControl.getR().eval(rCommand).asRList();
						new PowerResultDialog(parent, "Power Results", result.get(0).asRDataFrame(), result.get(1).asRChar().getData(), longRCommand, PowerResultTableModel.class);
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
			if (tPanel.getSelectedComponent()==pNCP) {
				parent.openHelp("ncps");
			} else if (tPanel.getSelectedComponent()==cvPanel) {
				parent.openHelp("cormat2");
			} else if (tPanel.getSelectedComponent()==oPanel) {
				parent.openHelp("optNumeric");
			} else if (tPanel.getSelectedComponent()==userDefinedFunctions) {
				parent.openHelp("udpf");
			} else {
				parent.openHelp("power");
			}
		} else {		
			dispose();
		}
	}

	private String getVariables(boolean quote) {
		if (vp==null) return "";
		String q = "";
		if (quote) q = "\"";		
		return ", variables="+q+vp.getVariables()+q;
	}

	private void createLongRCommand(double alpha) {

		longRCommand = "createCalcPowerCall(graph="+parent.getGraphView().getNL().getGraphName()+", alpha="+alpha+", ncpL=\""+pNCP.getNCPString()+"\""
				+ ","+"corr.sim = " + cvPanel.getSigma() //diag(length(mean)),corr = NULL,"+
				+ cvPanel.getMatrixForParametricTest()
				+ ", f = \""+userDefinedFunctions.getUserDefined()+"\""
				+ ", n.sim = "+Configuration.getInstance().getGeneralConfig().getNumberOfSimulations()
				+ ", type = \""+Configuration.getInstance().getGeneralConfig().getTypeOfRandom()+"\""
				+ ", upscale = "+(Configuration.getInstance().getGeneralConfig().getUpscale()?"TRUE":"FALSE")
				+ getVariables(true)
				+ ", digits=4"
				+ ", seed="+Configuration.getInstance().getGeneralConfig().getSeed()
				+ ")";		
		
		longRCommand = RControl.getR().eval(longRCommand).asRChar().getData()[0];
		
	}

}
