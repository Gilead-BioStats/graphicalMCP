package org.af.gMCP.gui.graph;

import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.af.commons.widgets.DesktopPaneBG;
import org.af.commons.widgets.validate.RealTextField;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.CreateGraphGUI;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.dialogs.MatrixCreationDialog;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class PView extends JPanel implements KeyListener, ActionListener {

	private static final Log logger = LogFactory.getLog(PView.class);

	JLabel alphaLabel = new JLabel("Total α: ");
	GridBagConstraints c = new GridBagConstraints();	
	CellConstraints cc = new CellConstraints();
	JPanel correlatedPanel = null;
	JButton createMatrix;
	JButton jbLoadPValues = new JButton("Load p-values from R");
	public JComboBox jcbCorObject;
	
	protected JRadioButton jrbNoCorrelation = new JRadioButton("No information about correlations (Bonferroni based weighted tests)");
	public JRadioButton jrbRCorrelation = new JRadioButton("Select an R correlation matrix"); 
	public JRadioButton jrbSimes = new JRadioButton("Use Simes test");
	
	private Vector<PPanel> panels = new Vector<PPanel>();
	
	CreateGraphGUI parent;
	
	List<Double> pValues = null;
	List<RealTextField> entangledWeights = new Vector<RealTextField>();
	
	JButton refresh;
	
	JLabel statusLabel = new JLabel("");
	
	private RealTextField totalAlpha = new RealTextField("totalAlpha", 0, 1);
	
	JLabel weightLabel = new JLabel("Weights");
	
	public PView(CreateGraphGUI parent) {
		this.parent = parent;
		setLayout(new GridBagLayout());
				
		c.weightx=1; c.weighty=1; c.fill = GridBagConstraints.BOTH;
		c.gridx=0; c.gridy=0; c.gridwidth = 1; c.gridheight = 1; c.ipadx=0; c.ipady=0;
		
		totalAlpha.addKeyListener(this);
		totalAlpha.setText(Configuration.getInstance().getClassProperty(this.getClass(), "alpha level", "0.025"));
		
		setUp();
		
		jbLoadPValues.addActionListener(this);
    }

	public void actionPerformed(ActionEvent e) {
		parent.getGraphView().setResultUpToDate(false);
		if (e.getSource()==refresh) {
			refresh(true);
		} else if (e.getSource()==jrbNoCorrelation) {
			if (parent.getGraphView().getNL().getNodes().size()>0) {
				parent.getGraphView().buttonConfInt.setEnabled(true);
				parent.getGraphView().buttonadjPval.setEnabled(true);
			}
		}  else if (e.getSource()==jrbRCorrelation) {
			parent.getGraphView().buttonConfInt.setEnabled(false);
			parent.getGraphView().buttonadjPval.setEnabled(true);
		} else if (e.getSource()==jrbSimes) {
			if (!Configuration.getInstance().getClassProperty(this.getClass(), "showSimesInfo", "yes").equals("no")) {
    			JCheckBox tellMeAgain = new JCheckBox("Don't show me this info again.");
    			String message = 
    					"The Simes test requires certain assumptions\n"+
    					"(sufficient is for example independence or positive\n" +
    					"regression dependence) and it's the responsibility\n" +
    					"of the user to check whether they are fulfilled.";
    			JOptionPane.showMessageDialog(parent, new Object[] {message, tellMeAgain}, "Info", JOptionPane.INFORMATION_MESSAGE);
    			if (tellMeAgain.isSelected()) {
    				Configuration.getInstance().setClassProperty(this.getClass(), "showSimesInfo", "no");
    			}
    		}
			parent.getGraphView().buttonConfInt.setEnabled(false);
			parent.getGraphView().buttonadjPval.setEnabled(true);
		} else if (e.getSource()==jbLoadPValues) {
			parent.getGraphView().loadPValuesFromR(); 
		} else if (e.getSource()==createMatrix) {
			if (parent.getGraphView().getNL().getNodes().size()<2) {
				JOptionPane.showMessageDialog(parent, "Correlation makes only sense for more than one hypothesis.", "No correlation for one hypothesis", JOptionPane.ERROR_MESSAGE);
			} else {				
				String obj = jcbCorObject.getSelectedItem().toString();
				String matrix = obj.endsWith("matrices found.")?null:obj;
				MatrixCreationDialog mcd = new MatrixCreationDialog(parent, null, matrix, MatrixCreationDialog.getNames(parent.getGraphView().getNL().getNodes()));
				refresh(false);
				if (mcd.created==true) {
					jrbRCorrelation.setSelected(true);
				}
			}
		}
	}

	public void addPPanel(Node node) {
		panels.add(new PPanel(node, this));
		//logger.debug("Added panel for node "+node.getName());		
		setUp();
	}

	public JPanel getCorrelatedPanel() {
		
		if (correlatedPanel!=null) {
			refresh(false);		
			return correlatedPanel;
		}
		
		try {
			refresh = new JButton("Refresh", new ImageIcon(ImageIO.read(DesktopPaneBG.class
					.getResource("/org/af/gMCP/gui/graph/images/update24.png"))));
			createMatrix = new JButton("Create Matrix", new ImageIcon(ImageIO.read(DesktopPaneBG.class
					.getResource("/org/af/gMCP/gui/graph/images/matrix.png"))));
		} catch (IOException e) {
			logger.error("IOError that should never happen.", e);
		}
		refresh.setToolTipText("search again for matrices in R");
		createMatrix.setToolTipText("create matrix with GUI");
        refresh.addActionListener(this);
        createMatrix.addActionListener(this);
		
	    ButtonGroup group = new ButtonGroup();
	    group.add(jrbNoCorrelation);
	    group.add(jrbRCorrelation);
	    group.add(jrbSimes);

	    jrbNoCorrelation.addActionListener(this);
	    jrbRCorrelation.addActionListener(this);
	    jrbSimes.addActionListener(this);
		
		correlatedPanel = new JPanel();
		
	    jcbCorObject = new JComboBox(new String[] {});
	    jcbCorObject.addActionListener(this);
	    refresh(false);

	    if (!jrbRCorrelation.isSelected() && !jrbSimes.isSelected()) {
	    	jrbNoCorrelation.setSelected(true);
	    }
		
        String cols = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu, pref, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";
        
        FormLayout layout = new FormLayout(cols, rows);
        correlatedPanel.setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 2;
        
        correlatedPanel.add(jrbNoCorrelation,     cc.xyw(2, row, 7));     
        
        row += 2;
        
        correlatedPanel.add(jrbRCorrelation,     cc.xy(2, row));
        correlatedPanel.add(jcbCorObject, cc.xy(4, row));
        correlatedPanel.add(refresh, cc.xy(6, row));  
        correlatedPanel.add(createMatrix, cc.xy(8, row));
        
        row += 2;
        
        correlatedPanel.add(jrbSimes,     cc.xyw(2, row, 7));
        
        return correlatedPanel;
	}
	
	/**
	 * Constructs a String to be included in the gMCP call.
	 * @return String that is either empty or starts with a comma and 
	 * adds parameters to the gMCP call depending on the selected correlation.
	 */
	public String getParameters() {
		if (jrbRCorrelation.isSelected() && !jrbRCorrelation.isEnabled()) {
			JOptionPane.showMessageDialog(parent, "No correlation matrix available.\nUsing Bonferroni based test.", "No correlation matrix available.", JOptionPane.WARNING_MESSAGE);
			jrbNoCorrelation.setSelected(true);
		}
		String param = ", test=\"Bonferroni\"";
		if (jrbRCorrelation.isSelected()) {
			param = ", correlation="+jcbCorObject.getSelectedItem()+", test=\"parametric\"";
		} else if (jrbSimes.isSelected()) {
			param = ", test=\"Simes\"";
		}		
		return param;
	}
	
	public String getTest() {
		String param = "\"Bonferroni\"";
		if (jrbRCorrelation.isSelected()) {
			param = "\"parametric\"";
		} else if (jrbSimes.isSelected()) {
			param = "\"Simes\"";
		}
		return param;
	}
	
	public void setTest(String test) {
		if (test.equals("Bonferroni")) {
			this.jrbNoCorrelation.setSelected(true);
		} else if (test.equals("parametric")) {
			this.jrbRCorrelation.setSelected(true);
		} else if (test.equals("Simes")) {
			this.jrbSimes.setSelected(true);
		} else {
			throw new RuntimeException("Test '"+test+"' not recognized.");
		}
	}

	public double getPValue(Node node) {
		for (int i=panels.size()-1;i>=0;i--) {
			if (panels.get(i).node==node) {
				return panels.get(i).p;
			}
		}
		throw new RuntimeException("Something happend that should never happen. Please report!");
	}

	public String getPValuesString() {	
		//Why can't we generally call 'savePValues();' here and when should we do this before calling getPValuesString()?
		String s = "c(";
		for (PPanel panel : panels) {		
			s += panel.getP()+", ";
		}
		return s.substring(0, s.length()-2)+")";
	}
	
	public List<Double> getPValues() {
		Vector<Double> v = new Vector<Double>();		
		for (PPanel panel : panels) {		
			v.add(panel.getP());
		}
		return v;
	}
	
	public double getTotalAlpha() throws Exception {
		return getTotalAlpha(true);
	}
	
	public double getTotalAlpha(boolean warn) throws Exception {
		try {
			return Double.parseDouble(totalAlpha.getText());
		} catch (NumberFormatException e) {
			if (warn) {
				String error = "Alpha "+totalAlpha.getText()+" is no valid number between 0 and 1.";
				JOptionPane.showMessageDialog(parent, error, "Error parsing alpha", JOptionPane.ERROR_MESSAGE);
				throw (new WrongInputException(error));
			}
			return 0;
		}
	}

	public void keyPressed(KeyEvent e) {keyTyped(e);}

	public void keyReleased(KeyEvent e) {keyTyped(e);}

	public void keyTyped(KeyEvent e) {
		parent.getGraphView().setResultUpToDate(false);
		for (PPanel p : panels) {
			p.updateMe(false);
		}
	}
	
	public void newGraph() {
		panels.removeAllElements();		
	}
	public void recalculate() {
		for (PPanel p : panels) {
			p.updateMe(true);
		}
		revalidate();
		repaint();		
	}
	
	private void refresh(boolean showInfo) {
		jcbCorObject.removeAllItems();
		int dim = parent.getGraphView().getNL().getNodes().size();
		String[] matrices = RControl.getR().eval("gMCP:::getAllCorrelationMatrices(n="+dim+")").asRChar().getData();
		if (showInfo && !Configuration.getInstance().getClassProperty(this.getClass(), "showRefreshInfo", "yes").equals("no")) {
			JCheckBox tellMeAgain = new JCheckBox("Don't show me this info again.");
			int n = (matrices.length==1 && matrices[0].endsWith("matrices found."))?0:+matrices.length;
			String message = "Searched and found "+n+" correlation "+((n==1)?" matrix":" matrices")+" of\n" +
					"dimension "+dim+"x"+dim+" in the R global environment.";
			JOptionPane.showMessageDialog(parent, new Object[] {message, tellMeAgain}, "Info", JOptionPane.INFORMATION_MESSAGE);
			if (tellMeAgain.isSelected()) {
				Configuration.getInstance().setClassProperty(this.getClass(), "showRefreshInfo", "no");
			}
		}
		if (matrices.length==1 && matrices[0].endsWith("matrices found.")) {
			/*if(jrbRCorrelation.isSelected()) {
				jrbNoCorrelation.setSelected(true);
			}*/
			jcbCorObject.setEnabled(false);
			jrbRCorrelation.setEnabled(false);			
		} else {				
			jcbCorObject.setEnabled(true);
			jrbRCorrelation.setEnabled(true);
		}
		for (String s : matrices) {
			jcbCorObject.addItem(s);
		}
	}
    public void removePPanel(Node node) {
		for (int i=panels.size()-1;i>=0;i--) {
			if (panels.get(i).node==node) {
				panels.remove(i);
				//logger.debug("Removed panel for node "+node.getName());
			}
		}
		setUp();		
	}
    public void renameNode(int i, String name) {
		panels.get(i).label.setText(name);		
	}

    public void restorePValues() {
		String debug = "Restoring PValues: ";
		if (pValues != null) {
			for (int i=0; i<pValues.size(); i++) {
				if (i<panels.size()) {
					panels.get(i).setP(pValues.get(i));
					debug += Configuration.getInstance().getGeneralConfig().getDecFormat().format(pValues.get(i))+"; ";
				}
			}
		}
		logger.debug(debug);
	}
    
    public void savePValues() {
		String debug = "Saving PValues: ";
		pValues = new Vector<Double>();
		for (PPanel panel : panels) {
			/*TODO The following line is a work-around for the following problem:
			 * If I insert pvalues with the middle mouse button no keyTyped event has been raised.
			 * Also InputMethodListener does not work. 
			 */
			panel.keyTyped(null);
			pValues.add(panel.getP());
			debug += Configuration.getInstance().getGeneralConfig().getDecFormat().format(panel.getP())+"; ";
		}
		logger.debug(debug);
	}
    
	public void setPValues(double[] pvalues) {
		setPValues(ArrayUtils.toObject(pvalues));
	}

	public void setPValues(Double[] pvalues) {
		pValues = Arrays.asList(pvalues);
		restorePValues();
	}

	public void setTesting(boolean b) {
		Configuration.getInstance().getClassProperty(this.getClass(), totalAlpha.getText());
		PPanel.setTesting(b);
		for (PPanel p : panels) {
			p.updateMe(true);
		}
		totalAlpha.setEditable(!b);
		//if (b) {
		//	weightLabel.setText("α Level");
		//} else {
		//	weightLabel.setText("Weights");
		//}
		refresh.setEnabled(!b);
		createMatrix.setEnabled(!b);
		
		jrbNoCorrelation.setEnabled(!b);
		jrbRCorrelation.setEnabled(!b);
	    jrbSimes.setEnabled(!b);

	    jcbCorObject.setEnabled(!b);
	    jbLoadPValues.setEnabled(!b);
	    if (!b) refresh(false);
	}

	JLabel subGraphLabel = new JLabel();
	
	public void setUp() {
		JPanel panel = new JPanel();
		
		String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";
        for (PPanel p : panels) {
        	rows += ", pref, 5dlu";
        }
        for (int i=0; i<parent.getGraphView().getNumberOfLayers(); i++) {
        	cols += ", fill:pref:grow, 5dlu";
        }
        if (parent.getGraphView().getNumberOfLayers()>1) {
        	rows += ", pref, 5dlu";
        }
        
        FormLayout layout = new FormLayout(cols, rows);
        panel.setLayout(layout);        
		
    	panel.add(new JLabel("Hypothesis"), cc.xy(2, 2));
    	
    	panel.add(weightLabel, cc.xy(4, 2));
    	//panel.add(new JLabel("Signif. Level"), cc.xy(4, 2));
    	
    	int col = 6;
    	
    	for (int i=1; i<parent.getGraphView().getNumberOfLayers(); i++) {
    		panel.add(new JLabel("Weights "+(i+1)), cc.xy(col, 2));
    		col += 2;
    	}
    	
    	panel.add(new JLabel("P-Value"), cc.xy(col, 2));
				
		int row = 4;
		for (PPanel p : panels) {
			int col2=2;
			for (Component c : p.getComponent()) {
				panel.add(c, cc.xy(col2, row));	
				col2 += 2;
			}
			row += 2;
		}		
		panel.add(statusLabel, cc.xyw(2, row, col-3));
		panel.add(jbLoadPValues, cc.xy(col, row));
		row += 2;
		panel.add(alphaLabel, cc.xy(2, row));    	
    	panel.add(totalAlpha, cc.xy(4, row));
    	//totalAlpha.setText("0.05");
    	totalAlpha.addKeyListener(this);
    	
    	if (parent.getGraphView().getNumberOfLayers()>1) {
    		row += 2;
    		panel.add(new JLabel("Component graph weights: "), cc.xy(2, row));
        	    
    		for (int i=entangledWeights.size(); i<parent.getGraphView().getNumberOfLayers(); i++) {
    			RealTextField tf = new RealTextField("totalAlpha", 0, 1);
    			tf.setText("0");
    			tf.addKeyListener(this);
    			entangledWeights.add(tf);
    		}
    		
    		col = 4;
        	for (int i=0; i<parent.getGraphView().getNumberOfLayers(); i++) {        		
        		panel.add(entangledWeights.get(i), cc.xy(col, row));
        		col += 2;
        	}
        	row += 2;
        	panel.add(subGraphLabel, cc.xyw(2, row, 5));
    	}
    	
    	updateLabels();
		panel.revalidate();		
		removeAll();		
		add(panel, c);
		c.gridy++;
		add(getCorrelatedPanel(), c);
		revalidate();
	}

	public void updateLabels() {
		statusLabel.setForeground(Color.BLACK);
		String text = "Sum of weights: ";
		if (parent.getGraphView().getNumberOfLayers()>1) text = "Subgraph sum of weights: ";
		for (int i=0; i<parent.getGraphView().getNumberOfLayers(); i++) {
			double weight = 0;
			for (PPanel p : panels) {
				if (!p.rejected) {
					weight += p.w.get(i);
				}
			}
			if (weight>1.0001) {
				statusLabel.setForeground(Color.RED);
			}
			text += Configuration.getInstance().getGeneralConfig().getDecFormat().format(weight)+"; ";
		}
		text = text.substring(0, text.length()-2);
		statusLabel.setText(text);
		subGraphLabel.setForeground(Color.BLACK);
		subGraphLabel.setText("");
		double weight = 0;
		if (entangledWeights.size()>0) {
			try {
				for (int i=0; i<entangledWeights.size(); i++) {
					weight += Double.parseDouble(entangledWeights.get(i).getText());
				}
				if (Math.abs(1-weight)>0.0001) {
					subGraphLabel.setForeground(Color.RED);
					subGraphLabel.setText("Component graph weights do not sum up to 1."); // We don't have to much space here, so we drop: " but instead to "+weight+".");
				}
			} catch (NumberFormatException nfe) {
				subGraphLabel.setForeground(Color.RED);
				subGraphLabel.setText("Component graph weights could not be parsed.");
			}
		}		
		for (int i=0; i<entangledWeights.size(); i++) {
			parent.getGraphView().getNL().setEntangledLegendWeight(i, entangledWeights.get(i).getText());
		}
	}

	public void setEntangledWeights(double[] ew) {
		/*for (int i = entangledWeights.size(); i<ew.length; i++) {
			entangledWeights.add(new JTextField());
		}*/
		for (int i=0; i<ew.length; i++) {
			entangledWeights.get(i).setText(""+ew[i]);
		}	
		updateLabels();
	}

	public void addEntangledLayer() {
		for (PPanel p : panels) {
			p.addEntangledLayer();
		}
		weightLabel.setText("Weights 1");
		setUp();		
	}

	/**
	 * Removes entangled layer
	 * @param layer Counting starts from 0 (not from 1).
	 */
	public void removeEntangledLayer(int layer) {
		for (PPanel p : panels) {
			p.removeEntangledLayer(layer);
		}
		if (parent.getGraphView().getNumberOfLayers()==1) {
			weightLabel.setText("Weights");
		}
		setUp();		
	}

	public void setCorrelation(String correlation) {		
		refresh(false);
		jcbCorObject.setSelectedItem(correlation);
	}

	/*public String getAlpha() {
		int layers = parent.getGraphView().getNumberOfLayers();
		if (layers==1) return ""+getTotalAlpha();
		String weights = "c(";
		for (int i=0; i<layers; i++) {
			weights += entangledWeights.get(i).getText();
			if (i!=layers-1) {				
				weights += ", ";
			}
		}
		return weights+")*"+getTotalAlpha();
	}*/
	
}
