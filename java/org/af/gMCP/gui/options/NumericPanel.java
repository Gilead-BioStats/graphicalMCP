package org.af.gMCP.gui.options;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.af.commons.widgets.validate.ValidationException;
import org.af.gMCP.config.Configuration;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

/**
 * OptionsPanel for plot settings.
 */
public class NumericPanel extends OptionsPanel implements ActionListener { 

    private JCheckBox useEpsApprox;
    private JTextField jtfEps;
    private JCheckBox tryToSimplify;
    private JTextField jtfDigits;
    private JCheckBox verbose;
    private Configuration conf;
    private JTextField numberOfSimulations;
    private JComboBox randomNumbers;
    private JComboBox upscale;
    private JCheckBox useSeed;
    private JTextField seed;
    private JLabel seedLabel = new JLabel("Random number seed:");

    public NumericPanel(Configuration conf) {
        this.conf = conf;

        makeComponents();
        doTheLayout();
    }


    private void makeComponents() {
        useEpsApprox = new JCheckBox("Use epsilon approximation");
        useEpsApprox.setSelected(conf.getGeneralConfig().useEpsApprox());
        useEpsApprox.addActionListener(this);
        useEpsApprox.setEnabled(false);
        useEpsApprox.setToolTipText("<html>" +
        		"In this version this option can not be changed.<br>" +
        		"No calculations with infinitesimal small values are done<br>" +
        		"but instead the epsilon is approximated by a small real number." +
        		"</html>");
        
        jtfEps = new JTextField(30);
        jtfEps.setText(""+conf.getGeneralConfig().getEpsilon()); 
        jtfEps.setEnabled(conf.getGeneralConfig().useEpsApprox());
        jtfEps.setToolTipText("<html>" +
        		"The small real value that should be used to approximate<br>" +
        		"the infinitesimal small epsilon. Default is 0.001.</html>");
        
        tryToSimplify = new JCheckBox("Try to show fractions / rounded numbers");
        tryToSimplify.setSelected(conf.getGeneralConfig().simplify());
        tryToSimplify.addActionListener(this);
        tryToSimplify.setEnabled(false);
        tryToSimplify.setToolTipText("<html>" +
        		"</html>");
        
        jtfDigits = new JTextField(30);
        jtfDigits.setText(""+conf.getGeneralConfig().getDigits2()); 
        jtfDigits.setEnabled(conf.getGeneralConfig().simplify());
        jtfDigits.setToolTipText("<html>" +
        		"</html>");
        
        verbose = new JCheckBox("Verbose output of algorithms");
        verbose.setSelected(conf.getGeneralConfig().verbose());
        verbose.setToolTipText("<html>" +
        		"If selected the algorithms produce a verbose<br>" +
        		"output that is shown in the GUI. For example the Simes<br>" +
        		"test specifies for each intersection of elementar hypotheses<br>" +
        		"whether and why it could be rejected.</html>");
        
        numberOfSimulations = new JTextField(30);
        numberOfSimulations.setText(""+conf.getGeneralConfig().getNumberOfSimulations());
        numberOfSimulations.setToolTipText("<html>" +
        		"The Monte Carlo sample size for power calculations.<br>" +
        		"Default is 10000.</html>");
        
        randomNumbers = new JComboBox(new String[] {"quasirandom", "pseudorandom"});
        randomNumbers.setSelectedIndex(conf.getGeneralConfig().getTypeOfRandom().equals("quasirandom")?0:1);
        randomNumbers.setToolTipText("<html>" +
        		"You can select quasirandom or pseudorandom numbers for<br>" +
        		"power calculations. The quasirandom option uses a randomized<br>" +
        		"Lattice rule, and should be more efficient than the<br>" +
        		"pseudorandom option that uses ordinary (pseudo) random numbers.</html>");
        
        upscale = new JComboBox(new String[] {"Yes", "No"});
        upscale.setSelectedIndex(conf.getGeneralConfig().getUpscale()?0:1);
        upscale.setToolTipText("<html>" +
        		"If 'No' is selected then for each intersection of hypotheses (i.e. each subgraph)<br>" +
        		"a weighted test is performed at the possibly reduced level alpha of sum(w)*alpha,<br>" + 
        		"where sum(w) is the sum of all node weights in this subset.<br>" +
        		"If 'Yes' is selected all weights are upscaled, so that sum(w)=1.<br>" +
        		"Please see the manual for a longer explanation and examples.</html>");
        
        useSeed = new JCheckBox("Use random number seed");
        useSeed.setSelected(conf.getGeneralConfig().useSeed());
        useSeed.addActionListener(this);        
        useSeed.setToolTipText("<html>" +
        		"If selected a user specified seed is used to set the random number<br>"+
                "generator state to the specified value. This way all calculations<br>"+
                "involving random numbers give a reproducable result." +
        		"</html>");
        
        seed = new JTextField(30);
        seed.setEnabled(useSeed.isSelected());
        seedLabel.setEnabled(useSeed.isSelected());
        seed.setText(""+conf.getGeneralConfig().getSeed());
        seed.setToolTipText("<html>" +
        		"Integer seed value to use for random number generation."+
        		"</html>");
        
        
    }

    private void doTheLayout() {
        JPanel p1 = new JPanel();

        String cols = "pref, 5dlu, fill:pref:grow";
        String rows = "pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref";
        FormLayout layout = new FormLayout(cols, rows);

        p1.setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 1;
        
        p1.add(useEpsApprox, cc.xyw(1, row, 3));
        
        row += 2;
        
        p1.add(new JLabel("Epsilon:"),     cc.xy(1, row));
        p1.add(jtfEps, cc.xy(3, row));        
        
        /*TODO Enable if needed:
        row += 2;
        
        p1.add(tryToSimplify, cc.xyw(1, row, 3));
        
        row += 2; parametricAlgo = new JComboBox(new String[] {"Bretz2011", "simple-parametric"});
        
        p1.add(new JLabel("Number of digits to assure:"),     cc.xy(1, row));
        p1.add(jtfDigits, cc.xy(3, row));
        */        
        
        row += 2;
        
        p1.add(verbose, cc.xyw(1, row, 3));  

        row += 2;
        
        p1.add(new JLabel("Monte Carlo sample size for power:"),     cc.xy(1, row));
        p1.add(numberOfSimulations, cc.xy(3, row));        
        
        row += 2;
        
        p1.add( new JLabel("Type of random numbers:"),     cc.xy(1, row));
        p1.add(randomNumbers, cc.xy(3, row));        
        
        row += 2;
        
        p1.add( new JLabel("Weights of subgraphs are upscaled to 1:"),     cc.xy(1, row));
        p1.add(upscale, cc.xy(3, row));        
        
        add(p1);
        
        row += 2;
        
        p1.add(useSeed, cc.xyw(1, row, 3));
        
        row += 2;
        
        p1.add(seedLabel,     cc.xy(1, row));
        p1.add(seed, cc.xy(3, row));   
    }
    
    public void setProperties() throws ValidationException {
       	conf.getGeneralConfig().setVerbose(verbose.isSelected());
       	conf.getGeneralConfig().setUseEpsApprox(useEpsApprox.isSelected());
       	try {
        	double eps = Double.parseDouble(jtfEps.getText());
        	conf.getGeneralConfig().setEps(eps);
        } catch (NumberFormatException e) {
        	JOptionPane.showMessageDialog(this, "\""+jtfEps.getText()+"\" is not a valid double for epsilon.", "Invalid input", JOptionPane.ERROR_MESSAGE);
        }
       	conf.getGeneralConfig().setSimplify(tryToSimplify.isSelected());
       	try {
        	int nr = Integer.parseInt(jtfDigits.getText());
        	conf.getGeneralConfig().setDigits2(nr);
        } catch (NumberFormatException e) {
        	JOptionPane.showMessageDialog(this, "\""+jtfDigits.getText()+"\" is not a valid integer.", "Invalid input", JOptionPane.ERROR_MESSAGE);
        }
       	try {
        	conf.getGeneralConfig().setNumberOfSimulations(Integer.parseInt(numberOfSimulations.getText()));
        } catch (NumberFormatException e) {
        	JOptionPane.showMessageDialog(this, "\""+numberOfSimulations.getText()+"\" is not a valid integer.", "Invalid input", JOptionPane.ERROR_MESSAGE);
        }
       	conf.getGeneralConfig().setTypeOfRandom(randomNumbers.getSelectedItem().toString());
       	conf.getGeneralConfig().setUpscale(upscale.getSelectedIndex()==0);
       	conf.getGeneralConfig().setUseSeed(useSeed.isSelected());
       	try {
       		conf.getGeneralConfig().setSeed(Integer.parseInt(seed.getText()));
        } catch (NumberFormatException e) {
        	JOptionPane.showMessageDialog(this, "\""+seed.getText()+"\" is not a valid integer.", "Invalid input", JOptionPane.ERROR_MESSAGE);
        }
    }

	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==useEpsApprox) {
			jtfEps.setEnabled(useEpsApprox.isSelected());
		}
		if (e.getSource()==useSeed) {
			seed.setEnabled(useSeed.isSelected());
			seedLabel.setEnabled(useSeed.isSelected());
		}
	}
}
