package tests;

import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

public class OptionPaneTest extends JFrame {
	public static void main(String[] args) {
		JCheckBox tellMeAgain = new JCheckBox("Don't show me this info again.");
		String message = "This test is appropriate if the p-values\n" +
				"belong to one-sided test-statistics with a joint\n" +
				"multivariate normal null distribution.";
		JOptionPane.showMessageDialog(new JFrame(), new Object[] {message, tellMeAgain}, "Info", JOptionPane.INFORMATION_MESSAGE);
		
		/*JOptionPane.showOptionDialog(new JFrame(), , "Info", JOptionPane.OK_OPTION, JOptionPane.INFORMATION_MESSAGE, null, new Object[] {tellMeAgain}, null);*/
		System.out.println(tellMeAgain.isSelected());
	}

}
