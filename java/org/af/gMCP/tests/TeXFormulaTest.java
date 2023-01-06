package org.af.gMCP.tests;

import java.awt.Graphics;

import javax.swing.JFrame;
import javax.swing.JPanel;

import org.scilab.forge.jlatexmath.TeXConstants;
import org.scilab.forge.jlatexmath.TeXFormula;
import org.scilab.forge.jlatexmath.TeXIcon;

public class TeXFormulaTest extends JFrame {

	public TeXFormulaTest() {
		JPanel panel = new TeXPanel();
		getContentPane().add(panel);		
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		TeXFormulaTest f = new TeXFormulaTest();
		f.setSize(800, 600);
		f.setVisible(true);
	}
	
	

}


class TeXPanel extends JPanel {
	
	protected void paintComponent(Graphics g) {		
		System.out.println("Painting panel.");		
		TeXIcon icon = getTeXIcon("1+3/4*ε^2");
		icon = getTeXIcon("1-ε");
		icon.paintIcon(this, g,	100, 100);
	}
	
	/**
	 * This function takes a string and creates a TeXIcon from this.
	 * Things like "2^(1+2)" or even "2*2/4" will cause Exceptions or give false results.
	 * This function is only meant to be for polynomials. 
	 * @param s String to be parsed.
	 * @return
	 */
	private TeXIcon getTeXIcon(String s) {
		boolean print = true;		
		s.replaceAll("ε", "\\varepsilon");
		String latex = "";
		while (s.length()>0) {			
			int i = getNextOperator(s);
			if (i!=-1) {
				String op = ""+s.charAt(i);
				String start = s.substring(0, i);
				s = s.substring(i+1, s.length());
				if (op.equals("+") || op.equals("-") || op.equals("*")) {
					if (print) {
						latex += start;							
					}
					if (!op.equals("*")) {
						latex += op;
					} else {
						//formula.addSymbol("cdot");
					}					
					print = true;
				}
				if (op.equals("/")) {
					i = getNextOperator(s);
					String s2;
					if (i!=-1) {
						s2 = s.substring(0, i);
					} else {
						s2 = s;
					}
					if (op.equals("/")) {
						latex += "\\frac{"+start+"}{"+s2+"}";
					}
					print = false;
				}
			} else {
				if (print) {
					latex += s;
				}
				s = "";
			}
		}	
		TeXFormula formula = new TeXFormula(latex);//
		formula = new TeXFormula("\\mathbf{"+latex+"}");
		
		return formula.createTeXIcon(TeXConstants.ALIGN_CENTER, 16);
	}
	
	private static int getNextOperator(String s) {
		int min = s.length()+1;
		int i = s.indexOf("+");
		if (i!=-1) {
			min = i;
		}
		i = s.indexOf("-");
		if (i!=-1 && min>i) {
			min = i;
		}
		i = s.indexOf("*");
		if (i!=-1 && min>i) {
			min = i;
		}
		i = s.indexOf("/");
		if (i!=-1 && min>i) {
			min = i;
		}
		if (min==s.length()+1) return -1;
		return min;
	}
}