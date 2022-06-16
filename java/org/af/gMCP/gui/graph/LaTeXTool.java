package org.af.gMCP.gui.graph;

import java.awt.Component;

import javax.swing.JFrame;
import javax.swing.JPanel;

import org.af.gMCP.config.Configuration;
import org.scilab.forge.jlatexmath.TeXConstants;
import org.scilab.forge.jlatexmath.TeXFormula;
import org.scilab.forge.jlatexmath.TeXIcon;

public class LaTeXTool {

	/**
	 * Replaces certain symbols in a String with LaTeX markup commands.
	 * For example: 
	 * "3E-6"      ->   "\frac{3}{10^{6}}"
	 * "(1+2ε)/3"  ->   "\frac{{(1+2ε)}}{3}"
	 * @param s
	 * @return
	 */
	public static String StringToLaTeX(String s) {
		String latex = "";
		if (s.indexOf("E-")!=-1) {
			latex = s.replaceAll("E-", "}{10^{");
			latex = "\\frac{"+latex+"}}";
		} else {
			int openBracket = 0;
			boolean waitingForDenominator = false;
			String nominator = "";			
			s.replaceAll("ε", "\\varepsilon");	
			s.replaceAll(" ", "");
			for (int i=0;i<s.length(); i++) {
				String c = ""+s.charAt(i);	
				if (c.equals("(")) openBracket++;				
				if (c.equals(")")) openBracket--;				
				if ( (c.equals("+") || c.equals("-") || c.equals("*") || 
						(c.equals(")") &&  (i+1)<s.length() && !(s.charAt(i+1)+"").equals("/")) ) && openBracket == 0) {
					String start = s.substring(0, i+1);										
					if (waitingForDenominator) {
						if (c.equals(")")) {
							latex += "\\frac{"+nominator+"}{"+start+"}";
						} else {
							latex += "\\frac{"+nominator+"}{"+start.substring(0, i)+"}"+c;
						}
						waitingForDenominator = false;
					} else {
						latex += start;
					}
					s = s.substring(i+1, s.length());
					i=-1;
				}
				if (c.equals("/")) {					
					nominator = s.substring(0, i);
					s = s.substring(i+1, s.length());
					i=-1;
					waitingForDenominator = true;
				}
			}
			if (waitingForDenominator) {
				latex += "\\frac{"+nominator+"}{"+s+"}";				
			} else {
				latex += s;
			}			
			latex = latex.replaceAll("\\*", Configuration.getInstance().getGeneralConfig().getTimesSymbol());			
			latex = latex.replaceAll("\\(", "{(");
			latex = latex.replaceAll("\\)", ")}");
		}
		//logger.debug("LaTeX string:"+latex);	
		return latex;
	}
	
	/**
	 * This function takes a string and creates a TeXIcon from this.
	 * @param s String to be parsed.
	 * @return
	 */
	public static TeXIcon getTeXIcon(JFrame parent, String s, int points) {
		try {	
			String latex = StringToLaTeX(s);
			TeXFormula formula = new TeXFormula(latex);//
			formula = new TeXFormula("\\mathbf{"+latex+"}");		
			TeXIcon result = formula.createTeXIcon(TeXConstants.ALIGN_CENTER, points);
			// TODO What about getIconHeight()/
			if (result.getIconWidth()>60) {
				result = formula.createTeXIcon(TeXConstants.ALIGN_CENTER, (int) (points*0.7));
			}
			//if (latex.indexOf("frac")==-1 && latex.length()>4) points = (int) (points*0.7);
			return result;
		} catch(Exception e) {
			//e.printStackTrace();
			//System.out.println("Error: "+latex);
			//TODO This is not allowed while painting:
			//JOptionPane.showMessageDialog(parent, "Invalid weight string:\n"+latex+"\nError:\n"+e.getMessage(), "Invalid input", JOptionPane.ERROR_MESSAGE);
			TeXFormula formula = new TeXFormula("Syntax Error");
			return formula.createTeXIcon(TeXConstants.ALIGN_CENTER, (int) (points*0.7)); 
		}		
	}

	public static Component panel = new JPanel();
	
	public static void main(String[] args) {
		System.out.println(StringToLaTeX("3E-6"));
		System.out.println(StringToLaTeX("(1+2ε)/3"));
		System.out.println(LaTeX2UTF("2*\\nu=\\epsilon"));
		System.out.println(sanitize("Special characters: {,},_,^,%,&,$."));
	}
	
	public static final String[] greekLaTeX = {
		"alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", 
		"theta", "iota", "kappa", "lambda", "mu", "nu", "xi", 
		/*"omicron",*/ "pi", "rho", "sigma", "tau", "upsilon", "phi",
		"chi", "psi", "omega"
	};
	
	public static final char[] greek = {
			'α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 
			'θ', 'ι', 'κ', 'λ', 'μ', 'ν', 'ξ', 
			/*'ο',*/ 'π', 'ρ', 'σ', 'τ', 'υ', 'φ',
			'χ', 'ψ', 'ω'
	};
	
	public final static String NO_GREEK_CHARACTER = "No greek character";
	
	public static String LaTeX2UTF(String s) {
		for (int i=0; i<greek.length; i++) {
			s = s.replaceAll("\\\\"+greekLaTeX[i], ""+greek[i]);
		}
		return s;
	}
	
	public static String UTF2LaTeX(char greekC) {
		for (int i=0; i<greek.length; i++) {
			if (greekC == greek[i]) return greekLaTeX[i];
		}
		return ""+greekC;// NO_GREEK_CHARACTER;
	}
	
	public static String UTF2LaTeX(String s) {		
		for (int i=0; i<greek.length; i++) {
			s = s.replaceAll(""+greek[i], greekLaTeX[i]);
		}
		return s;
	}

	//TODO If the character is already escaped, this might cause problems... Therefore check for escape sequence?
	public static String sanitize(String latex) {		
		//latex = latex.replaceAll("\\}", "\\}");
		//latex = latex.replaceAll("\\{", "\\{");		
		//latex = latex.replaceAll("\\^", "\\\\^");
		latex = latex.replaceAll("\\%", "\\\\%");
		latex = latex.replaceAll("\\&", "\\\\&");
		latex = latex.replaceAll("\\$", "\\\\\\$");		
		return latex;
	}

}
