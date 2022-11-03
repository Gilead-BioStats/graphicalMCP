package org.af.gMCP.gui.graph;

import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;

import org.af.gMCP.gui.CreateGraphGUI;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class DView extends JTabbedPane {

	private static final Log logger = LogFactory.getLog(DView.class);
	CreateGraphGUI control;
	JPanel description = null;
	private JTextArea jtaDescription = null;
	JPanel analysis = null;
	JTextArea jtaAnalysis = null;
	public static final String defaultDescription = "Enter a description for the graph.";
	
	public DView(CreateGraphGUI control) {
		this.control = control;	
		setDescription(defaultDescription);
		setAnalysis("");
		setSelectedComponent(description);
	}
	
	public void setDescription(String s) {
		if (s == null && description != null) {
			//remove(description);
			//description = null;
			return;
		}
		if (description == null) {		
			description = new JPanel();
			jtaDescription = getTextArea(s, true);
			description.setLayout(new GridBagLayout());			
			GridBagConstraints c = getConstraints();
			JScrollPane sp = new JScrollPane(jtaDescription);
			sp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
			description.add(sp, c);
			insertTab("Description", null, description, "Description", 0);
			return;
		}
		jtaDescription.setText(s);
		jtaDescription.setCaretPosition(0);
	}

	public void setAnalysis(String s) {
		if (s == null && analysis != null) {
			//remove(analysis);
			//analysis = null;
			return;
		}
		if (analysis == null) {		
			analysis = new JPanel();
			analysis.setLayout(new GridBagLayout());
			jtaAnalysis = getTextArea(s, false);
			GridBagConstraints c = getConstraints();
			JScrollPane sp = new JScrollPane(jtaAnalysis);
			sp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
			analysis.add(sp, c);
			insertTab("Analysis", null, analysis, "Analysis", this.getComponentCount());
			return;
		}
		jtaAnalysis.setText(s);
		jtaAnalysis.setCaretPosition(0);
	}
	
	public JTextArea getTextArea(String s, boolean editable) {
		JTextArea jta = new JTextArea(s);
		jta.setFont(new Font("Monospaced", Font.PLAIN, 12));
		jta.setMargin(new Insets(5,5,5,5));
		jta.setLineWrap(true);
		jta.setEditable(editable);
		jta.setWrapStyleWord(true);
		jta.setCaretPosition(0);
		return jta;
	}
	
	public GridBagConstraints getConstraints() {
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;	
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=0; c.ipady=0;
		c.weightx=1; c.weighty=1;
		return c;
	}

	public String getDescription() {
		String descr = jtaDescription.getText();
		descr = descr.replaceAll("\\\\", "\\\\\\\\");
		descr = descr.replaceAll("\n", "\\\\n");
		descr = descr.replaceAll("\"", "\\\\\"");
		return descr;
	}

	
}
