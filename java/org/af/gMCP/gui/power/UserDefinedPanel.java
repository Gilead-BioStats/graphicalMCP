package org.af.gMCP.gui.power;

import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Vector;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.text.BadLocationException;

import org.af.gMCP.gui.graph.Node;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class UserDefinedPanel extends JPanel implements ActionListener {
	
	private static final String K_OUT_OF_N = "k out of n";
	List<JButton> buttons = new Vector<JButton>();
	List<JButton> buttons2 = new Vector<JButton>();  
	
	JTextField jtUserDefined = new JTextField();
	JButton addAnother = new JButton("Add another power function");    
    JButton clearList = new JButton("Clear");
    
	DefaultListModel listModel;
    JList listUserDefined;

    JTextArea jta = new JTextArea();
    
    JButton loadUDPF = new JButton("Load");
    JButton saveUDPF = new JButton("Save");
    
    List<Node> nodes;
    
    JDialog parent;
    boolean justOne;
	
	public UserDefinedPanel(PDialog parent, Vector<Node> nodes) {
		this(parent, nodes, false);
	}
    
	public UserDefinedPanel(JDialog parent, List<Node> nodes, boolean justOne) {		
		this.nodes = nodes;
		this.parent = parent;
		this.justOne = justOne;
		
		JButton b = new JButton("(");
		b.setActionCommand("(");
		buttons.add(b);

		b = new JButton(")");
		b.setActionCommand(")");
		buttons.add(b);
		
		b = new JButton("AND");
		b.setActionCommand("&&");
		buttons.add(b);
		
		b = new JButton("OR");
		b.setActionCommand("||");
		buttons.add(b);
		
		b = new JButton("NOT");
		b.setActionCommand("!");		
		buttons.add(b);		
		
		b = new JButton("At least k out of n");
		b.setActionCommand(K_OUT_OF_N);		
		buttons.add(b);
		
		/*b = new JButton("Weighted mean");
		b.setActionCommand("!");		
		buttons.add(b);*/	
		
		for (int i=0; i<nodes.size(); i++) {
			b = new JButton(nodes.get(i).getName());
			b.setActionCommand("x["+(i+1)+"]");			
			buttons2.add(b);
		}
		
		JPanel hypPanel = new JPanel();
		for (JButton button : buttons2) {
			button.addActionListener(this);
			hypPanel.add(button);
		}
		
		JPanel opPanel = new JPanel();
		for (JButton button : buttons) {
			button.addActionListener(this);
			opPanel.add(button);
		}
		
		jta.setMargin(new Insets(4,4,4,4));
		jta.setText(
				"In the text field above you can enter an user defined power function.\n" +
				"Use the R syntax and \"x[i]\" to specify the proposition that hypothesis i\n"+
				"could be rejected. Alternatively use the buttons below.\n" +
				"Example:  (x[1] && x[2]) || x[4]\n" +
				"This calculates the probability that the first and second\n" +
				"or (not exclusive) the fourth null hypothesis can be rejected.\n"+
				/*"- if the test statistic follows a t-distribution, enter the non-centrality parameter µ*sqrt(n)/σ\n"+
				"  (µ=difference of real mean and mean under null hypothesis, n=sample size, σ=standard deviation)\n"+
				"- triangle(min, peak, max)\n"+
				"- rnorm(1, mean=0.5, sd=1)\n"+*/
				"(Negation (!) takes precedence over 'and' (&&), which takes precedence over 'or' (||).\n"+
				"In doubt use brackets.)\n"+
				"Note that you can use all R commands, for example\n"+
				"any(x) to see whether any hypotheses was rejected or\n" +
				"all(x[1:4]) to see whether all of the first four hypotheses were rejected."+				
				(justOne?"":"\nHit return to add another power function."));
		

        String cols = "5dlu, fill:pref:grow, 5dlu, fill:pref:grow, 5dlu";
        String rows = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";
        
        setLayout(new FormLayout(cols, rows));
        CellConstraints cc = new CellConstraints();
		
		int row = 2;

		if (justOne) { 
			add(jtUserDefined, cc.xyw(2, row, 3));
		} else {
			jtUserDefined.addActionListener(this); // We really don't want the action listener in the other case.
			add(jtUserDefined, cc.xy(2, row));	

			addAnother.addActionListener(this);
			add(addAnother, cc.xy(4, row));
		}
		
		/*clearList.addActionListener(this);
		mPanel.add(addAnother, cc.xy(4, row));*/		
		
		row +=2;
		
		listModel = new DefaultListModel();
		listUserDefined = new JList(listModel);
		
		if (justOne) {
			add(new JScrollPane(jta), cc.xywh(2, row, 3, 3));
		} else {
			add(new JScrollPane(jta), cc.xywh(2, row, 1, 3));
			add(new JScrollPane(listUserDefined), cc.xy(4, row));
		}		
	
		row +=2;
		
		clearList.addActionListener(this);
		if (!justOne) add(clearList, cc.xy(4, row));		
		//mPanel.add(saveUDPF, cc.xy(6, row));
		
		row +=2;		
				
		add(new JScrollPane(hypPanel), cc.xyw(2, row, 3));

		row +=2;
		
		add(new JScrollPane(opPanel), cc.xyw(2, row, 3));
		
	}

	/**
	 * Constructs String that contains the parameter f for user defined
	 * functions used by calcPower and extractPower
	 * @return If justOne==false: String that contains the parameter f for user defined
	 * functions used by calcPower and extractPower. Either empty or
	 * of the form ", f=list(...)".
	 * Otherwise if justOne==true: Body of the function, e.g. x[1] && x[2]  
	 * @param single 
	 */
	public String getUserDefined() {
		if (justOne) {
			/*if (listModel.getSize()==1) {
				return ""+listModel.getElementAt(0);
			}*/
			return jtUserDefined.getText();			
		}
		//if (listModel.getSize()==0) return "list()";
		String s = "list(";
		for (int i=0; i<listModel.getSize(); i++) {
			s +="'"+listModel.get(i)+"'=function(x) {"+listModel.get(i)+"}";
			if (i!=listModel.getSize()-1) s+= ",\n          ";
		}		
		return s + ")";
	}

	public void actionPerformed(ActionEvent e) {
		if (e!=null && (buttons.contains(e.getSource()) || buttons2.contains(e.getSource()))) {
			String command = ((JButton)e.getSource()).getActionCommand();
			if (command.equals(K_OUT_OF_N)) {
				KoutofNDialog koon = new KoutofNDialog(parent, nodes);				
				command = koon.getCommand();
			} 
			try {
				jtUserDefined.getDocument().insertString(jtUserDefined.getCaretPosition(), command, null);
			} catch (BadLocationException e1) {					
				jtUserDefined.setText(jtUserDefined.getText()+" "+command);
			}
			return;
		}
		if (e!=null && e.getSource() == clearList) {
			listModel.removeAllElements();
			return;
		}	
		if (jtUserDefined.getText().length()>0) {
			listModel.addElement(jtUserDefined.getText());
			//listUserDefined.ensureIndexIsVisible(0);
			jtUserDefined.setText("");
		}
	}

	public Element getConfigNode(Document document) {
		Element e = document.createElement("powerfunctions");
		for (int i=0; i<listModel.getSize(); i++) {
			Element ef = document.createElement("userdefined");
			ef.setAttribute("expression", ""+listModel.get(i));
			e.appendChild(ef);
		}
		return e;
	}

	public void loadConfig(Element e) {
		NodeList nlist = e.getChildNodes();
		int i = nlist.getLength()-1;
		while(listModel.getSize()<nlist.getLength()) {
			listModel.insertElementAt(((Element)nlist.item(i)).getAttribute("expression"), 0);
			i--;
		}			
	}

	

	
	    
}
