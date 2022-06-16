package org.af.gMCP.gui.graph;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.text.DecimalFormat;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.af.gMCP.gui.RControl;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class PPanel implements ActionListener, KeyListener, NodeListener, FocusListener, DocumentListener {
	
	private static final Log logger = LogFactory.getLog(PPanel.class);
	
	double p = 0;
	List<Double> w;
	String name;
	
	JLabel label;
	/** JTextFields for weights of the different layers */
	private List<JTextField> wTFList = new Vector<JTextField>();
	/** JTextField for p-Value */
	private JTextField pTF;
	JButton jbReject;
	
	Node node;
	PView pview;
	Boolean rejected = false;
	DecimalFormat format = new DecimalFormat("#.######");//Configuration.getInstance().getGeneralConfig().getDecFormat();
	
	public Vector<Component> getComponent() {
		Vector<Component> v = new Vector<Component>();
		v.add(label);
		for (JTextField wTF : wTFList) {
			v.add(wTF);
		}
		v.add(pTF);
		v.add(jbReject);
		return v;
	}
	
	public PPanel(Node node, PView pview) {		
		node.addNodeListener(this);
		this.name = node.getName();
		this.w = node.getWeight();
		this.node = node;
		this.pview = pview;
        
        label = new JLabel(name);
		
        for (Double wd : w) {
        	JTextField wTF = new JTextField(RControl.getFraction(wd), 7);
        	wTF.addActionListener(this);
        	wTF.addFocusListener(this);
        	wTF.addKeyListener(this);
        	wTFList.add(wTF);
        }
		
		pTF = new JTextField(format.format(p), 7);
		pTF.addActionListener(this);
		pTF.addKeyListener(this);
		
		jbReject = new JButton("Reject and pass α");
		jbReject.setEnabled(false);
		jbReject.addActionListener(this);
		if (node.isRejected()) {
			reject();
		} else {
			updateMe(false);
		}
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==jbReject) {
			node.reject();			
		} else {
			updateMe(false);
		}
	}

	/**
	 *  
	 * @see org.af.gMCP.gui.graph.NodeListener#reject()
	 */
	public void reject() {
		/*for (JTextField wTF : wTFList) { //Already setEditable(false) in testing.
			wTF.setEnabled(false);
		}
		pTF.setEnabled(false);
		*/		
		jbReject.setEnabled(false);
		label.setText(label.getText()+" rejected!");
		label.setForeground(new Color(0,100,0));		
		rejected = true;
		pview.recalculate();
	}

	public void keyPressed(KeyEvent e) {keyTyped(e);}

	public void keyReleased(KeyEvent e) {keyTyped(e);}

	public void keyTyped(KeyEvent e) {
		try {
			p = Double.parseDouble(pTF.getText().replace(",", "."));
			pTF.setBackground(Color.WHITE);
		} catch (NumberFormatException nfe) {
			//logger.warn("Either \""+pTF.getText()+"\" or \""+pTF.getText()+"\" is no double number.");
			pTF.setBackground(Color.RED);
		}
		w = new Vector<Double>();
		for (JTextField wTF : wTFList) {
			try {
				if (wTF.getText().length()!=0) { /* This if-clause is due to a bug/version conflict in JHLIR/REngine/rJava/R for R 2.8 */
					double tempw = RControl.getR().eval(wTF.getText().replace(",", ".")).asRNumeric().getData()[0];
					w.add(tempw);
					if (!Double.isInfinite(tempw) && !Double.isNaN(tempw)) {
						wTF.setBackground(Color.WHITE);						
					} else {
						wTF.setBackground(Color.RED);
						return;
					}				
				} else {
					wTF.setBackground(Color.RED);
					w.add(Double.NaN);
					return;
				}
			} catch (Exception nfe) {		
				wTF.setBackground(Color.RED);
				w.add(Double.NaN);
				return;
			}	
		}
		node.setWeight(ArrayUtils.toPrimitive((Double[])w.toArray(new Double[0])), this);
		//logger.info("P: "+p+", W: "+w);
		updateMe(false);
	}

	/**
	 * Update the Panel, i.e.
	 * - calculate and show which nodes are rejectable
	 * - update the labels showing the total sum of weights
	 *   and possible warnings (like alpha or weight >1)
	 * - if (setText==true) set the p-values and weights
	 *   in the corresponding text fields. 
	 * @param setText Should the p-values and weights be updated in the corresponding text fields?
	 */
	void updateMe(boolean setText) {
		if (setText) {
			pTF.setText(format.format(p).replace(",", "."));
		}
		pTF.setEditable(!testing);
		for (JTextField wTF : wTFList) {
			wTF.setEditable(!testing);
		}
		for (int i=0; i<wTFList.size(); i++) {
			JTextField wTF = wTFList.get(i); 
			if (setText) {
				wTF.setText(getWString().get(i));
			}
		}
		
		double sumW = 0;
		if (w.size()==1) {
			sumW = w.get(0);
		} else {		
			for (int k=0; k<w.size(); k++)  {
				try {
					sumW += w.get(k) * Double.parseDouble(pview.entangledWeights.get(k).getText());
				} catch (Exception e) {
					//TODO Do we have to do anything here? I guess not.
				}
			}
		}
		try {
			if (p<=sumW*pview.getTotalAlpha(false)) {
				//logger.debug(""+p+"<="+sumW+"*"+pview.getTotalAlpha());
				node.setRejectable(true);
				pTF.setBackground(new Color(50, 255, 50));
				if (testing) {
					jbReject.setEnabled(!node.isRejected());
				} else  {
					jbReject.setEnabled(false);
				}
			} else {
				//logger.debug("NOT: "+p+"<="+sumW+"*"+pview.getTotalAlpha());
				node.setRejectable(false);
				pTF.setBackground(Color.WHITE);
				jbReject.setEnabled(false);
			}
		} catch (Exception e) {
			// Can not happen for warn = false.
		}
		pview.updateLabels();
	}

	public void updated(Node node) {		
		this.name = node.getName();
		this.w = node.getWeight();
		updateMe(true);
	}

	private Vector<String> getWString() {
		Vector<String> result = new Vector<String>();
		for (double wd : w) {
			if (testing) {
				result.add(format.format(wd/**pview.getTotalAlpha()*/).replace(",", "."));
			} else {
				if ( !Double.isInfinite(wd) && !Double.isNaN(wd) ) {
					result.add(""+wd);
				} else {
					result.add(RControl.getFraction(wd));
				}

			}		
		}
		return result;
	}

	public double getP() {		
		return p;
	}

	public void setP(double p) {
		this.p = p;
		updateMe(true);
	}	
	
	static boolean testing;
	
	public static void setTesting(boolean b) {
		testing = b;
	}

	public void focusGained(FocusEvent e) {	}

	public void focusLost(FocusEvent e) {
		 textFieldChange();
	}

	//TODO We have to clean up this lot of different listeners (doing different things?)
	
	public void addEntangledLayer() {
		JTextField wTF = new JTextField("0", 7);
    	wTF.addActionListener(this);
    	wTF.addFocusListener(this);
    	wTF.addKeyListener(this);
    	wTF.getDocument().addDocumentListener(this);
    	wTFList.add(wTF);
	}
	
	public void removeEntangledLayer(int layer) {
		wTFList.get(layer).removeActionListener(this);
		wTFList.remove(layer);
		/* We don't have to remove a value from variable w,
		 * since it will be updated via the NodeListener functionality.
		 */		
	}

	private void textFieldChange() {
		//System.out.println("TextFieldChange");
		if (wTFList.get(0).isEditable()) {
			keyTyped(null);
			for (int i=0; i<wTFList.size(); i++) {
				JTextField wTF = wTFList.get(i); 
				if (!testing) {
					if ( !Double.isInfinite(w.get(i)) && !Double.isNaN(w.get(i)) ) {
						wTF.setText(RControl.getFraction(w.get(i)));
					} else {
						wTF.setText(""+w.get(i));
					}
				}
				
			}
			updateMe(true);
		}
	}
	
	public void insertUpdate(DocumentEvent e) {
		textFieldChange();
	}


	public void removeUpdate(DocumentEvent e) {
		textFieldChange();
	}

	public void changedUpdate(DocumentEvent e) {
		textFieldChange();
	}

}
