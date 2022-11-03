package org.af.gMCP.gui.contrasts;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.af.commons.Localizer;
import org.af.commons.widgets.WidgetFactory;
import org.af.commons.widgets.buttons.OkCancelButtonPane;
import org.af.jhlir.call.RDataFrame;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class StatMethodDialog extends JDialog implements ActionListener {

    protected static Localizer localizer = Localizer.getInstance();
    private List<Object> labels;
    private List<Component> components;
    protected RDataFrame df;
    private List<String> violatedConstraints = new ArrayList<String>();
    protected static final Log logger = LogFactory.getLog(StatMethodDialog.class);


    public StatMethodDialog(JFrame owner, String title, RDataFrame df) {
        super(owner, title);
        this.df = df;
        checkConstraints();
        if (violatedConstraints.size() > 0) {
            String msg = localizer.getString("SHARED_DIALOG_COULDNOTOPEN");
            for (int i = 0; i < violatedConstraints.size(); i++) {
                msg += (i + 1) + ".\n" + violatedConstraints.get(i)+"\n\n";
            }
            // throw new ConstraintViolation(msg);
        }
    }

     /**
     * Sets the content
     * @param labels Labels for components. Value null is allowed if no label is desired.
     * @param components Array of components which must have the same length as labels.
     */
    public void setContent(Object[] labels, Component[] components) {
        setContent(Arrays.asList(labels), Arrays.asList(components));
    }

    /**
     * Sets the content
     * @param labels Labels for components. Value null is allowed if no label is desired.
     * @param components List of components which must have the same length as labels.
     */
    public void setContent(List<Object> labels, List<Component> components) {
        this.labels = new Vector<Object>(labels);
        this.components = new Vector<Component>(components);
    }

    public void removeContent(Component comp) {
    	int nr = components.indexOf(comp);
    	labels.remove(nr);
    	components.remove(nr);
    }

    public void insertContent(int index, String label, Component comp) {
    	labels.add(index, label);
    	components.add(index, comp);
    }

    public void setup() {
        doTheLayout();
        pack();
        WidgetFactory.showCompleteTitle(this);
        setLocationRelativeTo(getOwner());
        setVisible(true);
    }

    protected void doTheLayout() {
        JPanel p = makeMainDialogPanel();
        p = WidgetFactory.makeDialogPanelWithButtons(p, this);
        setContentPane(p);
    }

    /**
     * This method will create a JPanel from List&lt;Object&gt; labels and
     * List&gt;Component&gt; components.
     *
     * @return JPanel which will be used as ContentPane of this StatMethodDialog.
     */
    private JPanel makeMainDialogPanel() {
        String cols = "5dlu, pref, 4dlu, pref:grow, 5dlu";
        String rows = "";
        for (int i = 0; i < components.size(); i++) {
        	if (components.get(i) instanceof JScrollPane) {
        		rows += "5dlu, fill:default:grow, ";
        	} else {
        		rows += "5dlu, pref, ";
        	}
        }
        rows += "5dlu";

        FormLayout layout = new FormLayout(cols, rows);

        JPanel panel = new JPanel(layout);
        CellConstraints cc = new CellConstraints();

        for (int i = 0; i < labels.size(); i++) {
            Object label = labels.get(i);
            Component comp = components.get(i);

            if (label != null) {
            	if (label instanceof Component) {
            		panel.add((Component)label, cc.xy(2, 2 + i * 2));
            	} else {
            		panel.add(new JLabel(label.toString()), cc.xy(2, 2 + i * 2));
            	}
            	panel.add(comp, cc.xy(4, 2 + i * 2));
            } else {
            	panel.add(comp, cc.xyw(2, 2 + i * 2, 3));
            }
        }

        return panel;
    }

    protected void onOk() throws RemoteException {}

    protected void onCancel() {
        dispose();
    }

    protected void checkConstraints() {}

    protected void requiresNumVar() {
        requiresNumVars(1);
    }

    protected void requiresIntVar() {
        requiresIntVars(1);
    }
    protected void requiresFactVar() {
        requiresFactVars(1);
    }

    protected void requiresNumVars(int n) {
        int k = df.getNumberVars().size();
        if (k < n) {
            if (n == 1)
                violatedConstraints.add(localizer.getString("SHARED_DIALOG_NUMVAR"));
            else
                violatedConstraints.add(localizer.getString("SHARED_DIALOG_NUMVARS1") + n + "\n" + localizer.getString("SHARED_DIALOG_NUMVARS2") + k);
        }
    }

    protected void requiresVars(int n) {
    	int k = df.getColumnCount();
    	if (k < n) {
            if (n == 1)
                violatedConstraints.add(localizer.getString("SHARED_DIALOG_VAR"));
            else
                violatedConstraints.add(localizer.getString("SHARED_DIALOG_VARS1") + n + "\n" + localizer.getString("SHARED_DIALOG_VARS2")+ k);
        }
    }

    protected void requiresFactVars(int n) {
        int k = df.getFactorVars().size();
        if (k < n) {
            if (n == 1)
               violatedConstraints.add(localizer.getString("SHARED_DIALOG_FACTORVAR"));
            else
                violatedConstraints.add(localizer.getString("SHARED_DIALOG_FACTORVARS1") + n + "\n" + localizer.getString("SHARED_DIALOG_FACTORVARS2") + k);
        }
    }

    protected void requiresIntVars(int n) {
        int k = df.getIntegerVars().size();
        if (k < n) {
        	if (n == 1)
        		violatedConstraints.add(localizer.getString("SHARED_DIALOG_INTVAR"));
        	else
        		violatedConstraints.add(localizer.getString("SHARED_DIALOG_INTVARS1") + n + "\n" + localizer.getString("SHARED_DIALOG_INTVARS2") + k);
        }
    }

    public void actionPerformed(ActionEvent e) {
        if (e.getActionCommand().equals(OkCancelButtonPane.OK_CMD)) {
            // todo think about where to catch this exception and where to report
            try {
                onOk();
            } catch (RemoteException e1) {
                e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
        }
        if (e.getActionCommand().equals(OkCancelButtonPane.CANCEL_CMD))
            onCancel();
    }
    
    public RDataFrame getDataFrame() {
        return df;
    }
}
