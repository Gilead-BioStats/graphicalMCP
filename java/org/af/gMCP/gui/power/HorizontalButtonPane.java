package org.af.gMCP.gui.power;

//TODO Can be deleted as soon as afcommons include the JButton constructors.

import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;


/**
 * Horizontal pane with buttons.
 */
public class HorizontalButtonPane extends JPanel {
    protected final String[] cmds;
    protected final JButton[] buttons;
    protected final boolean rightSpace;

    public static final String OK_CMD = "OK";
    public static final String CANCEL_CMD = "CANCEL";
    public static final String APPLY_CMD = "APPLY";

    /**
     * Constructor
     *
     * @param captions List of captions for buttons.
     * @param cmds List of action command names for buttons.
     */
    public HorizontalButtonPane(List<String> captions, List<String> cmds) {
        this(captions.toArray(new String[1]), cmds.toArray(new String[1]));
    }

    /**
     * Constructor
     *
     * @param buttons List of buttons.
     */
    public HorizontalButtonPane(List<JButton> buttons, boolean rightSpace) {
        this(buttons.toArray(new JButton[1]), rightSpace);        
    }

    /**
     * Constructor
     *
     * @param buttons List of buttons.
     */
    public HorizontalButtonPane(JButton[] buttons, boolean rightSpace) {
        this.buttons = buttons;
        cmds = new String[buttons.length];
        this.rightSpace = rightSpace;
        setLayout();
    }
    
    /**
     * Constructor
     *
     * @param captions Array of captions for buttons.
     * @param cmds Array of action command names for buttons.
     */
    public HorizontalButtonPane(String[] captions, String[] cmds) {
        this.cmds = cmds;
        buttons = new JButton[cmds.length];
        for (int i = 0; i < buttons.length; i++) {
            buttons[i] = new JButton(captions[i]);
            buttons[i].setActionCommand(cmds[i]);
        }
        this.rightSpace = true;
        setLayout();
    }

    private void setLayout() {
    	setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
    	add(Box.createHorizontalGlue());
        for (int i = 0; i < buttons.length; i++) {
            add(buttons[i]);
            if (rightSpace || i!=buttons.length-1) add(Box.createHorizontalStrut(5));
        }
	}

	/**
     * Add an action listener to all buttons.
     *
     * @param al The ActionListener.
     */
    public void addActionListener(ActionListener al) {
        for (JButton b : buttons) {
            b.addActionListener(al);
        }
    }

    /**
     * Enable/Disable all buttons.
     *
     * @param enabled True if buttons should be enabled.
     */
    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        for (JButton b : buttons) {
            b.setEnabled(enabled);
        }
    }

    /**
     * Enable/disable the ith button.
     *
     * @param i       Index of button.
     * @param enabled True if button should be enabled.
     */
    public void setEnabled(int i, boolean enabled) {
        buttons[i].setEnabled(enabled);
    }

    /**
     * Set the caption of the ith button.
     *
     * @param i    Index of button.
     * @param text Text for button.
     */
    public void setText(int i, String text) {
        buttons[i].setText(text);
    }
}

