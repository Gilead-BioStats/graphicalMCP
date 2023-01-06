package org.af.gMCP.gui.options;

import javax.swing.JPanel;

import org.af.commons.widgets.validate.ValidationException;

/**
 * This abstract Class ... 
 */

abstract class OptionsPanel extends JPanel {
    abstract protected void setProperties() throws ValidationException, SetPropertiesException;
}
