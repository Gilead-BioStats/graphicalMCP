package org.af.gMCP.config;

import javax.swing.UIManager;


public class JavaConfig extends SpecificConfig {

	/**
	 * Constructor - use Configuration.getInstance().getJavaConfig() to access it.
	 * @param conf JavaConfig object
	 */
    JavaConfig(Configuration conf) {
        super(conf);
    }
    
    public String getLooknFeel() {
        return getProperty("looknfeel", UIManager.getCrossPlatformLookAndFeelClassName()); // "com.jgoodies.looks.plastic.Plastic3DLookAndFeel");
    }

    public void setLooknFeel(String looknfeel) {
       setProperty("looknfeel", looknfeel);
    }

}
