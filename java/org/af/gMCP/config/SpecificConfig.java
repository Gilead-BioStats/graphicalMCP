package org.af.gMCP.config;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

abstract public class SpecificConfig {
	protected static Log logger = LogFactory.getLog(SpecificConfig.class);
    private Configuration conf;

	/**
	 * Constructor
	 * @param conf SpecificConfig object
	 */
    public SpecificConfig(Configuration conf) {
        this.conf = conf;
    }

    protected Configuration getConf() {
        return conf;
    }

    protected String getProperty(String prop) {
        return getConf().getProperty(prop);
    }

    protected String getProperty(String prop, String def) {
        return getConf().getProperty(prop, def);
    }

    protected void setProperty(String prop, String val) {
        getConf().setProperty(prop, val);
    }

    protected int getIntProperty(String prop, String def){
        return Integer.parseInt(getConf().getProperty(prop, def));
    }

    protected double getDoubleProperty(String prop, String def){
        return Double.parseDouble(getConf().getProperty(prop, def));
    }

    protected boolean getBoolProperty(String prop, String def){
        return Boolean.parseBoolean(getConf().getProperty(prop, def));
    }

    protected void setIntProperty(String prop, int val) {
        setProperty(prop, ""+val);
    }

    protected void setDoubleProperty(String prop, double val) {
        setProperty(prop, ""+val);
    }

    protected void setBoolProperty(String prop, boolean val) {
        setProperty(prop, ""+val);
    }


}
