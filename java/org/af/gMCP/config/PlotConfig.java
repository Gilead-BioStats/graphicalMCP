package org.af.gMCP.config;

public class PlotConfig extends SpecificConfig {

	/**
	 * Constructor - use Configuration.getInstance().getPlotConfig() to access it.
	 * @param conf PlotConfig object
	 */
    PlotConfig(Configuration conf) {
        super(conf);
    }

    public String getFormat() {
        return getProperty("plot.format", "png");
    }

    public void setFormat(String format) {
       setProperty("plot.format", format);
    }

    public double getWidth() {
        return getDoubleProperty("plot.width", "600");
    }

    public void setWidth(double w) {
        setDoubleProperty("plot.width", w);
    }

    public double getHeight() {
        return getDoubleProperty("plot.height", "600");
    }

    public void setHeight(double h) {
        setDoubleProperty("plot.height", h);
    }
    
    public double getWidthInch() {
        return getDoubleProperty("plot.width.inch", "4");
    }

    public void setWidthInch(double w) {
        setDoubleProperty("plot.width.inch", w);
    }

    public double getHeightInch() {
        return getDoubleProperty("plot.height.inch", "4");
    }

    public void setHeightInch(double h) {
        setDoubleProperty("plot.height.inch", h);
    }

    public int getPointSize() {
        return getIntProperty("plot.pointsize", "12");
    }

    public void setPointSize(int p) {
        setIntProperty("plot.pointsize", p);
    }

    public String getUnits() {
        return getProperty("plot.units", "in");
    }

    public void setUnits(String u) {
        setProperty("plot.units", u);
    }

    public int getTNWidth() {
        return getIntProperty("plot.thumbnail.width", "150");
    }

    public void setTNWidth(int w) {
        setIntProperty("plot.thumbnail.width", w);
    }

}
