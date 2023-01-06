package org.af.gMCP.gui;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Hashtable;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.logging.ApplicationLog;
import org.af.commons.logging.LoggingSystem;
import org.af.gMCP.config.Configuration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.rosuda.JRI.Rengine;
import org.rosuda.REngine.REngineException;
import org.rosuda.REngine.JRI.JRIEngine;

public class RControl {

	private static final Log logger = LogFactory.getLog(RControl.class);

	protected static RControl rc = null;

	public static RControl getRControl(boolean debug) {
		if (rc==null) {
			rc = new RControl(debug); 
		}
		return rc;
	}

	public static RCallServicesREngineWrapper getR() {
		getRControl(true);
		return rcs;
	}

	protected static RCallServicesREngineWrapper rcs = null;
	public static DebugTextConsole console = null;

	protected RControl(boolean debug) {
		if (!LoggingSystem.alreadyInitiated()) {
			LoggingSystem.init(
					"/org/af/gMCP/gui/commons-logging.properties",
					System.getProperty("eclipse") == null && !debug,
					System.getProperty("eclipse") != null || debug,
					new ApplicationLog());
			ErrorHandler.init("rohmeyer@small-projects.de", "http://www.algorithm-forge.com/report/bugreport.php", true, true, ErrorDialogChooseLevel.class);

		}
		if (!Rengine.versionCheck()) {
            System.err.println("Error: API version of the Rengine class and the native binary differ.");
            System.exit(1);
        }
		Rengine rengine = Rengine.getMainEngine();
		if (rengine == null) {
			// Call java with VM arguments: -Declipse="true"
			if (System.getProperty("eclipse") != null) {
				console = new DebugTextConsole();
				rengine = new Rengine(new String[] {"--vanilla"}, true, console);
			} else {
				rengine = new Rengine();
			}
		}
		try {
			rcs = new RCallServicesREngineWrapper(new JRIEngine(rengine));
			if (System.getProperty("eclipse") != null) {
				rcs.eval("Sys.setlocale(\"LC_NUMERIC\", \"C\")");
				if (System.getProperty("libPath") != null) {
					rcs.eval(".libPaths(new=\""+System.getProperty("libPath")+"\")");
				}
				rcs.eval("require(gMCP)");	
				rcs.eval("assign(\"env\", globalenv(), envir = gMCP:::gMCPenv)");
				rcs.eval("graph <- BonferroniHolm(3)");
				rcs.eval("graph2 <- BretzEtAl2011()");
				rcs.eval("m <- matrix(0, nrow=2, ncol=2)");
				rcs.eval("dunnettM <- matrix(c(1,1/2,1/2,1), nrow=2)");
				rcs.eval("mu <- c(0.860382, 0.9161474, 0.9732953)");
				rcs.eval("sdEst <- c(0.8759528, 1.291310, 0.8570892)");
				rcs.eval("pval <- c(0.01260, 0.05154, 0.02124)/2");
				rcs.eval("pHommel <- c(0.097, 0.015, 0.005, 0.006, 0.004, 0.008, 0.04)");
				rcs.eval("nSize <- list(c(10,10,10), c(20,20,20), c(20,10,30))");
			}
		} catch (REngineException e) {
			ErrorHandler.getInstance().makeErrDialog("Error creating RCallServicesREngine!", e);
		}
		if (System.getProperty("eclipse") == null && !debug) System.setOut(new PrintStream(new LoggingOutputStream(logger), true));
	}
	
	/** This Hashtable saves for Strings d+":"+cycles (d double, cycles int) the result from getFraction(Double d, int cycles). */
	static Hashtable<String, String> fractions = new Hashtable<String, String>(); 
	
	/**
	 * Given a Double d this function returns a String with a fraction representation of d 
	 * if R's MASS::fractions can find such which differs less than GeneralConfig().getAccuracy() from d.
	 * Otherwise as.character(d) will be returned.
	 * @param d Double whose fraction representation is searched.
	 * @param cycles The maximum number of steps in MASS::fractions' approximation process.
	 * @return
	 */
	public static String getFraction(Double d, int cycles) {
		if (Double.isNaN(d)) {
			return("NaN");
		}
		if (Double.isInfinite(d)) {
			return("Infinity");
		}
		String result = fractions.get(""+d+":"+cycles);
		if (result != null) return result;
		result = RControl.getR().eval("as.character(MASS::fractions("+d+(cycles==-1?"":", cycles="+cycles)+"))").asRChar().getData()[0];
		boolean accurate = RControl.getR().eval("abs("+d+"-"+result+")<"+Configuration.getInstance().getGeneralConfig().getAccuracy()).asRLogical().getData()[0];
		if (!accurate) {
			//result = "~"+result; 
			result = RControl.getR().eval("as.character("+d+")").asRChar().getData()[0];
		}
		fractions.put(d+":"+cycles, result);
		return result;
	}
	
	public static String getFraction(Double d, boolean useUnicode) {
		return getFraction(d, useUnicode, -1);
	}

	public static String getFraction(Double d, boolean useUnicode, int cycles) {
		String f = getFraction(d, cycles);
		if (true) return f; //TODO boolean useUnicode is ignored - do we want to use this somewhere?
		if (!useUnicode) { return f; }
		if (f.equals("1/2")) return("½");
		if (f.equals("1/3")) return("⅓");
		if (f.equals("2/3")) return("⅔");
		if (f.equals("1/4")) return("¼");
		if (f.equals("3/4")) return("¾");
		/* The following does often not work:
		if (f.equals("1/5")) return("⅕");
		if (f.equals("2/5")) return("⅖");
		if (f.equals("3/5")) return("⅗");
		if (f.equals("4/5")) return("⅘");
		if (f.equals("1/6")) return("⅙");
		if (f.equals("5/6")) return("⅚");
		if (f.equals("1/8")) return("⅛");
		if (f.equals("3/8")) return("⅜");
		if (f.equals("5/8")) return("⅝");
		if (f.equals("7/8")) return("⅞");*/
		return f;
	} 

	public static String getFraction(Double d) {
		return getFraction(d, -1);
	}

	public static boolean exists(String obj) {		
		return getR().eval("exists(\""+obj+"\")").asRLogical().getData()[0];
	}

	/**
	 * Creates an ASCII R text representation of a double array.
	 * This includes values -Inf, Inf and NaN.
	 * @param x double array of interest
	 * @return ASCII text R representation of R numeric vector
	 */
	public static String getRString(double[] x) {
		String s = "c(";
		for (int i=0; i<x.length; i++) {
			double v = x[i];
			if (v == Double.NEGATIVE_INFINITY) {
				s += "-Inf,";
			} else if (v == Double.POSITIVE_INFINITY) {
				s += "Inf,";
			} else if (v == Double.NaN){
				s += "NaN,";
			} else {
				s += v+",";
			}			
		}
		return s.substring(0, s.length()-1)+")";
	}

	public static void evalAndLog(String command) {
		ReproducableLog.logR(command);
		getR().eval(command);		
	}

	public static String setSeed() {
		String code = "";
		if (Configuration.getInstance().getGeneralConfig().useSeed()) {
			code = "set.seed("+Configuration.getInstance().getGeneralConfig().getSeed()+")";
			getR().eval(code);
		}		
		return code+"\n";
	}

}

class LoggingOutputStream extends ByteArrayOutputStream { 

	private String lineSeparator;    
	Log logger;

	public LoggingOutputStream(Log logger) { 
		super(); 
		this.logger = logger; 
		lineSeparator = System.getProperty("line.separator"); 
	} 

	public void flush() throws IOException { 
		String record; 
		synchronized(this) { 
			super.flush(); 
			record = this.toString(); 
			super.reset(); 
			if (record.length() == 0 || record.equals(lineSeparator)) return; 
			logger.info(record); 
		} 
	} 
} 
