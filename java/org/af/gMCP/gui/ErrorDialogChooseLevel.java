package org.af.gMCP.gui;

import java.util.List;
import java.util.Vector;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.logging.ApplicationLog;
import org.af.commons.logging.LoggingSystem;
import org.af.commons.tools.StringTools;
import org.af.gMCP.config.Configuration;

public class ErrorDialogChooseLevel extends ErrorDialogChooseLevelBase {

	public ErrorDialogChooseLevel(String msg, Object e, boolean fatal) {	
		super(msg, e, fatal);
	}
	
	public String getSubjectShort() {
		return "gMCP "+Configuration.getInstance().getGeneralConfig().getVersionNumber()+
		" (R "+Configuration.getInstance().getGeneralConfig().getRVersionNumber()+") " +
		"bug report from "+System.getProperty("user.name", "<unknown user name>")+
		" on "+System.getProperty("os.name", "<unknown OS>");
	}
	

	protected String getErrorReport(int level) {	
		
		if (level==0) return "Please reconsider reporting this error.";
		String text = subjectShort +" : "+  message +"\n"+ getSep("R Error Message") +getRErrorMessage() +getSep("R TraceBack")+getTraceBack()+"\n" + getSep("Java Stacktrace") +stacktrace;  			
    			//(message.length()<40?message:message.substring(0, 37)+"...");
		if (level==1) return text;
		
		List<String> rhistory = new Vector<String>();
		// We copy the history, to avoid a ConcurrentModificationException when there are still R commands executed.
		for (int i=0; i < RControl.getR().getHistory().size(); i++) {
			rhistory.add(RControl.getR().getHistory().get(i));
		}
		
		text += getSep("R Session Info")+getRSessionInfo() +getSep("Graph Info")+getGraph() +getSep("R GUI History")+StringTools.collapseStringList(rhistory,"\n");
		if (level==2) return text;
		
		text += getSep("System Info")+getSystemInfo()+getSep("R Options")+getROptions();
		return text;
	}
	
	private String getGraph() {
    	return StringTools.collapseStringArray(RControl.getR().eval("gMCP:::getDebugInfo()").asRChar().getData());
	}

	public static void main(String[] args) {
		LoggingSystem.init("/org/af/gMCP/gui/commons-logging.properties", false, true,	new ApplicationLog());
		ErrorHandler.init("rohmeyer@small-projects.de", "http://www.algorithm-forge.com/report/bugreport.php", true, true, ErrorDialogChooseLevel.class);
		RControl.getRControl(true).getR().eval("plot(z=1:100)");		
		throw new RuntimeException("This is a test.");
		//ErrorHandler.getInstance().makeErrDialog("Report Error");
	}
}
