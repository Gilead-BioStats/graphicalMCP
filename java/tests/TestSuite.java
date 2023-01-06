package tests;

import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

public class TestSuite {
	
	public static String[] exampleGraphs = new String[] {
		"BonferroniHolm(5)",
		"parallelGatekeeping()",
		"improvedParallelGatekeeping()",
		"BretzEtAl2011()",
		"HungEtWang2010()",
		"HuqueAloshEtBhore2011()",
		"HommelEtAl2007()",
		"HommelEtAl2007Simple()",
		"MaurerEtAl1995()",
		"improvedFallbackI(weights=rep(1/3, 3))",
		"improvedFallbackII(weights=rep(1/3, 3))",
		"cycleGraph(nodes=paste(\\\"H\\\",1:4,sep=\\\"\\\"), weights=rep(1/4, 4))",
		"fixedSequence(5)",
		"fallback(weights=rep(1/4, 4))",
		"generalSuccessive(weights = c(1/2, 1/2))",
		"simpleSuccessiveI()",
		"simpleSuccessiveII()",
		"truncatedHolm()",
		"BauerEtAl2001()",
		"BretzEtAl2009a()",
		"BretzEtAl2009b()",
		"BretzEtAl2009c()"
	};
	
	public static void main(String[] args) {
		Result result = org.junit.runner.JUnitCore.runClasses(getClasses());
		System.out.println(getResultString(result));
	}
	
    public static Class[] getClasses() {
    	return new Class[] { 
    			LoadSaveTest.class,
    			/*DataTableTest.class,
    			EdgeTest.class,
    			InteractionTest.class,    			
    			NetListTest.class,
    			NodeTest.class,
    			PowerTest.class,
    			PPanelTest.class,
    			RControlTest.class,
    			TexFormulaTest.class,
    			ToolTest.class*/};
    }
    
    public static String getResultString(Result result) {
    	String s = "There were "+result.getFailureCount()+" failures ("+result.getRunCount()+" tests; runtime "+result.getRunTime()/1000+" sec):\n\n";
    	for (Failure f : result.getFailures()) {
    		s += "Failure in "+f.getDescription().getClassName()+"."+f.getDescription().getMethodName()+"()\n";
    		s += "Message: "+f.getMessage()+"\n\nTrace: "+f.getTrace()+"\n\n";
    	}
    	return s;
    }
}
