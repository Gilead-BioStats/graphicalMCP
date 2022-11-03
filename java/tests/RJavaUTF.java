package tests;

import org.af.gMCP.gui.RControl;

public class RJavaUTF {

	/**
	 * The following program shows a problem when I forget to adequately escape backslashes...
	 * @param args
	 */
	public static void main(String[] args) {
		String s = RControl.getR().eval("\"\\epsilon\"").asRChar().getData()[0];
		System.out.println(s);
	}	
	
	/* Causes:
	 
*** longjmp causes uninitialized stack frame ***: /usr/lib/jvm/java-7-openjdk-amd64/bin/java terminated
======= Backtrace: =========
/lib/x86_64-linux-gnu/libc.so.6(__fortify_fail+0x5c)[0x7fe4bf18608c]
/lib/x86_64-linux-gnu/libc.so.6(+0x111f9d)[0x7fe4bf185f9d]
/lib/x86_64-linux-gnu/libc.so.6(__longjmp_chk+0x33)[0x7fe4bf185f03]
/usr/lib/libR.so(+0xb603d)[0x7fe457bb203d]
/usr/lib/libR.so(+0xb5048)[0x7fe457bb1048]
/usr/lib/libR.so(Rf_errorcall+0x3a0)[0x7fe457bb1820]
/usr/lib/libR.so(+0xd46f9)[0x7fe457bd06f9]
/usr/lib/libR.so(+0xd6cfa)[0x7fe457bd2cfa]
/usr/lib/libR.so(Rf_yyparse+0xabc)[0x7fe457bd3d6c]
/usr/lib/libR.so(+0xd9a09)[0x7fe457bd5a09]
/usr/lib/libR.so(+0xda048)[0x7fe457bd6048]
/usr/lib/libR.so(R_ParseVector+0x56)[0x7fe457bd62a6]
/usr/local/lib/R/site-library/rJava/jri/libjri.so(Java_org_rosuda_JRI_Rengine_rniParse+0x34)[0x7fe46809c354]
[0x7fe4b50136d2]
	 
	 */

}
