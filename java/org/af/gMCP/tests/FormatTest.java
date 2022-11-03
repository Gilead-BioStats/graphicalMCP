package org.af.gMCP.tests;

import java.io.File;
import java.text.DecimalFormat;

public class FormatTest {
	
	public static void main(String[] args) {
		System.out.println(Boolean.parseBoolean(""));
		System.out.println(new File("test.dat").getAbsolutePath().replaceAll("\\\\", "\\\\\\\\"));
		System.out.println("\\alpha");
		System.out.println("\\alpha".replaceAll("\\\\", "\\\\\\\\"));
		System.out.println("2^(1+2)".replaceAll("\\(", "{(").replaceAll("\\)", ")}"));
		DecimalFormat format = new DecimalFormat("#.####");
		System.out.println(format.format(0.0001));
		System.out.println(format.format(0.9999));		
		System.out.println(format.format(0.00001));
		System.out.println(format.format(0.99999));
		System.out.println(format.format(1.0E-4));
		format = new DecimalFormat("#.#####");
		System.out.println();
		System.out.println(format.format(0.0001));
		System.out.println(format.format(0.9999));		
		System.out.println(format.format(0.00001));
		System.out.println(format.format(0.99999));
		System.out.println(format.format(1.0E-4));
		format = new DecimalFormat("#.###");
		System.out.println();
		System.out.println(format.format(0.0001));
		System.out.println(format.format(0.9999));		
		System.out.println(format.format(0.00001));
		System.out.println(format.format(0.99999));
		System.out.println(format.format(1.0E-4));
	}
}
