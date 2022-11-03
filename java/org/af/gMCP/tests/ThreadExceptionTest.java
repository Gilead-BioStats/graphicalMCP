package org.af.gMCP.tests;

import javax.swing.SwingUtilities;

import org.af.commons.errorhandling.DefaultExceptionHandler;

public class ThreadExceptionTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		//TODO Why is there no DefaultExceptionHandler on this thread?	
		if (SwingUtilities.isEventDispatchThread()) {
			System.out.println("This is the EDT!");
		} else {
			System.out.println("This is not the EDT!");
		}
		
		System.out.println(System.getProperty("sun.awt.exception.handler", "NOT_SET"));
		System.out.println(Thread.currentThread().getUncaughtExceptionHandler().getClass().toString());
		
		Thread.currentThread().setUncaughtExceptionHandler(new DefaultExceptionHandler());
		
		System.out.println(System.getProperty("sun.awt.exception.handler", "NOT_SET"));
		System.out.println(Thread.currentThread().getUncaughtExceptionHandler().getClass().toString());
		
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {		
				Thread.currentThread().setUncaughtExceptionHandler(new DefaultExceptionHandler());
			}
		});

		System.out.println(System.getProperty("sun.awt.exception.handler", "NOT_SET"));
		System.out.println(Thread.currentThread().getUncaughtExceptionHandler().getClass().toString());
		
	}

}
