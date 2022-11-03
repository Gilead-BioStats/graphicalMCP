package org.af.gMCP.gui.graph;

/* A WrongInputException is thrown if one of the input fields
 * contains invalid data (for example an empty alpha field).
 * This Exception should be caught and the calculation aborted.
 * It is the responsibility of the function who throws such a
 * function to inform the user about the error. 
 */
public class WrongInputException extends Exception {
	WrongInputException(String error) {
		super(error);
	}
}
