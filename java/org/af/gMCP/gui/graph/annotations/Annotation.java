package org.af.gMCP.gui.graph.annotations;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;

import javax.json.Json;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonParser;

public abstract class Annotation {

	int x, y;
	Color color = Color.BLACK;
	protected AnnotationPanel nl;
	
	final static String CLASS = "Class";

	/**
	 * Returns a JSON String from which an equal Annotation object can be reconstructed by readJSON. 
	 * @return
	 */
	public String saveToJSON() {
		StringWriter sw = new StringWriter();
		JsonGenerator gen = Json.createGenerator(sw);
		gen.writeStartObject()
		.write(CLASS, this.getClass().getCanonicalName());//.getSimpleName());
		writeObject(gen);	
		gen.writeEnd();
		gen.close();
		return sw.toString();
	}
	
	public int[] offset(int x2, int y2) {
		return new int[] {(int) (x* nl.getZoom())-x2, (int) (y* nl.getZoom())-y2};
	}
	
	public abstract Annotation readJSON(String json);
	public abstract void writeObject(JsonGenerator gen);
	public abstract Dimension paintObject(Graphics graphics);
	public abstract String getLaTeX();
	public abstract boolean inYou(int x, int y);

	public static Annotation createAnnotation(String s) {
		JsonParser parser = Json.createParser(new StringReader(s));
		String key = "";
		Annotation a = null;
		try {
			while (parser.hasNext()) {
				JsonParser.Event event = parser.next();
				switch(event) {
				case START_ARRAY:
				case END_ARRAY:
				case START_OBJECT:
				case END_OBJECT:
				case VALUE_FALSE:
				case VALUE_NULL:
				case VALUE_TRUE:				
					break;
				case KEY_NAME:
					key = parser.getString();
					break;
				case VALUE_STRING:
				case VALUE_NUMBER:
					String v = parser.getString();
					if (key.equals(CLASS)) {
						System.out.println("Creating object of class "+ v);
						a = (Annotation)Class.forName(v).getConstructor().newInstance();
					}
					break;
				}
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return a;
	}
	
	public static void main(String[] args) {
		Legend l = new Legend(100, 100,
				Arrays.asList(new String[]{
						"Component Weights",
						"Component Graph 1: 0.5",
						"Component Graph 1: 0.3",
						"Component Graph 1: 0.2"
				}), Arrays.asList(new Color[]{
						Color.BLACK,
						Color.RED,
						Color.GREEN,
						Color.BLUE
				}), new TestPanel());
		String s = l.saveToJSON();
		System.out.println(s);
		//createAnnotation("[0,{\"1\":{\"2\":{\"3\":{\"4\":[5,{\"6\":7}]}}}}]");
		createAnnotation(s);
	}

	public void setX(int x) {
		//System.out.println("Moving from x="+this.x+" to "+x+".");
		this.x = x;		
	}
	
	public void setY(int y) {
		this.y = y;		
	}

	public int getX() {
		return x;
	}

	public int getY() {
		return y;
	}

	public void setColor(Color color) {
		this.color = color;		
	}
	
}
