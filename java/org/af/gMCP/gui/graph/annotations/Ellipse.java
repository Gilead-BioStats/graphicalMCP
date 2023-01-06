package org.af.gMCP.gui.graph.annotations;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.json.stream.JsonGenerator;

public class Ellipse extends Annotation {

	int width;
	int height;
	Color color = Color.BLUE;
	
	public Ellipse(int x, int y, int width, int height, AnnotationPanel nl) {
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
		this.nl = nl;
	}

	@Override
	public void writeObject(JsonGenerator gen) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Dimension paintObject(Graphics graphics) {
		Graphics2D g = (Graphics2D) graphics;
		g.setColor(color);
		g.drawOval((int)(x*nl.getZoom()), (int)(y*nl.getZoom()), (int)(width*nl.getZoom()), (int)(height*nl.getZoom()));		
		return new Dimension(width, height);
	}

	@Override
	public String getLaTeX() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Annotation readJSON(String json) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean inYou(int xP, int yP) {
		double m1 = x + width/2.0;
		double m2 = y + height/2.0;
		boolean value = false;
		if (width==height) {
			value = (Math.sqrt((xP-m1)*(xP-m1)+(yP-m2)*(yP-m2))<width/2);
		} else if (width>height) {
			double f11 = x+width/2-Math.sqrt(width*width/4-height*height/4);
			double f21 = x+width/2+Math.sqrt(width*width/4-height*height/4);
			double f12 = y+height/2;
			double f22 = y+height/2;
			value = Math.sqrt((xP-f11)*(xP-f11)+(yP-f12)*(yP-f12))+Math.sqrt((xP-f21)*(xP-f21)+(yP-f22)*(yP-f22))<width;
		} else {
			double f11 = x+width/2;
			double f21 = x+width/2;
			double f12 = y+height/2-Math.sqrt(height*height/4-width*width/4);
			double f22 = y+height/2+Math.sqrt(height*height/4-width*width/4);
			value = Math.sqrt((xP-f11)*(xP-f11)+(yP-f12)*(yP-f12))+Math.sqrt((xP-f21)*(xP-f21)+(yP-f22)*(yP-f22))<height;
		}
		if (value) {
			color = Color.GREEN;
		} else {
			color = Color.RED;
		}
		return value;
	}
}
