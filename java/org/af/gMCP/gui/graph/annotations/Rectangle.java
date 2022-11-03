package org.af.gMCP.gui.graph.annotations;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.json.stream.JsonGenerator;

public class Rectangle extends Annotation {
	
	int width;
	int height;
	Color backColor = Color.WHITE;
	
	public Rectangle(int x, int y, int width, int height) {
		this(x, y, width, height, null);
	}

	public Rectangle(int x, int y,int width, int height, AnnotationPanel nl) {
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
		g.setColor(backColor);
		g.fill3DRect((int)(x* nl.getZoom()), (int)(y* nl.getZoom()), (int)(width* nl.getZoom()), (int)(height* nl.getZoom()), true);
		g.setColor(color);		
		g.draw3DRect((int)(x* nl.getZoom()), (int)(y* nl.getZoom()), (int)(width* nl.getZoom()), (int)(height* nl.getZoom()), true);
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
	public boolean inYou(int x, int y) {		
		return (x / nl.getZoom() - this.x)>0
				&& (x / nl.getZoom() - this.x)<width
				&& (y / nl.getZoom() - this.y)>0
				&& (y / nl.getZoom() - this.y)<height;
	}
	
	
}
