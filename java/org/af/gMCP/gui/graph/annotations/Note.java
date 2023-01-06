package org.af.gMCP.gui.graph.annotations;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.json.stream.JsonGenerator;

public class Note extends Annotation {

	String text;
	int fontsize;
	Color color;
	int xP, yP; 
	int width, height;
	
	public Note(String text, int xP, int yP, int x, int y, int width, int height, Color color, int fontsize) {
		this.text = text;
		this.xP = xP;
		this.yP = yP;
		this.x = x;
		this.y = y;
		this.color = color;
		this.fontsize = fontsize;
	}

	@Override
	public void writeObject(JsonGenerator gen) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Dimension paintObject(Graphics graphics) {
		// TODO Auto-generated method stub
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
		// TODO Auto-generated method stub
		return false;
	}
	
}
