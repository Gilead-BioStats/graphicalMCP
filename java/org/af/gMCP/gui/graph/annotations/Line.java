package org.af.gMCP.gui.graph.annotations;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.json.stream.JsonGenerator;

import org.af.commons.images.GraphDrawHelper;

public class Line extends Annotation {

	final static int NONE = 0;
	final static int ARROW = 1;
	
	int x1, y1, x2, y2;
	Color color;
	int type;
	
	public Line(int x1, int y1, int x2, int y2, int type, Color color) {
		this.x1 = x1;
		this.y1 = y1;
		this.x2 = x2;
		this.y2 = y2;
		this.type = type;
		this.color = color;
	}

	@Override
	public void writeObject(JsonGenerator gen) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Dimension paintObject(Graphics graphics) {
		Graphics2D g = (Graphics2D) graphics;
		if (type == NONE) {
			g.drawLine((int)(x1*nl.getZoom()), (int)(y1*nl.getZoom()), (int)(x2*nl.getZoom()), (int)(y2*nl.getZoom()));
		} else {
			GraphDrawHelper.malVollenPfeil(g, (int)(x1*nl.getZoom()), (int)(y1*nl.getZoom()), (int)(x2*nl.getZoom()), (int)(y2*nl.getZoom()), 5, 45);		
		}
		return new Dimension(Math.abs(x1-x2), Math.abs(y1-y2));
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
