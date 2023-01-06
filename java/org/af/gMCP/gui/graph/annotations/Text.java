package org.af.gMCP.gui.graph.annotations;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.json.stream.JsonGenerator;

public class Text extends Annotation {

	String text;
	Font basefont;
	float basefontSize = 14;
	Font lastFont;
	float lastFontSize = 0;
	
	public Text(int x, int y, String text, Color color, Font basefont, AnnotationPanel nl) {
		this.x = x;
		this.y = y;
		this.text = text;
		this.color = color;
		this.basefont = basefont;
		basefontSize = basefont.getSize();
		lastFont = basefont;
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
		if (lastFontSize != (float) (basefontSize * nl.getZoom())) {
			lastFontSize = (float) (basefontSize * nl.getZoom());			
			lastFont = basefont.deriveFont(lastFontSize);			
		}
		g.setFont(lastFont);
		g.drawString(text, (int)(x* nl.getZoom()), (int)(y* nl.getZoom()));
		FontMetrics fm = g.getFontMetrics();
		return new Dimension(fm.stringWidth(text), fm.getHeight());
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

	public void setText(String text) {
		this.text = text;		
	}
}
