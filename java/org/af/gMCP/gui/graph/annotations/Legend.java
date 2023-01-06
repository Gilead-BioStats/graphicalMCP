package org.af.gMCP.gui.graph.annotations;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import javax.json.stream.JsonGenerator;
import javax.swing.JFrame;
import javax.swing.JPanel;

public class Legend extends Annotation {

	List<String> lines = new Vector<String>();
	List<Color> colors = new Vector<Color>();
	boolean header = true;
	Font f;
	List<Text> av = new Vector<Text>();
	Rectangle box = null;
	
	
	public Legend(int x, int y, List<String> lines, List<Color> colors, AnnotationPanel nl) {		
		this(x, y, lines, colors, nl, new Font("Arial", Font.PLAIN, 14));
		//TODO Can we use UIManager.getDefaults().getFont("JLabel.font") as default? Or do we want "Helvetica"?
	}
	
	public Legend(int x, int y, List<String> lines, List<Color> colors, AnnotationPanel nl, Font font) {
		this.x = x;
		this.y = y;
		this.lines = new Vector(lines); // Otherwise rm could throw an UnsupportedOperationException.
		this.colors = new Vector(colors);
		this.nl = nl;
		if (lines.size()!=colors.size()) throw new RuntimeException("Number of lines ("+lines.size()+") and colors "+colors.size()+" does not match.");
		
		for (int i=0; i<lines.size(); i++) {
			if (i==0 && header) {
				f = font.deriveFont(Font.BOLD);
			} else {
				f = font;
			}
			av.add(new Text(x+10, y+(i+1)*20, lines.get(i), colors.get(i), f, nl));
						 
		}
	}
	
	public void add(int index, String line, Color color) {
		lines.add(index, line);
		colors.add(index, color);
		av.add(index, new Text(x+10, y+(index+1)*20, line, color, f, nl));
	}
	
	/**
	 * Removes the element at the specified index and shifts any subsequent elements. 
	 * @param index The index of the element to be removed
	 * @param shiftColors If false all text keeps its color. Otherwise all text after removed item shifts its color one down (and the last color is lost).  
	 */
	public void rm(int index, boolean shiftColors) {
		System.out.println("Removing index "+index);
		lines.remove(index);
		if (!shiftColors) {
			colors.remove(index);
		} else {
			colors.remove(colors.size()-1);
		}
		av.remove(index);
		for (int i=index; i<av.size(); i++) {
			Annotation a = av.get(i);
			a.setY(a.getY()-20);
		}
		if (box!=null) {
			box.height -= 20;
		}		
		for (int i=0; i < lines.size(); i++) {
			av.get(i).setColor(colors.get(i));
		}
	}

	public Dimension paintObject(Graphics graphics) {
		Graphics2D g = (Graphics2D) graphics;
		
		int width = 0;
		Dimension d = null;
		
		if (box!=null) {
			box.paintObject(g);
		}
		
		for (Annotation a : av) {
			d = a.paintObject(g);
			width = Math.max(d.width, width); 
		}
		
		if (box == null) {
			box = new Rectangle(x, y, width+20, lines.size()*20+10, nl);			
			d = box.paintObject(g);
			for (Annotation a : av) {
				a.paintObject(g);
			}
		}
		
		return d;

	}
	
	public static void main(String[] args) {
		JFrame f = new JFrame();
		f.setVisible(true);
		f.setSize(800, 600);
		TestPanel tp = new TestPanel();
		f.setContentPane(tp);		
		System.out.println(tp.l.saveToJSON());
	}

	@Override
	public void writeObject(JsonGenerator gen) {
		gen.write("lastName", "Java");
		/*
		.write("postalCode", "12345")
		.writeStartArray("phoneNumbers")
		.writeStartObject()
		.write("type", "mobile")
		.write("number", "111-111-1111")
		.writeEnd()
		.writeStartObject()
		.write("type", "home")
		.write("number", "222-222-2222")
		.writeEnd()
		.writeEnd()
		.writeEnd();*/		
		
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
		if (box==null) return false;
		return box.inYou(x, y);
	}
	
	@Override
	public void setX(int x) {
		for (Annotation a : av) {
			a.setX(a.x+(x-this.x));
		}
		if (box!=null) {
			box.setX(box.x+(x-this.x));
		}
		super.setX(x);				
	}
	
	@Override
	public void setY(int y) {
		for (Annotation a : av) {
			a.setY(a.y+(y-this.y));
		}
		if (box!=null) {
			box.setY(box.y+(y-this.y));
		}
		super.setY(y);		
	}

	public void setText(int i, String text) {
		av.get(i).setText(text);		
	}
	
	
}

class TestPanel extends JPanel implements AnnotationPanel {
	
	Legend l;

	TestPanel() {
		l = new Legend(100, 100,
				Arrays.asList(new String[]{
						"Component Weights",
						"Component Graph 1: 0.5",
						"Component Graph 2: 0.3",
						"Component Graph 3: 0.2"
				}), Arrays.asList(new Color[]{
						Color.BLACK,
						Color.RED,
						Color.GREEN,
						Color.BLUE
				}), this);
	}
	
	public void paintComponent(Graphics g) {
		((Graphics2D)g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);
		l.paintObject(g);
	}

	public double getZoom() {		
		return 1;
	}
	
}