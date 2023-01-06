package org.af.gMCP.gui.dialogs;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;

import javax.swing.JPanel;

/**
 * A JPanel that draws an Image object. 
 */
public class ImagePanel extends JPanel {
	Image image;
	
	int x = 0;
	int y = 0;
	int x2 = 0;
	int y2 = 0;
	
	public ImagePanel(Image image, int x, int y, int x2, int y2) {
		this.x = x;
		this.y = y;
		this.x2 = x2;
		this.y2 = y2;
		setImage(image);
	}
	
	public ImagePanel(Image image) {
		setImage(image);
	}
	
	public void setImage(Image image) {
		this.image = image;
		this.setPreferredSize(new Dimension(image.getWidth(null)+x+x2, image.getHeight(null)+y+y2));		
		this.repaint();
	}
	
	public void paintComponent(Graphics g) {
		super.paintComponents(g);
		g.drawImage(image, x, y, null);
	}
}
