 package org.af.gMCP.tests;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JFrame;
import javax.swing.JPanel;

import org.af.commons.images.GraphDrawHelper;

public class CircleTest3Points extends JFrame implements MouseListener {

	public CircleTest3Points() {
		getContentPane().setLayout(new GridBagLayout());

		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.insets = new Insets(5, 5, 5, 5); 
		c.weightx=1; c.weighty=1;
		
		PaintPanel3Points panel = new PaintPanel3Points(50,50,0,0,100,100);
		panel.addMouseListener(this);
		panel.addMouseListener(panel);
		getContentPane().add(panel, c);
		c.gridx = 1; c.gridy = 0;
		
		panel = new PaintPanel3Points(100,100,0,0,50,50);
		panel.addMouseListener(this);
		panel.addMouseListener(panel);
		getContentPane().add(panel, c);
		c.gridx = 0; c.gridy = 1;

		panel = new PaintPanel3Points(100,50,0,0,50,50);
		panel.addMouseListener(this);
		panel.addMouseListener(panel);
		getContentPane().add(panel, c);
		c.gridx = 1; c.gridy = 1;
		
		panel = new PaintPanel3Points(50,100,0,0,50,50);
		panel.addMouseListener(this);
		panel.addMouseListener(panel);
		getContentPane().add(panel, c);
		c.gridx = 0; c.gridy = 2;

	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		CircleTest3Points f = new CircleTest3Points();
		f.setSize(800, 600);
		f.setVisible(true);
	}

	public void mouseClicked(MouseEvent e) {
		System.out.println("Mouseclicked at ("+e.getX()+","+e.getY()+").");
	}

	public void mouseEntered(MouseEvent e) {	}

	public void mouseExited(MouseEvent e) {		}

	public void mousePressed(MouseEvent e) {	}

	public void mouseReleased(MouseEvent e) {	}


	
}

class PaintPanel3Points extends JPanel implements MouseListener {
	
	public PaintPanel3Points(double a1, double a2, double b1, double b2, double c1, double c2) {
		this.a1 = a1;
		this.a2 = a2;
		this.b1 = b1;
		this.b2 = b2;
		this.c1 = c1;
		this.c2 = c2;
		this.setSize(200, 200);
	}
	
	double a1 = 100;
	double a2 = 100;
	double b1 = 120;
	double b2 = 80;
	double c1 = 180;
	double c2 = 130;
	
	protected void paintComponent(Graphics g) {
		g.setColor(Color.WHITE);
		g.fillRect(0, 0, getWidth(), getHeight());
		g.setColor(Color.BLACK);
		g.drawOval((int)a1-1, (int)a2-1, 2, 2);
		g.drawString("A", (int)a1, (int)a2);
		g.drawOval((int)b1-1, (int)b2-1, 2, 2);
		g.drawString("B", (int)b1, (int)b2);
		g.drawOval((int)c1-1, (int)c2-1, 2, 2);
		g.drawString("C", (int)c1, (int)c2);
		GraphDrawHelper.drawEdge(g, a1, a2, b1, b2, c1, c2, 8, 45, true);
		/*double[] m = {0,0};
		try {
			m = GraphToolKit.getCenter(a1, a2, b1, b2, c1, c2);
			g.drawOval((int)m[0]-1, (int)m[1]-1, 2, 2);
			g.drawString("M", (int)m[0], (int)m[1]);
			double r = Math.sqrt((m[0]-a1)*(m[0]-a1)+(m[1]-a2)*(m[1]-a2));
			double d = Math.sqrt((c1-a1)*(c1-a1)+(c2-a2)*(c2-a2));
			if (r/d>10) throw new GraphException("too linear");
			//g.drawOval((int)(m[0]-r), (int)(m[1]-r), (int)(2*r), (int)(2*r));
			double[] phi = GraphToolKit.getAngle(a1, a2, b1, b2, c1, c2, m[0], m[1]);
			g.drawArc((int)(m[0]-r), (int)(m[1]-r), (int)(2*r), (int)(2*r), (int)(phi[0]), (int)(phi[1]));		
		} catch (GraphException e) {
			System.out.println(e.getMessage());
			g.drawLine((int)a1, (int)a2, (int)c1, (int)c2);
		}*/
	}
	
	protected void test1(Graphics g) {
		double a = 100;
		double b = 100;
		//a = 200;
		//b = 200;
		double c = 200;		
		double d = 200;
		//c = 100;
		//d = 100;
		double alpha = (60.0/360.0)*2*Math.PI;
		double s = Math.sqrt((c-a)*(c-a)+(d-b)*(d-b));
		double r = s/(2*Math.sin(alpha));
		System.out.println("Radius is "+r+".");
		double p1 = (a+c)/2;
		double p2 = (b+d)/2;
		double h = s/2*Math.cos(alpha)/Math.sin(alpha);
		double z1 = -1;
		double z2 = -(a-c)*z1/(b-d);
		System.out.println("Z is ("+z1+","+z2+").");
		double zb = Math.sqrt(z1*z1+z2*z2);
		z1 = z1/zb;
		z2 = z2/zb;
		double m1 = p1+h*z1;
		double m2 = p2+h*z2;
				
		g.drawOval((int)a-1, (int)b-1, 2, 2);
		g.drawString("1", (int)a, (int)b);
		g.drawOval((int)c-1, (int)d-1, 2, 2);
		g.drawString("2", (int)c, (int)d);
		g.drawOval((int)m1-1, (int)m2-1, 2, 2);
		g.drawString("M", (int)m1, (int)m2);
		
		double phi1 = Math.atan((-b+m2)/(a-m1))*360/(2*Math.PI)+((a-m1<0)?180:0);
		double phi2 = Math.atan((-d+m2)/(c-m1))*360/(2*Math.PI)+((c-m1<0)?180:0);
		System.out.println("phi is ("+phi1+","+phi2+").");
		
		g.drawArc((int)(m1-r), (int)(m2-r), (int)(2*r), (int)(2*r), (int)(phi1), (int)(phi2-phi1));		
	}

	public void mouseClicked(MouseEvent e) {
		b1 = e.getX();
		b2 = e.getY();
		repaint();
	}

	public void mouseEntered(MouseEvent e) {}

	public void mouseExited(MouseEvent e) {}

	public void mousePressed(MouseEvent e) {}

	public void mouseReleased(MouseEvent e) {}
}