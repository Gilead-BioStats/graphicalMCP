 package org.af.gMCP.tests;

import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JFrame;
import javax.swing.JPanel;

public class CircleTest extends JFrame implements MouseListener {

	public CircleTest() {
		JPanel panel = new PaintPanel();
		panel.addMouseListener(this);
		getContentPane().add(panel);		
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		CircleTest f = new CircleTest();
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

	/*
	 * Given three points (a1, a2), (b1, b2), (c1, c2) this function returns
	 * the center of the well-defined circle that goes through all of the three points.  
	 */
	public static double[] getCenter(double a1, double a2, double b1, double b2, double c1, double c2) {
		//If all points are on one line - return an error:
		
		//
		double m1 = 0;
		double m2 = 0;
		return new double[] {m1, m2};
	}
	
}

class PaintPanel extends JPanel {
	protected void paintComponent(Graphics g) {
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
}