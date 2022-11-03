package org.af.gMCP.gui.graph;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class PrintableObject implements Printable {
	
	private NetList nl;
	protected static Log logger = LogFactory.getLog(PrintableObject.class);

	public PrintableObject(NetList nl) {
		this.nl = nl;
	}

	public int print(Graphics g, PageFormat pf, int pageIndex) throws PrinterException {		
		if (0 != pageIndex) {
			return NO_SUCH_PAGE;
		}
		try {
			double width = pf.getImageableWidth();
			double height = pf.getImageableHeight();
			Graphics2D g2 = (Graphics2D) g;
			Dimension dim = nl.getSize();

			g2.translate(pf.getImageableX(), pf.getImageableY());
			logger.info("Size (" + width + " - " + dim.getWidth() 
					+ ", " + height	+ " " + dim.getHeight() + ")");
			double scale = Math.min(width / dim.getWidth(), height / dim.getHeight());
			g2.scale(scale, scale);
			nl.setBackground(Color.white);
			nl.paintGraph(g);
		} catch (Exception ex) {
			throw new PrinterException(ex.getMessage());
		}
		return PAGE_EXISTS;
	}

}
