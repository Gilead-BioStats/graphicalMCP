package org.af.gMCP.poi;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import javax.imageio.ImageIO;

import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.graph.GraphView;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.util.Units;
import org.apache.poi.xwpf.usermodel.Borders;
import org.apache.poi.xwpf.usermodel.ParagraphAlignment;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.poi.xwpf.usermodel.XWPFParagraph;
import org.apache.poi.xwpf.usermodel.XWPFRun;
import org.apache.poi.xwpf.usermodel.XWPFTable;
import org.apache.poi.xwpf.usermodel.XWPFTableRow;

public class GraphDocXWriter {

	GraphView control;
	
	public GraphDocXWriter(GraphView control) {
		this.control = control;
	}	
	
	public static void addImage(XWPFParagraph p, BufferedImage image) throws IOException, InvalidFormatException {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		ImageIO.write(image, "png", os);
		InputStream picIS = new ByteArrayInputStream(os.toByteArray());
		
		XWPFRun run = p.createRun();
		run.addPicture(picIS, XWPFDocument.PICTURE_TYPE_PNG, "graph.png", Units.toEMU(image.getWidth()/zoom), Units.toEMU(image.getHeight()/zoom));
	}
	
	final static double zoom = 4; 
	
	/* Stuff one could use:
	 * run.setSubscript(VerticalAlign.SUBSCRIPT);
	 * run.setTextPosition(120);
	 * p.setAlignment(ParagraphAlignment.DISTRIBUTE);
	 * p.setIndentationRight(150);
	 */
	
	public void createDocXReport(File file) throws IOException, InvalidFormatException {
		XWPFDocument doc = new XWPFDocument();
		XWPFRun run;
		XWPFParagraph p;
		
		createHeader(doc);
		
		/* Insert graph */
		
		p = doc.createParagraph();
		addImage(p, control.getNL().getImage(zoom, Configuration.getInstance().getGeneralConfig().getColoredImages()));
		
		doc.createParagraph().createRun().addBreak();
		
		p = doc.createParagraph();		

		String descr = control.getDView().getDescription();
		//System.out.println(descr);

		for (String s : descr.split("\\\\n")) {
			run = p.createRun();
			run.setText(s);
			run.addBreak();
		}
		
		doc.createParagraph().createRun().addBreak();
		
		p = doc.createParagraph();
		run = p.createRun();
		run.setBold(true);
		run.setText("R Code:");
		run.addBreak();
		run = p.createRun();
		run.setFontSize(10);
		run.setFontFamily("Courier");
		run.setText(control.rCode);
		
		doc.createParagraph().createRun().addBreak();
		
		List<Double> pv = control.getPView().getPValues();
		List<String> pvn = control.getNL().nlp.get(0).getHNames();
		boolean[] rejected = null;
		double[] adjPValues = null;
		try {
			rejected = RControl.getR().eval(control.result+"@rejected").asRLogical().getData();
			adjPValues = RControl.getR().eval(control.result+"@adjPValues").asRNumeric().getData();
		} catch (Exception e) {
			//TODO - error handling + throwing in all strange cases.
			e.printStackTrace();
		}		
		createPValueTable(doc, pv, pvn, adjPValues, rejected);
		
		
		FileOutputStream fos = new FileOutputStream(file);
		doc.write(fos);
		fos.close();
	}
	
	private void createHeader(XWPFDocument doc) {
		XWPFParagraph p = doc.createParagraph();
		p.setAlignment(ParagraphAlignment.LEFT);
		setAllBorders(p, Borders.SINGLE);
		
		XWPFRun run = p.createRun();
		run.setFontSize(24);
		run.setBold(true);
		run.setText("gMCP Report");
		
		//TODO Let user set user name and date format.
		String user = Configuration.getInstance().getGeneralConfig().getUser();
		String rVersion = Configuration.getInstance().getGeneralConfig().getRVersionNumber();
		String gMCPVersion = Configuration.getInstance().getGeneralConfig().getVersionNumber();
		String date = new SimpleDateFormat("yyyy-MM-dd HH:mm").format(new Date());
		doc.createParagraph().createRun().setText("Date: "+date+ ", User: "+user+", R "+rVersion+", gMCP "+gMCPVersion);

		doc.createParagraph().createRun().addBreak();
	}
	
	/**
	 * Creates a p-value table
	 * @param doc XWPFDocument document for table
	 * @param pv List of p-values
	 * @param pvn List of hypotheses names corresponding to p-values
	 * @param rejected 
	 * @param adjPValues 
	 */
	private void createPValueTable(XWPFDocument doc, List<Double> pv, List<String> pvn, double[] adjPValues, boolean[] rejected) {
		
		XWPFTable table = doc.createTable();
		XWPFTableRow row = table.getRow(0);
		row.addNewTableCell();
		XWPFParagraph p = row.getCell(0).getParagraphs().get(0);
		XWPFRun run = p.createRun();
		run.setText("Hypothesis");
		run.setBold(true);
		
		p = row.getCell(1).getParagraphs().get(0);
		run = p.createRun();
		run.setText("P-Value");
		run.setBold(true);
		
		if (rejected!=null) {
			row.addNewTableCell();
			p = row.getCell(2).getParagraphs().get(0);
			run = p.createRun();
			run.setText("Rejected");
			run.setBold(true);
			
			if (adjPValues!=null) {
				row.addNewTableCell();
				p = row.getCell(3).getParagraphs().get(0);
				run = p.createRun();
				run.setText("Adj. P-Value");
				run.setBold(true);
				
			}
		}		
		
		row = table.createRow();		
		
		for (int i=0; i < pv.size(); i++) {
			//TODO Hypotheses names could be parsed and e.g. "_" replaced by run.setSubscript(VerticalAlign.SUBSCRIPT);
			row.getCell(0).setText(pvn.get(i));
			row.getCell(1).setText(""+pv.get(i));
			if (rejected!=null) {
				row.getCell(2).setText(""+rejected[i]);
				if (adjPValues!=null) {
					row.getCell(3).setText(""+adjPValues[i]);
				}
			}
			if (i !=pv.size()-1) {
				row = table.createRow();
			}
		}
		
	}

	private static void setAllBorders(XWPFParagraph p, Borders type) {
		p.setBorderBottom(type);
		p.setBorderTop(type);
		p.setBorderRight(type);
		p.setBorderLeft(type);
		p.setBorderBetween(type);		
	}
}
