package org.af.gMCP.gui.pdf;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.rmi.RemoteException;
import java.text.DateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Formatter;
import java.util.List;
import java.util.Locale;
import java.util.Vector;

import org.af.commons.io.pdf.PDFHelper;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.RControl;
import org.af.gMCP.gui.graph.GraphView;
import org.af.jhlir.call.RDataFrame;
import org.af.jhlir.call.RObj;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Cell;
import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.pdf.PdfWriter;

public class PDFGenerator {
	private static final Log logger = LogFactory.getLog(PDFGenerator.class);
    private List<Step> selectedSteps;

    public static Font font = new Font(Font.TIMES_ROMAN, 10, Font.NORMAL);
	public static Font fontb = new Font(Font.TIMES_ROMAN, 10, Font.BOLD);
	public static Font fonth = new Font(Font.TIMES_ROMAN, 12, Font.BOLD);

	GraphView control;
	
	/**
	 * Constructor which takes the AbstractControl-Object that is later used
	 * for accessing the Session and Configuration.
	 * @param control
	 */
	public PDFGenerator(GraphView control, List<Step> selectedSteps) {
		this.control = control;
        this.selectedSteps = selectedSteps;
    }
	
	//TODO Most likely we want the locale Locale - Locale.ENGLISH - what should one do?
	DateFormat df = DateFormat.getDateInstance(DateFormat.LONG, Locale.ENGLISH);;

	public void makePDF() throws FileNotFoundException, DocumentException {
		Calendar calendar = Calendar.getInstance();
		String filename = "Report - "+(df.format(calendar.getTime()))+".pdf";
		filename = filename.replace(':', '-');
		File outputFile = new File(Configuration.getInstance().getClassProperty(this.getClass(), "pdfDirectory", System.getProperty("user.home")), filename);
		makePDF(outputFile);
	}

	/**
	 * Creates a PDF file for this session and starts a PDF viewer to show this file.
	 * @throws DocumentException
	 * @throws FileNotFoundException
	 */
	public void makePDF(File outputFile) throws FileNotFoundException, DocumentException {
		Document document = new Document();
		logger.info("Creating pdf "+outputFile.getAbsolutePath()+".");
		PdfWriter writer = PdfWriter.getInstance(document, new FileOutputStream(outputFile));
		document.addTitle("Report");
		//document.addAuthor(control.getConf().getGeneralConfig().getApplicationTitle()+", Revision "+control.getConf().getGeneralConfig().getSVNVersion());
		document.addSubject("Statistical Report");
		document.addKeywords("Statistical Report");
		document.addCreator("iText used by the R Java Client - http://www.algorithm-forge.com/rjavaclient/");

/*
		control.createHeaderFooter();

		addHeaderFooter(document);
		if (control.footer==null) {
			writer.setPageEvent(new PageNumbers());
		}
*/
		document.open();
/*
		if (control.prefix!=null) {
			control.prefix.print(document);
		}

		for (Step step:selectedSteps) {
            if (step.result != null) {
            	RListRefW result = step.result;
                List<Plot> plots = step.plots;
                addResult(document, writer, result, plots);
            } else {
                document.add(new Paragraph(step.title+"\n\n"));
            }
		}

		if (control.afterword!=null) {
			control.afterword.print(document);
		}
*/
		document.close();

		/*GeneralConfig conf = control.getConf().getGeneralConfig();
		new PDFViewerStarter(conf.getPDFViewerPath(),
				conf.getPDFViewerOptions(), outputFile);*/

	}

	public void addHeaderFooter(Document document) {
		/*if (control.header!=null) {
			document.setHeader(control.header);
		}
		if (control.footer!=null) {
			document.setFooter(control.footer);
		}*/
	}

	
	/**
	 * Writes a Result-List and its list of files in a PDF document.
	 * @param document PDF document
	 * @param writer PdfWriter
	 * @param result Result-List
	 * @param plots List of plot files
	 * @throws DocumentException
	 */
	
	public void addResult(Document document, PdfWriter writer, Result result, List<Plot> plots) throws DocumentException {
		if (result != null) {
			try {		
				/*
				RObject res = result.at("result");				

				RListW resL = new RListW(control.getRControl().getRServices(), (RList) res);					
				boolean[] printedPlots = new boolean[plots.size()];

				logger.info("Appending headline and text.");
				RObject hlExp = result.at("headline");
				if (hlExp != null) {
					document.add(new Paragraph(new Chunk(RObjectHelper.asString(hlExp), fonth)));
				}
				for (int i=0; i< resL.length(); i++) {
					RObjectW ro = resL.atW(i);
					String resStr="";
					if (ro instanceof RCharW) {
						resStr = ((RCharW)ro).stringVal();
						if (resStr.startsWith("$PLOT_")) {
							int plotNr = Integer.parseInt(resStr.substring(6, resStr.length()));
							if (plotNr > printedPlots.length || plotNr < 1) {
								logger.error("Plot "+plotNr+" requested, but only "+printedPlots.length+" Plots available.");
							} else {
								document.add(getImage(writer,plots.get(plotNr-1).getPNGFile()));
								printedPlots[plotNr-1]=true;
								addCaption(document, plots.get(plotNr-1).caption);
							}
						} else {
							document.add(new Paragraph(resStr+"\n\n", font));	
						}		                	              	
					} else if (ro instanceof RDataFrameW) {
						document.add(new Paragraph());
						document.add(createTable(ro));
						document.add(new Paragraph());
						addCaption(document, getHeader(ro));
					}   
				}
				*/
				logger.info("Appending plots.");
				for (int i=0; i<plots.size(); i++) {
					//if (!printedPlots[i]) 
					{
						File file = plots.get(i).getPNGFile();
						document.add(getImage(writer,file));
						addCaption(document, plots.get(i).caption);
					}
				}

			} catch (IOException e) {
				logger.fatal("Error adding image to PDF.", e);
			}

		}
	}
	

	/*
	private String getHeader(RObjectW at) throws REvalException, RemoteException {
		RFunctionCall rac = new RFunctionCall(control.getRControl(), "attr");
		rac.addParam("x", at.getReference());
		rac.addParam("which", "caption");
		RObjectW rexpCaption = rac.getRef();
		return (rexpCaption==null || rexpCaption.getRObject()==null)?null:rexpCaption.asCharW().stringVal();		
	}
	*/

	public static void addCaption(Document document, String caption) throws DocumentException {
		if (caption!=null) {
			Paragraph p = new Paragraph(new Chunk(caption+"\n\n", fontb));
			p.setAlignment(Element.ALIGN_CENTER);
			document.add(p);
		}
	}

	private int getNr(String s, int n) {
		int nr = 0;
		while (n < s.length() && s.charAt(n)!='$') {
			int read = Integer.parseInt(""+s.charAt(n));
			nr = (nr*10)+read;
			n++;
		}
		return nr;
	}

	private Image getImage(PdfWriter writer, File file) throws IOException, BadElementException {
		Image image = PDFHelper.getImage(writer, file);
		image.scaleToFit(300,300);
		image.setAlignment(Image.MIDDLE);
		writer.setStrictImageSequence(true);
		return image;
	}

	/**
	 * Determines whether a given file is a PDF file by extension.
	 * Alternatively we could test whether the first 4 byte are "%PDF-".
	 * @param file
	 * @return Has the given file the extension ".pdf"?
	 */
	public boolean isPDF(File file) {
		String name = file.getName();
		logger.info("Is "+name+" a PDF file?");
		boolean result = false;
		String extension = name.substring(file.getName().length()-3);
		logger.info("Extension is: "+extension);
		if (extension.equalsIgnoreCase("pdf")) {
			result=true;
		}
		logger.info("Result: "+result);
		return result;
	}

	/**
	 * Creates a PdfPTable Object with Data from a xtable RExpression.
	 * @param xtable to export to PDF
	 * @return PdfPTable with data from xtable
	 * @throws BadElementException
	 * @throws RemoteException 
	 */
	public PdfPTable createTable(RObj xtable) throws BadElementException, RemoteException {
		RDataFrame df = RControl.getR().call("as.data.frame", xtable).asRDataFrame();

		PdfPTable table = new PdfPTable(df.getColumnCount()+1);

		Cell cell;

		logger.info("Writing headline of pdftable.");
		List<String> list = new Vector<String>();
		list.add(""); Collections.addAll(list, df.getColNames());
		for (String s : list) {
			cell = new Cell(new Chunk(s, fontb));
			cell.enableBorderSide(Rectangle.LEFT);
			cell.enableBorderSide(Rectangle.RIGHT);
			cell.enableBorderSide(Rectangle.BOTTOM);
			cell.setBorderWidth(0.5f);
			table.addCell(cell.createPdfPCell());
		}

		logger.info("The table has "+df.getRowCount()+" rows and "+df.getColumnCount()+" columns.");
		for (int i=0; i<df.getRowCount(); i++) {
			logger.info("Writing row "+(i+1)+" of "+df.getRowCount()+".");
			cell = new Cell(new Chunk(df.getRowName(i), font));
			PDFHelper.enable4Borders(cell);
			table.addCell(cell.createPdfPCell());
			for (int j=0; j<df.getColumnCount(); j++) {
				logger.info("Writing element ("+i+","+(j+1)+"):");
                String element = createString(df.getCol(i).get(i),false);
				cell = new Cell(new Chunk(element, font));
				logger.info(element);
				PDFHelper.enable4Borders(cell);
				table.addCell(cell.createPdfPCell());
			}
		}
		return table;
	}

	/**
	 * Return the formated String representation of the Object.
	 * If o is a numeric value, only control.getConf().getGeneralConfig().getIntDigits()
	 * positions after decimal point are printed.
	 * @param o Object to convert to String.
	 * @param appendZeros Should zeros appended.
	 * @return String representation of the Object.
	 */
	public String createString(Object o, boolean appendZeros) {
		if (o instanceof Float || o instanceof Double) {
			int digits = Configuration.getInstance().getGeneralConfig().getDigits(); //TODO control.getConf().getROutputConfig().getDigitsInTables();
			StringBuilder sb = new StringBuilder();
			Formatter formatter = new Formatter(sb);
			formatter.format("%."+digits+"f", o);
			return sb.toString();
		}
		return o.toString();
	}

}
