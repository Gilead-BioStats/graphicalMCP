package org.af.gMCP.gui;

import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URLEncoder;
import java.util.Hashtable;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.errorhandling.HTTPPoster;
import org.af.commons.logging.ApplicationLog;
import org.af.commons.logging.LoggingSystem;
import org.af.commons.threading.SafeSwingWorker;
import org.af.commons.tools.StringTools;
import org.af.commons.widgets.GUIToolKit;
import org.af.commons.widgets.RightClickTextMenuListener;
import org.af.gMCP.config.Configuration;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.log4j.FileAppender;
import org.apache.log4j.Logger;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public abstract class ErrorDialogChooseLevelBase extends JDialog implements ActionListener {

	String[] reportLevels = new String[] {
			"Report no error.",
			"Minimal: Just the stack trace + version + OS type",
			"Default: The most important information",
			"Maximal: Include more information about the system"
	};
	
	protected JComboBox jcbReportLevel;
	protected JCheckBox jcbScreenshot = new JCheckBox("Send screenshot of GUI window");	
	
    protected static Log logger = LogFactory.getLog(ErrorDialogChooseLevelBase.class);
	
	public JButton send = new JButton("Send directly");
	public JButton emailClient = new JButton("Open email client");
	public JButton createZip = new JButton("Save report file");
	public JButton close = new JButton("Ignore and close");	
	
    protected final boolean fatal;
    // message in header
    protected String informMsg = "";
    // displayed error message
    protected final String msg;
    protected String message = "";
    protected String stacktrace = "";
    // throwable which caused the error, might be null
    protected final Object e;
    protected ApplicationLog al;
    String subjectShort, subjectLong;
	
	JTextArea jta;
	
	public ErrorDialogChooseLevelBase(String msg, Object e, boolean fatal) {	
		super(GUIToolKit.findActiveFrame(), "Error report", true);        
        this.fatal = fatal;
        this.e = e;
        if (e!=null && e instanceof Throwable) ((Throwable)e).printStackTrace();
        this.msg = msg;
    	if (e!=null) {
    		if (e instanceof Throwable) {
        		message = ((Throwable)e).getMessage();        	
        		stacktrace = ExceptionUtils.getStackTrace((Throwable)e);
        	} else {
        		message = e.toString();
        	}
    	}
    	if (message==null) message = "";
    	
    	al = LoggingSystem.getInstance().getApplicationLog();

		String cols = "5dlu, pref, 5dlu, fill:pref:grow, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";
		String rows = "5dlu, pref, 5dlu, pref, 5dlu, fill:200dlu:grow, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";

		FormLayout layout = new FormLayout(cols, rows);
		getContentPane().setLayout(layout);
		CellConstraints cc = new CellConstraints();
		
		jcbReportLevel = new JComboBox(reportLevels);

	    int rLevel;
	    try {
	    	rLevel = Integer.parseInt(Configuration.getInstance().getClassProperty(this.getClass(), "reportLevel", "2"));
	    } catch (Exception ex) {
	    	rLevel = 2;
	    }
	    jcbReportLevel.setSelectedIndex(rLevel);
		jcbReportLevel.addActionListener(this);
		
		subjectShort = getSubjectShort();
	    
		jta = new JTextArea(getErrorReport(jcbReportLevel.getSelectedIndex()));
		jta.setCaretPosition(0);
		jta.setFont(new Font("Monospaced", Font.PLAIN, 10));
		jta.setLineWrap(true);
		jta.setWrapStyleWord(true);
		jta.setMargin(new Insets(4,4,4,4));
		jta.addMouseListener(new RightClickTextMenuListener(jta));
		
    	subjectLong = subjectShort+" : "+    			
    			(message.length()<40?message:message.substring(0, 37)+"...");
	    
		int row = 2;
		
		JTextArea jlabel = new JTextArea("We are sorry that an error occurred.\n" +
				"We would be pleased, if you could send the report below to "+ErrorHandler.getInstance().getDeveloperAddress()+" :"); // bugreport@small-projects.de		
		jlabel.setOpaque(false);
		jlabel.setEditable(false);
		jlabel.setFont(jlabel.getFont().deriveFont(jlabel.getFont().getStyle() ^ Font.BOLD));
		
		getContentPane().add(jlabel, cc.xyw(2, row, 11));
		
		row += 2;		
		
		getContentPane().add(jcbReportLevel, cc.xyw(2, row, 11));
		
		row += 2;		

		JScrollPane jsp = new JScrollPane(jta);
		//jsp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		
		getContentPane().add(jsp, cc.xyw(2, row, 11));
		
		row += 2; 
		
		getContentPane().add(send, cc.xy(6, row));
		send.addActionListener(this);
		getContentPane().add(emailClient, cc.xy(8, row));
		emailClient.addActionListener(this);
		getContentPane().add(createZip, cc.xy(10, row));
		createZip.addActionListener(this);
		getContentPane().add(close, cc.xy(12, row));
		close.addActionListener(this);		
		
	}
	
	public abstract String getSubjectShort();
	
	public void showDialog() {
		pack();
		setSize(900, 700);
		
		setVisible(true);
	}
	
	void openEmailClient() {
		try {
			String mailto = "mailto:"+ErrorHandler.getInstance().getDeveloperAddress() //"bugreport@small-projects.de"
					        +"?subject="+URLEncoder.encode(subjectLong, "UTF-8").replace("+", "%20")
							+"&body="+URLEncoder.encode(jta.getText(), "UTF-8").replace("+", "%20");
			//String uriString = URLEncoder.encode(mailto, "UTF-8").replace("+", "%20");
			//System.out.println(uriString);
			
			/* This is a Wrapper for Desktop.getDesktop().mail(uriMailTo);
             * that will do that for Java >=6 and nothing for
             * Java 5.
             */    
			// Long for Desktop.getDesktop().mail(new URI(mailto));
    		try {	
    			URI uriMailTo = new URI(mailto);
    			Method main = Class.forName("java.awt.Desktop").getDeclaredMethod("getDesktop");
    			Object obj = main.invoke(new Object[0]);
    			Method second = obj.getClass().getDeclaredMethod("mail", new Class[] { URI.class }); 
    			second.invoke(obj, uriMailTo);
    		} catch (Exception e) {			
    			logger.warn("No Desktop class in Java 5 or URI error.",e);
    		}

			dispose();
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} 
	}

	protected String getSep(String title) {
		return "\n************************* "+title+" *************************\n\n";
	}
	
	protected abstract String getErrorReport(int level);

	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==close) {
			dispose();
			return;
		}
		if (e.getSource()==send) {
			SafeSwingWorker<Void, Void> worker = new SafeSwingWorker<Void, Void>() {
				@Override
				protected Void doInBackground() throws Exception {
					(new HTTPPoster()).post(ErrorHandler.getInstance().getReportURL(), getInfoTable(), getAttachedFiles());                
					return null;
				}

				@Override
				protected void onFailure(Throwable t) {
					String msg = "Could not connect to server and send report.\n("+t.getMessage()+")\nPlease send mail manually!";
					logger.error(msg, t);
					JOptionPane.showMessageDialog(ErrorDialogChooseLevelBase.this, msg, "Could not send report", JOptionPane.ERROR_MESSAGE);
					//lockableUI.setLocked(false);

					openEmailClient();		                
				}

				@Override
				protected void onSuccess(Void result) {                
					dispose();
					JOptionPane.showMessageDialog(ErrorDialogChooseLevelBase.this, "Report was sent.");
				}
			};
			worker.execute();
			return;
		}
		if (e.getSource()==emailClient) {
			openEmailClient();
			return;
		}
		if (e.getSource()==createZip) {
			PrintWriter out;
			File f;
			try {
				JFileChooser fc = new JFileChooser(Configuration.getInstance().getClassProperty(this.getClass(), "ReportSaveDirectory"));
				fc.setDialogType(JFileChooser.SAVE_DIALOG);		
				fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
				int returnVal = fc.showSaveDialog(this);
				if (returnVal == JFileChooser.APPROVE_OPTION) {			
					f = fc.getSelectedFile();
					Configuration.getInstance().setClassProperty(this.getClass(), "ReportSaveDirectory", f.getParent());
					if (!f.getName().toLowerCase().endsWith(".txt")) {
		            	f = new File(f.getAbsolutePath()+".txt");
					}					
				} else {
					return;
				}
				if (f.exists()) {
					int answer = JOptionPane.showConfirmDialog(this, "File '"+f.getAbsolutePath()+"' exists.\n Do you want to overwrite it?", "File exists", JOptionPane.WARNING_MESSAGE);
					if (answer!=JOptionPane.YES_OPTION) return;
				}
				out = new PrintWriter(f.getAbsolutePath());
				out.println(jta.getText());
				out.close();
				dispose();
				return;
			} catch (FileNotFoundException e1) {
				JOptionPane.showMessageDialog(this, "Error saving report:\n"+e1.getMessage(), "Error saving error report - oh my", JOptionPane.ERROR_MESSAGE);
				e1.printStackTrace();
			}
		}
		Configuration.getInstance().setClassProperty(this.getClass(), "reportLevel", ""+jcbReportLevel.getSelectedIndex());
		jta.setText(getErrorReport(jcbReportLevel.getSelectedIndex()));
		jta.setCaretPosition(0);
	}	
	
	protected Hashtable<String, String> getInfoTable() {
    	Hashtable<String, String> table = new Hashtable<String,String>();
    	//table.put("Contact", tfContact.getText());
    	
    	table.put("Bugreport", jta.getText());
    	
    	table.put("Subject", subjectLong);
    	return table;
    }
	
    protected Hashtable<String, File> getAttachedFiles() throws IOException {
    	return new Hashtable<String, File>();
    }    

    protected String getRErrorMessage() {
		return StringTools.collapseStringArray(RControl.getR().eval("paste(geterrmessage(), collapse=\"\\n\")").asRChar().getData());		
	}
	
	protected String getROptions() {		
		return StringTools.collapseStringArray(RControl.getR().eval("paste(capture.output(options()), collapse=\"\\n\")").asRChar().getData());
	}

	protected String getRSessionInfo() {
		return StringTools.collapseStringArray(RControl.getR().eval("paste(capture.output(sessionInfo()), collapse=\"\\n\")").asRChar().getData());
	}

	protected String getSystemInfo() {		
		return al.getSystemInfo();
	}
	
	protected String getTraceBack() {
		return StringTools.collapseStringArray(RControl.getR().eval("paste(capture.output(traceback()), collapse=\"\\n\")").asRChar().getData());
	}
	
    public File getReadableLogFile() {
        return new File(getReadableLogFileAppender().getFile());
    }
    
    public FileAppender getReadableLogFileAppender() {
        return (FileAppender)Logger.getRootLogger().getAppender("READABLE_FILE");
    }

    public File screen() throws IOException {
    	/*JFrame f = CreateGraphGUI.lastCreatedGUI;
    	BufferedImage image = new BufferedImage(f.getWidth(), f.getHeight(), BufferedImage.TYPE_INT_RGB);
    	Graphics2D graphics2D = image.createGraphics();
    	f.paint(graphics2D);
    	File tempDir = new File(System.getProperty("java.io.tmpdir"));
    	File file = new File(tempDir, "screen.jpg");
    	ImageIO.write(image, "jpeg", file);
    	return file;*/
    	return null;
    }
}
