package org.af.gMCP.gui;

import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Calendar;
import java.util.Hashtable;
import java.util.List;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.errorhandling.HTTPPoster;
import org.af.commons.io.FileTools;
import org.af.commons.io.Zipper;
import org.af.commons.logging.ApplicationLog;
import org.af.commons.logging.LoggingSystem;
import org.af.commons.threading.SafeSwingWorker;
import org.af.commons.tools.OSTools;
import org.af.commons.tools.StringTools;
import org.af.commons.widgets.GUIToolKit;
import org.af.commons.widgets.MultiLineLabel;
import org.af.commons.widgets.WidgetFactory;
import org.af.commons.widgets.buttons.HorizontalButtonPane;
import org.af.commons.widgets.buttons.OkCancelButtonPane;
import org.af.gMCP.config.Configuration;
import org.af.gMCP.gui.dialogs.TextFileViewer;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.log4j.FileAppender;
import org.apache.log4j.Logger;
import org.jdesktop.jxlayer.plaf.ext.LockableUI;

import com.jgoodies.forms.layout.CellConstraints;
import com.jgoodies.forms.layout.FormLayout;

public class ErrorDialogGMCP extends JDialog implements ActionListener {

    static int count = 0;
    
    static File tempDir = new File(System.getProperty("java.io.tmpdir"), "gMCP"+Calendar.getInstance().getTime());

    protected static Log logger = LogFactory.getLog(ErrorDialogGMCP.class);

    public static File makeLogFile(String fileName, String content) throws IOException{
        tempDir.mkdirs();
        File output = new File(tempDir, fileName);
        FileWriter fw = new FileWriter(output);
        fw.write(content);
        fw.close();
        return output;
    }
    
    protected ApplicationLog al;

    // throwable which caused the error, might be null
    protected final Object e;
    
    // is this a fatal error?
    protected final boolean fatal;
    // message in header
    protected String informMsg = "";
    // to disable the whole dialog
    protected LockableUI lockableUI;
    // displayed error message
    protected final String msg;
    protected String message = "";
    protected String stacktrace = "";
    // description
    protected JTextArea taDesc;
    
    // header, for error message
    protected MultiLineLabel taHeader;
    
    // other contact details of user
    protected JTextField tfContact;

    public ErrorDialogGMCP(String msg, Object e, boolean fatal) {				
        super(GUIToolKit.findActiveFrame(), true);        
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
	}


    /**
     * Dispatch actions from buttons
     *
     * @param e the action event
     */
    public void actionPerformed(ActionEvent e) {
        if (e.getActionCommand().equals(HorizontalButtonPane.CANCEL_CMD)) {
            onExit();
        }
        if (e.getActionCommand().equals(HorizontalButtonPane.OK_CMD)) {
            onInform();
        }
    }

    /**
     * Arranges the widgets.
     */
    private void doTheLayout() {
    	lockableUI = GUIToolKit.setContentPaneAsLockableJXLayer(getRootPane(), getPanel());

        JTabbedPane dd = new JTabbedPane();
        dd.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
        dd.add("Report", getPanel());
        
        Hashtable<String, File> files = new Hashtable<String, File>();
        try {
			files = getAttachedFiles();
            for (File file : files.values()) {
            	if (!file.getName().toLowerCase().endsWith(".jpg")) {
            		JTextArea textArea = new JTextArea(5, 10);
            		textArea.setText(FileTools.readFileAsString(file));
            		textArea.setEditable(false);
            		dd.add(file.getName(), new JScrollPane(textArea));
            	}
            }
        } catch (IOException e) {
			JOptionPane.showMessageDialog(this, "IO-Error while accessing log file!");
		}
        
        HorizontalButtonPane bp = new OkCancelButtonPane("Ok", "Cancel");
        setContentPane(WidgetFactory.makeDialogPanelWithButtons(dd, bp, this));
        pack();
        setLocationRelativeTo(getParent());
    }
    

    protected Hashtable<String, File> getAttachedFiles() throws IOException {
    	Hashtable<String, File> files = new Hashtable<String, File>();
    	try {
    		files.put("log", getReadableLogFile());
    		files.put("systeminfo", makeLogFile("system_info.txt", getSystemInfo()));
    		List<String> rhistory = new Vector<String>();
    		// We copy the history, to avoid a ConcurrentModificationException when there are still R commands executed.
    		for (int i=0; i < RControl.getR().getHistory().size(); i++) {
    			rhistory.add(RControl.getR().getHistory().get(i));
    		}
    		files.put("rcommands", makeLogFile("r_commands.txt", StringTools.collapseStringList(rhistory,"\n")));
    		files.put("sessioninfo", makeLogFile("session_info.txt", getRSessionInfo()));
    		files.put("roptions", makeLogFile("r_options.txt", getROptions()));
    		files.put("traceback", makeLogFile("taceback.txt", getTraceBack()));
    		files.put("graph", makeLogFile("graph.txt", getGraph()));
    		files.put("config", makeLogFile("config.txt", Configuration.getInstance().getConfigurationForDebugPurposes()));
    		//files.put("screen", screen());
    		files.put("abstractR", makeLogFile("abstractR.txt", ReproducableLog.getRLog()));
    		files.put("userInteraction", makeLogFile("userInteraction.txt", ReproducableLog.getGUILog()));
    	} catch (Exception e) {
    		/* Seriously, if something goes wrong here, 
    		 * we simply don't attach the following information.
    		 * Further error handling could easily create infinite loops.
    		 */
    		 JOptionPane.showMessageDialog(this, "Not all information could be collected for error reporting:\n"+e.getMessage(), "Error while reporting error", JOptionPane.ERROR_MESSAGE);
    		 e.printStackTrace();
    	}
        return files;
    }
    
    private String getGraph() {
    	return StringTools.collapseStringArray(RControl.getR().eval("gMCP:::getDebugInfo()").asRChar().getData());
	}

    protected Hashtable<String, String> getInfoTable() {
    	Hashtable<String, String> table = new Hashtable<String,String>();
    	table.put("Contact", tfContact.getText());
    	try {
    		table.put("Shortinfo", OSTools.getShortInfo());
    	} catch (Exception e) {
    		// It is totally okay to ignore errors here... (btw. there should be no error, but better safe than sorry)
    		e.printStackTrace();            		
    	}
    	table.put("Description", taDesc.getText());
    	if (e!=null) {
    		if (e instanceof Throwable) {
    			table.put("A StackTrace", StringTools.stackTraceToString(((Throwable)e)));
    			if (((Throwable)e).getMessage()!=null) {
    				table.put("Error message", ((Throwable)e).getMessage());
    			}    			
    		} else  {
    			table.put("Error", e.toString());
    		}
    	}
    	
    	String prefix = "";
    	if (tfContact.getText().length()>2 || taDesc.getText().length()>2) {
    		prefix = "A FILLED OUT ";
    	}
    	String subject = prefix+"gMCP "+Configuration.getInstance().getGeneralConfig().getVersionNumber()+
    			" (R "+Configuration.getInstance().getGeneralConfig().getRVersionNumber()+") " +
    			"bug report from "+System.getProperty("user.name", "<unknown user name>")+
    			" on "+System.getProperty("os.name", "<unknown OS>")+" : "+    			
    			(message.length()<40?message:message.substring(0, 37)+"...");
    	table.put("Subject", subject);
    	return table;
    }

	protected JPanel getOptionalPanel() {
    	al = LoggingSystem.getInstance().getApplicationLog();
        JPanel p = new JPanel();
        String cols = "left:pref, 5dlu, pref:grow";
        String rows = "pref";
        FormLayout layout = new FormLayout(cols, rows);
        p.setLayout(layout);    

        /* 
         CellConstraints cc = new CellConstraints();
         p.add(new JLabel("attach Data=")),    cc.xy(1, 1));
         p.add(chbAttachDf,                    cc.xy(3, 1)); */
        return p;
    }

    protected JPanel getPanel() {
        JPanel p = new JPanel();
        String cols = "5dlu, left:pref, 5dlu, f:d:g, 5dlu";
        String rows = "5dlu, pref, 5dlu, f:p:g, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu, pref, 5dlu";
        FormLayout layout = new FormLayout(cols, rows);

        p.setLayout(layout);
        CellConstraints cc = new CellConstraints();

        int row = 2;
        
        p.add(new JLabel("<html>" + msg.replaceAll("\n", "<br>") + "</html>"),                  cc.xyw(2, row, 3));

        p.add(taHeader,                                                 cc.xyw(2, row, 3));

        row += 2;

        p.add(new JLabel("Description:"),           cc.xy(1, row));
        JScrollPane sp1 = new JScrollPane(taDesc);
        p.add(sp1,                                                      cc.xy(4, row));

        row += 2;

        p.add(new JLabel("OPTIONAL: If you want to help or get feedback, give us some way to contact you:"), cc.xyw(2, row, 3));

        row += 2;

        p.add(new JLabel("Optional contact (email, phone)"),            cc.xy(2, row));
        p.add(tfContact,                                                  cc.xy(4, row));

        row += 2;

        p.add(getOptionalPanel(),                                       cc.xyw(2, row, 3));

        return p;
    }

    /**
     * @return the location of the (human readable) log file
     */
    public File getReadableLogFile() {
        return new File(getReadableLogFileAppender().getFile());
    }
    
    /**
     * @return the FileAppender for the (human readable) log file 
     */
    public FileAppender getReadableLogFileAppender() {
        return (FileAppender)Logger.getRootLogger().getAppender("READABLE_FILE");
    }

	private String getROptions() {		
		return StringTools.collapseStringArray(RControl.getR().eval("paste(capture.output(options()), collapse=\"\\n\")").asRChar().getData());
	}

	private String getRSessionInfo() {
		return StringTools.collapseStringArray(RControl.getR().eval("paste(capture.output(sessionInfo()), collapse=\"\\n\")").asRChar().getData());
	}

	private String getSystemInfo() {		
		return al.getSystemInfo();
	}
	
	private String getTraceBack() {
		return StringTools.collapseStringArray(RControl.getR().eval("paste(capture.output(traceback()), collapse=\"\\n\")").asRChar().getData());
	}

	/**
     * Create and initialize the widgets.
     */
    private void makeComponents() {
        taHeader = new MultiLineLabel(informMsg);
        tfContact = new JTextField();
        taDesc = new JTextArea(4,30);
    }
    
    /**
     * Handler for exit action. Overwrite this method if you want another behavior.
     */
    protected void onExit() {
    	dispose();
    }

    /**
     * Handler for inform button
     */
    protected void onInform() {
        lockableUI.setLocked(true);

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
                JOptionPane.showMessageDialog(ErrorDialogGMCP.this, msg);
                lockableUI.setLocked(false);
                
                // Open mail client in Java 6:
                String subject = "Error%20report";
                String body = "Description%20and%20contact%20information:";
                String mailtoURI = "mailto:"+ErrorHandler.getInstance().getDeveloperAddress()+"?SUBJECT="+subject+"&BODY="+body;

                /* This is a Wrapper for Desktop.getDesktop().mail(uriMailTo);
                 * that will do that for Java >=6 and nothing for
                 * Java 5.
                 */    
        		try {	
        			URI uriMailTo = new URI(mailtoURI);
        			Method main = Class.forName("java.awt.Desktop").getDeclaredMethod("getDesktop");
        			Object obj = main.invoke(new Object[0]);
        			Method second = obj.getClass().getDeclaredMethod("mail", new Class[] { URI.class }); 
        			second.invoke(obj, uriMailTo);
        		} catch (Exception e) {			
        			logger.warn("No Desktop class in Java 5 or URI error.",e);
        		}
            }

            @Override
            protected void onSuccess(Void result) {                
                dispose();
                JOptionPane.showMessageDialog(ErrorDialogGMCP.this, "Report was sent.");
            }
        };
        worker.execute();
    }
    
    protected void onZip() {    	
    	String info = "gMCP "+Configuration.getInstance().getGeneralConfig().getVersionNumber()+
    			" (R "+Configuration.getInstance().getGeneralConfig().getRVersionNumber()+") " +
    			"bug report from "+System.getProperty("os.name", "<unknown OS>")+": "; 
    	String completeInfo = "Something went wrong.\nWe would be glad, if you could send us a mail to "+ErrorHandler.getInstance().getDeveloperAddress()+" with the following information:\n\n" + info +(stacktrace.isEmpty()?"":"\n\n")+ stacktrace +"\n\n"+ getTraceBack() + "\n\n" + getRSessionInfo();
    	new TextFileViewer(null, "Error", completeInfo);
    	dispose();
    	try {
			getAttachedFiles();
			Zipper.writeIntoZip(tempDir, new File(System.getProperty("java.io.tmpdir"), "gMCP"+Calendar.getInstance().getTime()+".zip"));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	
    }
    
    public File screen() throws IOException {
    	JFrame f = CreateGraphGUI.lastCreatedGUI;
    	BufferedImage image = new BufferedImage(f.getWidth(), f.getHeight(), BufferedImage.TYPE_INT_RGB);
    	Graphics2D graphics2D = image.createGraphics();
    	f.paint(graphics2D);
    	File tempDir = new File(System.getProperty("java.io.tmpdir"));
    	File file = new File(tempDir, "screen.jpg");
    	ImageIO.write(image, "jpeg", file);
    	return file;
    }

    public void showDialog() {
    	try {
    		//new ErrorDialogChooseLevel(null);    	
    		setTitle("Sorry, an error occured - please tell us about it.");
    		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    		addWindowListener(new WindowAdapter() {
    			public void windowClosing(WindowEvent e) {
    				onExit();
    			}
    		});
    		makeComponents();
    		doTheLayout();
    		setResizable(true);
    		setAlwaysOnTop(true);
    		setVisible(true);
    	} catch (Exception e) {	
    		 JOptionPane.showMessageDialog(this, "Error during creating error dialog:\n"+e.getMessage(), "Error creating error dialog", JOptionPane.ERROR_MESSAGE);
    		e.printStackTrace();
    	}
    }  
    
	public static void main(String[] args) {
		RControl.getRControl(true);		
		ErrorHandler.getInstance().makeErrDialog("Report Error");
	}
    
}

