package org.af.gMCP.gui.pdf;

//import java.awt.Desktop;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JOptionPane;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.tools.OSTools;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Start pdf viewer like Adobe Acrobat.
 */

public class PDFViewerStarter {
    private static final Log logger = LogFactory.getLog(PDFViewerStarter.class);

    // location of executable of pdf viewer
    private final File pdfviewer;
    // option string for pdf viewer
    private final String viewerOptions;


    public PDFViewerStarter(String viewerPath, String viewerOptions, File pdf) {
        this(new File(viewerPath), viewerOptions);
        start(pdf);
    }


    /**
     * Constructor
     *
     * @param viewerPath    absolute path to pdf viewer executable
     * @param viewerOptions option string for pdf viewer
     */
    public PDFViewerStarter(String viewerPath, String viewerOptions) {
        this(new File(viewerPath), viewerOptions);
    }

    /**
     * Constructor
     *
     * @param pdfviewer     location of executable of pdf viewer
     * @param viewerOptions option string for pdf viewer
     */

    public PDFViewerStarter(File pdfviewer, String viewerOptions) {
        this.pdfviewer = pdfviewer;
        this.viewerOptions = viewerOptions;
    }

    // TOdo its bad to do the errorhandling here, but atm i cant decouple it better
    // but keep this stuff completely here
    public void start(File pdf) {
        Exception exc = null;
        try {
            if (pdfviewer != null) {
                if (!pdfviewer.exists() || !pdf.isFile()) {
                    // something wrong with path to viewer
                    String msg = "The PDF viewer you supplied in the options\n" +
                            "does not point to a valid file!\n" +
                            "Please try to correct your options.";
                    if (OSTools.isWindows())
                        msg += "\n\nTrying to open with associated\n " +
                                "application for file type \"pdf\" now...";
                    JOptionPane.showMessageDialog(null, msg);
                    // try to use OS to open pdf
                    startByOS(pdf);
                } else {
                    startByConfiguredViewer(pdf);
                }
            } else {
                // no pdf viewer given
                startByOS(pdf);
            }
        } catch (IOException e) {
            exc = e;
        } catch (InterruptedException e) {
            exc = e;
        }
        if (exc != null) {
            String msg = "There was an error opening the pdf file!\n" +
                    "Please check the pdf viewer options.\n" +
                    "\nError was: " + exc.getMessage();
            ErrorHandler.getInstance().makeErrDialog(msg, exc);
        }
    }


    /**
     * opens pdf viewer with the given pdf file
     * uses the viewer passed to the contructor
     *
     * @param pdf pdf file to display
     * @throws IOException when IO error occurs when opening the pdf viewer
     */
    private void startByConfiguredViewer(File pdf) throws IOException {
        // transform options string to list
        List<String> command = new Vector<String>();
        command.add(pdfviewer.getAbsolutePath());
        if (!viewerOptions.equals("")) {
            StringTokenizer st = new StringTokenizer(viewerOptions);
            while(st.hasMoreTokens()) {
                command.add(st.nextToken());
            }
        }
        // add pdf file location to command list
        command.add(pdf.getAbsolutePath());
        logger.info("Starting PDF viewer: " + command);
        // start pdf viewer
        ProcessBuilder pb = new ProcessBuilder(command);
        pb.start();
    }

    private void startByOS(File pdf) throws IOException, InterruptedException {
        if (OSTools.isWindows())
            startByRunDLL(pdf);
    }


    /**
     * opens pdf viewer with the given pdf file
     * uses RunDll in windows
     *
     * @param pdf pdf file to display
     * @throws IOException when IO error occurs when opening the pdf viewer
     */
    private void startByRunDLL(File pdf) throws IOException, InterruptedException {
        Process p = null;
        p = Runtime.getRuntime().exec("rundll32 " +
                "url.dll,FileProtocolHandler " + pdf.getAbsolutePath());
        p.waitFor();
    }

    /**
     * opens pdf viewer with the given pdf file
     *
     * @param pdfPath absolute path of pdf file to display
     * @throws IOException when IO error occurs when opening the pdf viewer
     */
    public void start(String pdfPath) throws IOException, InterruptedException {
        start(new File(pdfPath));
    }
}
