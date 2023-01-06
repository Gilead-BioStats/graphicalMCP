package org.af.gMCP.gui;

import java.awt.Dimension;
import java.awt.FileDialog;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.rosuda.JRI.RMainLoopCallbacks;
import org.rosuda.JRI.Rengine;

/**
 * This DebugTextConsole is only attached for development if java is  
 * started with VM arguments -Declipse="true".
 */
public class DebugTextConsole implements RMainLoopCallbacks {
	JFrame f;

	public JTextArea textarea = new JTextArea();

	public DebugTextConsole() {
		f = new JFrame();
		f.getContentPane().add(new JScrollPane(textarea));
		f.setSize(new Dimension(800,600));
	}
	
	public void setVisible(boolean visible) {
		f.setVisible(visible);
	}

	public void rWriteConsole(Rengine re, String text, int oType) {
		textarea.append(text);
	}

	public void rBusy(Rengine re, int which) {
		System.out.println("rBusy("+which+")");
	}

	public String rReadConsole(Rengine re, String prompt, int addToHistory) {
		System.out.print(prompt);
		try {
			BufferedReader br=new BufferedReader(new InputStreamReader(System.in));
			String s=br.readLine();
			return (s==null||s.length()==0)?s:s+"\n";
		} catch (Exception e) {
			System.out.println("jriReadConsole exception: "+e.getMessage());
		}
		return null;
	}

	public void rShowMessage(Rengine re, String message) {
		System.out.println("rShowMessage \""+message+"\"");
	}

	public String rChooseFile(Rengine re, int newFile) {
		FileDialog fd = new FileDialog(f, (newFile==0)?"Select a file":"Select a new file", (newFile==0)?FileDialog.LOAD:FileDialog.SAVE);
		fd.setVisible(true);
		String res=null;
		if (fd.getDirectory()!=null) res=fd.getDirectory();
		if (fd.getFile()!=null) res=(res==null)?fd.getFile():(res+fd.getFile());
		return res;
	}

	public void rFlushConsole(Rengine re) {}

	public void rLoadHistory(Rengine re, String filename) {}			

	public void rSaveHistory(Rengine re, String filename) {}			
}