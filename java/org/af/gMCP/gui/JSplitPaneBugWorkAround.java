package org.af.gMCP.gui;

import java.awt.Component;
import java.awt.Graphics;

import javax.swing.JSplitPane;

public class JSplitPaneBugWorkAround extends JSplitPane {
	
	boolean alreadyPainted;
	boolean pleaseSetDividerLocation = false;
	double p;
	
    public JSplitPaneBugWorkAround(int newOrientation,
            Component newLeftComponent,
            Component newRightComponent){
    	super(newOrientation, newLeftComponent, newRightComponent);
	}

	public void paint(Graphics g) {
        if (!alreadyPainted) {       
            if (pleaseSetDividerLocation) {
                super.setDividerLocation(p);
            }
            alreadyPainted = true;
        }
        super.paint(g);
    } 
	
    public void setDividerLocation(double p) {
        if (!alreadyPainted) {       
        	pleaseSetDividerLocation = true;
            this.p = p;
        } else {
            super.setDividerLocation(p);
        }
    }
}
