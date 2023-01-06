package org.af.gMCP.gui.contrasts;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.af.jhlir.call.RLegalName;


public class GroupingListener implements ActionListener {
    private static RLegalName lastGrouping;

    public void actionPerformed(ActionEvent e) {
        GroupingBox gcb = (GroupingBox) e.getSource();
        if (gcb.getGrouping() != null)
            lastGrouping = gcb.getGrouping();   
    }

    public static RLegalName getLastGrouping() {
        return lastGrouping;
    }
}
