package org.af.gMCP.gui.contrasts;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.af.commons.widgets.lists.SplitListChangeListener;
import org.af.jhlir.call.RDataFrame;
import org.af.jhlir.call.RLegalName;


public class LevelOrderListener implements ActionListener, SplitListChangeListener<String> {
    private RDataFrame df;
    private VarSelectBox cbFactor;
    private LevelOrderSL slLevelOrder;

    private static List<String> lastLevelOrder = new ArrayList<String>();

    public LevelOrderListener(RDataFrame df, VarSelectBox cbFactor, LevelOrderSL slLevelOrder) {
        this.df = df;
        this.cbFactor = cbFactor;
        this.slLevelOrder = slLevelOrder;
        RLegalName g = cbFactor.getSelectedObject();
        //System.out.println("Selected: \""+g+"\"");

        List<String> levels = Arrays.asList(df.getCol(g.toString()).asRFactor().getLevels());

        Collections.sort(levels);
        slLevelOrder.setLeft(levels);
        if (levels.containsAll(lastLevelOrder)) {
            slLevelOrder.setRight(lastLevelOrder);
            levels.removeAll(lastLevelOrder);
            slLevelOrder.setLeft(levels);
        }

        this.cbFactor.addActionListener(this);
        this.slLevelOrder.addSplitListChangeListener(this);
    }


    public void actionPerformed(ActionEvent e) {
        AnalysisDialogListenerFactory.updateLevelOrderList(df, cbFactor, slLevelOrder);
    }

    private void saveLastLevelOrder() {
        lastLevelOrder.clear();
        lastLevelOrder.addAll(slLevelOrder.getRight());
    }

    public void modelStateChanged(List left, List right) {
        saveLastLevelOrder();
    }
}
