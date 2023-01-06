package org.af.gMCP.gui.contrasts;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.af.commons.widgets.lists.MyJComboBox;
import org.af.commons.widgets.lists.SplitList;
import org.af.commons.widgets.validate.RealTextField;
import org.af.jhlir.call.RDataFrame;
import org.af.jhlir.call.RFactor;
import org.af.jhlir.call.RLegalName;

public class AnalysisDialogListenerFactory {

    public static void updateLevelSelectionBox(RDataFrame df,
                                        MyJComboBox<RLegalName> factorBox,
                                        LevelSelectBox levelBox) {
        RLegalName f = factorBox.getSelectedObject();
        levelBox.setModel(Arrays.asList(((RFactor) df.getCol(f.toString())).getLevels()));
    }

      public static void updateLevelOrderList(RDataFrame df,
                                        MyJComboBox<RLegalName> cbFactor,
                                        SplitList<String> slLevelOrder) {
        RLegalName g = cbFactor.getSelectedObject();
        List<String> groupLevels = Arrays.asList(((RFactor) df.getCol(g.toString())).getLevels());
        slLevelOrder.setLeft(groupLevels);
        slLevelOrder.setRight(new ArrayList<String>());
    }


    public static void updateMarginField(ROptionBox<String> typeBox,
                                         RealTextField marginField) {
        String t = typeBox.getSelectedName();
        if (t.equals("Ratio") || t.equals("Odds Ratio")) {
            marginField.setRange(0d, Double.POSITIVE_INFINITY, false, false);
            marginField.setText("1");
        }
        else {
            marginField.setRange(Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY, false, false);
            marginField.setText("0");
        }
    }

/*    public static void updateMarginTableHazard(VarSelectSL responsesList, ROptionBox<String> typeBox, MarginTable marginTable) {
        List<String> r = new ArrayList<String>();
        for (Object o : responsesList.getSelectedValues())
            r.add(o.toString());
        String t = typeBox.getSelectedName();
        marginTable.setModel(new MarginTableModelHazard(r, t.equals("Ratio")));
    }

    public static void updateMarginTableSafety(VarSelectSL responsesList, ROptionBox<String> typeBox, MarginTable marginTable) {
        List<String> r = new ArrayList<String>();
        for (Object o : responsesList.getSelectedValues())
            r.add(o.toString());
        String t = typeBox.getSelectedName();
        marginTable.setModel(new MarginTableModelSafety(r, t.equals("Ratio")));
    }*/

//    public static ActionListener makeLevelSelectionListener(RDataFrame df,
//                                                             JComboBox factorBox,
//                                                             JComboBox levelBox) {
//        return new LevelSelectionListener(df, factorBox, levelBox);
//    }
//
//
//    public static void makeDefaultGroupBoxListener(RDataFrame df,
//                                                             JComboBox groupBox,
//                                                             JComboBox treatmentGroupBox,
//                                                             JComboBox controlGRoupBox) {
//        new LevelSelectionListener(df, groupBox, treatmentGroupBox);
//        new LevelSelectionListener(df, groupBox, controlGRoupBox);
//    }
//
//    public static ActionListener makeDefaultGroupBoxListener(RDataFrame df,
//                                                             JComboBox groupBox,
//                                                             SplitList levelOrder) {
//        return new LevelOrderListener(df, groupBox, levelOrder);
//    }
//
//
//    public static ActionListener makeDefaultTypeBoxListener(JComboBox typeBox,
//                                                            RealTextField marginField) {
//        return new TypeBoxListener(typeBox, marginField);
//    }
//
//    public static ListSelectionListener makeDefaultResponsesListListener(JList listResponses,
//                                                            JTable tableEM) {
//        return new ResponsesListListener(listResponses, tableEM);
//    }
}


