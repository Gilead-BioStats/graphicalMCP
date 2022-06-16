package org.af.gMCP.gui.contrasts;

import java.awt.Component;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;

import org.af.commons.Localizer;
import org.af.commons.widgets.WidgetFactory;
import org.af.commons.widgets.validate.RealTextField;
import org.af.jhlir.call.RDataFrame;
import org.af.jhlir.call.RLegalName;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

// TODO Generalize this!

// makeresponsebox ----> makenumericVarbox
public class AnalysisDialogFactory {
    private static final Log logger = LogFactory.getLog(AnalysisDialogFactory.class);
    protected static Localizer localizer = Localizer.getInstance();

    private static String[] stringListToArray(List<String> list) {
        String[] result = new String[0];
        result = list.toArray(result);
        return result;
    }

    private static List<RLegalName> getNumVars(RDataFrame df) {
        return df.getNumberVarsLN();
    }

    private static List<RLegalName> getIntVars(RDataFrame df) {
        return df.getIntegerVarsLN();
    }

    private static List<RLegalName> getVars(RDataFrame df) {
        return df.getColNamesLN();
    }

    private static List<RLegalName> getFactorVars(RDataFrame df) {
        return df.getFactorVarsLN();
    }

    public static VarSelectBox makeNumericVarBox(RDataFrame df) {
        return new VarSelectBox(getNumVars(df));
    }

    public static VarSelectBox makeVarBox(RDataFrame df) {
        return new VarSelectBox(getVars(df));
    }

    public static VarSelectBox makeIntegerVarBox(RDataFrame df) {
        return new VarSelectBox(getIntVars(df));
    }

    /*public static VarSelectSL makeResponsesList(RDataFrame df) {
        return new VarSelectSL(df.getNumberVarsLN());
    }
    
    public static VarSelectSL makeRegressorsList(RDataFrame df) {
        return new VarSelectSL(df.getColNamesLN());
    }*/

    public static VarSelectBox makeFactorVarBox(RDataFrame df) {
        return new VarSelectBox(getFactorVars(df));
    }

    public static VarSelectBox makeFactorNumVarBox(RDataFrame df) {
    	List<RLegalName> v = new ArrayList<RLegalName>();
        v.addAll(df.getNumberVarsLN());
        v.addAll(df.getFactorVarsLN());
        return new VarSelectBox(v);
    }

    public static JComboBox makeFactorAndIntVarBox(RDataFrame df) {
        List<String> list = df.getFactorVars();
        list.addAll(df.getIntegerVars());
        return new JComboBox(list.toArray());
    }

    /*public static LevelSelectBox makeLevelSelectionBox(RDataFrame df, VarSelectBox factorBox) {
        LevelSelectBox b = new LevelSelectBox();
        AnalysisDialogListenerFactory.updateLevelSelectionBox(df, factorBox, b);
        new LevelSelectionListener(df, factorBox, b);
        return b;
    }*/

    public static GroupingBox makeGroupingComboBox(RDataFrame df) {
        GroupingBox gcb = new GroupingBox(getFactorVars(df));
        RLegalName g = GroupingListener.getLastGrouping();
        if (g != null && gcb.getAllGroupings().contains(g))
            gcb.setSelectedItem(g);
        gcb.addActionListener(new GroupingListener());
        return gcb;
    }


    public static LevelOrderSL makeLevelOrderSL(RDataFrame df, VarSelectBox factorBox) {
        LevelOrderSL sl = new LevelOrderSL();
        new LevelOrderListener(df, factorBox, sl);
        return sl;
    }

    /*public static VarSelectSL makeVarSelectSL(RDataFrame df) {
        return new VarSelectSL(getVars(df));
    }

    public static CovariatesSL makeCovariatesSL(RDataFrame df) {
        List<RLegalName> v = new ArrayList<RLegalName>();
        v.addAll(df.getNumberVarsLN());
        v.addAll(df.getFactorVarsLN());
        return new CovariatesSL(v);
    }

    public static VarSelectSL makeFactorsSL(RDataFrame df) {
        List<RLegalName> v = new ArrayList<RLegalName>();
        v.addAll(df.getFactorVarsLN());
        return new VarSelectSL(v);
    }

    public static VarSelectSL makeIntegersSL(RDataFrame df) {
        List<RLegalName> v = new ArrayList<RLegalName>();
        v.addAll(df.getIntegerVarsLN());
        return new VarSelectSL(v);
    }

    public static VarSelectSL makeNumsSL(RDataFrame df) {
        List<RLegalName> v = new ArrayList<RLegalName>();
        v.addAll(df.getNumericVarsLN());
        return new VarSelectSL(v);
    }*/

    public static ROptionBox<String> makeAlternativeBox1() {
        String[] a = {localizer.getString("SHARED_DIALOGFACTORY_TWOSIDED"), 
        		localizer.getString("SHARED_DIALOGFACTORY_LESS"), 
        		localizer.getString("SHARED_DIALOGFACTORY_GREATER")};
        String[] b = {"two.sided", "less", "greater"};
        return new ROptionBox<String>("alternative", a, b);
    }

    public static ROptionBox<String> makeAlternativeBox1b() {
        String[] a = {localizer.getString("SHARED_DIALOGFACTORY_LESS"), 
        		localizer.getString("SHARED_DIALOGFACTORY_GREATER")};
        String[] b = {"less", "greater"};
        return new ROptionBox<String>("alternative", a, b);
    }

    public static ROptionBox<String> makeAlternativeBox2() {
        String[] a = {localizer.getString("SHARED_DIALOGFACTORY_TWOSIDED"), 
        		localizer.getString("SHARED_DIALOGFACTORY_LOWER"), 
        		localizer.getString("SHARED_DIALOGFACTORY_UPPER")};
        String[] b = {"two.sided", "lower", "upper"};
        return new ROptionBox<String>("alternative", a, b);
    }

    public static ROptionBox<String> makeDistBox1() {
        String[] a = {"Negative Binomial", "Quasipoisson", "Poisson"};
        String[] b = {"NegBin", "QuasiPoisson", "Poisson"};
        return new ROptionBox<String>("dist", a, b);
    }

    public static ROptionBox<String> makeDistBox2() {
        String[] a = {"Quasibinomial", "Betabinomial", "Binomial"};
        String[] b = {"QuasiBinomial", "BetaBinomial", "Binomial"};
        return new ROptionBox<String>("dist", a, b);
    }

    public static ROptionBox<String> makeTypeBox1() {
        String[] a = {localizer.getString("SHARED_DIALOGFACTORY_RATIO"), localizer.getString("SHARED_DIALOGFACTORY_DIFFERENCE")};
        String[] b = {"ratio", "difference"};
        ROptionBox<String> cb = new ROptionBox<String>("type", a, b);
        return cb;
    }

    public static ROptionBox<String> makeTypeBox2() {
        String[] a = {localizer.getString("SHARED_DIALOGFACTORY_RATIO"), localizer.getString("SHARED_DIALOGFACTORY_DIFFERENCE"), localizer.getString("SHARED_DIALOGFACTORY_ODDSRATIO")};
        String[] b = {"ratio", "difference", "oddsratio"};
        return new ROptionBox<String>("type", a, b);
    }

    public static ROptionBox<String> makeComparisonBox1() {
        String[] a = {"Comparison to Control", "Trend", "Down-turn-protected trend", "Vehicle-Doses-Active", "Comparison to grand mean"};
        String[] b = {"Dunnett", "Williams", "UmbrellaWilliams", "VDsA", "GrandMean"};
        return new ROptionBox<String>("cmenthod", a, b);
    }

    public static ROptionBox<String> makeComparisonBox1x() {
        String[] a = {"Comparison to Control", "Trend", "Down-turn-protected trend", "Vehicle-Doses-Active", "Comparison to grand mean", "User defined"};
        String[] b = {"Dunnett", "Williams", "UmbrellaWilliams", "VDsA", "GrandMean", "UserDefined"};
        return new ROptionBox<String>("cmenthod", a, b);
    }


    public static ROptionBox<String> makeComparisonBox2() {
        String[] a = {"Comparison to Control", "Trend", "Down-turn-protected trend"};
        String[] b = {"Dunnett", "Williams", "UmbrellaWilliams"};
        return new ROptionBox<String>("cmenthod", a, b);
    }

    public static ROptionBox<String> makeComparisonBox2x() {
        String[] a = {"Comparison to Control", "Trend", "Down-turn-protected trend", "User defined"};
        String[] b = {"Dunnett", "Williams", "UmbrellaWilliams", "UserDefined"};
        return new ROptionBox<String>("cmenthod", a, b);
    }

    public static ROptionBox<String> makeComparisonBox3() {
        String[] a = {"Trend", "Down-turn-protected trend", "Change Point"};
        String[] b = {"Williams", "UmbrellaWilliams", "ChangePoint"};
        return new ROptionBox<String>("cmenthod", a, b);
    }

    public static ROptionBox<String> makeComparisonBox4() {
        String[] a = {"Comparison to Control", "Trend", "Down-turn-protected trend", "All-pair", "Sequence", "Change Point"};
        String[] b = {"Dunnett", "Williams", "UmbrellaWilliams", "Tukey", "Sequen", "Changepoint"};
        return new ROptionBox<String>("cmenthod", a, b);
    }

    public static ROptionBox<String> makeComparisonBox5() {
        String[] a = {"Comparison to Control", "All-pair", "Sequence"};
        String[] b = {"Dunnett", "Tukey", "Sequen"};
        return new ROptionBox<String>("cmenthod", a, b);
    }

    public static ROptionBox<String> makeCorStructBox() {
        String[] a = {"No Correlation Structure", "Autoregressive Order 1", "Continuous Autoregressive Order 1", "Unstructured"};
        String[] b = {"no", "AR1", "CAR1", "UN"};
        return new ROptionBox<String>("cmenthod", a, b);
    }

    public static ROptionBox<String> makeDoseScoreBox() {
        String[] a = {"ordinal", "log-lin", "numeric"};
        String[] b = {"ordinal", "loglin", "num"};
        return new ROptionBox<String>("scoretype", a, b);
    }

    public static ROptionBox<String> makeVarianceBox2() {
        String[] a = {localizer.getString("SHARED_DIALOGFACTORY_HETEROGENOUS"), localizer.getString("SHARED_DIALOGFACTORY_HOMOGENOUS"), 
        		"Bofinger (Only for Type: Difference)"};
        String[] b = {"var.unequal", "var.equal", "Bofinger"};
        return new ROptionBox<String>("var.equal", a, b);
    }

    public static ROptionBox<Boolean> makeVarianceBox() {
        String[] a = {localizer.getString("SHARED_DIALOGFACTORY_HETEROGENOUS"), localizer.getString("SHARED_DIALOGFACTORY_HOMOGENOUS")};
        Boolean[] b = {false, true};
        return new ROptionBox<Boolean>("var.equal", a, b);
    }


    public static RealTextField makeConfidenceField() {
        RealTextField tf = new RealTextField("Confidence", 0.5d, 1d, false, false);
        tf.setText("0.95");
        return tf;
    }

    public static RealTextField makeAlphaField() {
        RealTextField tf = new RealTextField("Alpha", 0d, 0.5d, false, false);
        tf.setText("0.05");
        return tf;
    }

    /*public static RealTextField makeMarginField(ROptionBox<String> typeBox) {
        RealTextField tf = new RealTextField("Testmargin");
        if (typeBox.getSelectedObject().equals("difference")) {
        	tf.setText("0");
        } else {
        	tf.setText("1");
        }
        new TypeBoxListener(typeBox, tf);
        return tf;
    }


    public static MarginTable makeMarginTableHazard(VarSelectSL responsesList, ROptionBox<String> cbType) {
        MarginTable tableMargin = new MarginTable(new MarginTableModelHazard(cbType.getSelectedName().equals("Ratio")));
        new ResponsesListListenerHazard(responsesList, cbType, tableMargin);
        return tableMargin;
    }

	public static MarginTable makeMarginTableHazard(
			LevelOrderSL slLevelOrder, ROptionBox<String> cbType) {
		MarginTable tableMargin = new MarginTable(new MarginTableModelHazard(cbType.getSelectedName().equals("Ratio")));
        new LevelListenerHazard(slLevelOrder, cbType, tableMargin);
        return tableMargin;
	}

    public static MarginTable makeMarginTableSafety(
    		VarSelectSL responsesList, ROptionBox<String> cbType) {
        MarginTable tableMargin = new MarginTable(new MarginTableModelSafety(cbType.getSelectedName().equals("Ratio")));
        new ResponsesListenerSafety(responsesList, cbType, tableMargin);
        return tableMargin;
    }

	public static MarginTable makeMarginTableSafety(
			LevelOrderSL slLevelOrder, ROptionBox<String> cbType) {
		MarginTable tableMargin = new MarginTable(new MarginTableModelSafety(cbType.getSelectedName().equals("Ratio")));
        new LevelListenerSafety(slLevelOrder, cbType, tableMargin);
        return tableMargin;
	}*/


    public static Box makeEnablerBox(Component component) {
        JCheckBox chb = new JCheckBox();
        WidgetFactory.registerEnabler(chb, component);
        return makeEnablerBox(component, chb, true);
    }

    public static Box makeEnablerBox(Component component, JCheckBox enabler) {
        return makeEnablerBox(component, enabler, true);
    }

    public static Box makeEnablerBox(Component component, JCheckBox enabler, boolean boxIsLeft) {
        Box box = Box.createHorizontalBox();
        if (boxIsLeft) {
            box.add(enabler);
            box.add(component);
        } else {
            box.add(component);
            box.add(enabler);
        }
        return box;
    }




	public static ROptionBox<String> makeAsyMethod() {
        String[] a = {"Logit", "Probit", "Multi-Normal", "Multi-t"};
        String[] b = {"logit", "probit", "normal", "mult.t"};
        return new ROptionBox<String>("asymenthod", a, b);
	}


}



