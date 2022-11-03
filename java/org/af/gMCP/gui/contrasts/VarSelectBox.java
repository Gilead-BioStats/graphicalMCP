package org.af.gMCP.gui.contrasts;

import java.util.List;

import org.af.commons.widgets.lists.MyJComboBox;
import org.af.jhlir.call.RLegalName;


public class VarSelectBox extends MyJComboBox<RLegalName>{
    public VarSelectBox(List<RLegalName> objects) {
        super(objects);
    }

    public VarSelectBox(RLegalName[] objects) {
        super(objects);
    }
}
