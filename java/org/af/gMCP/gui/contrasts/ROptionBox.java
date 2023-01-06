package org.af.gMCP.gui.contrasts;

import java.util.Arrays;
import java.util.List;

import org.af.commons.widgets.lists.MyJComboBox;

public class ROptionBox<E> extends MyJComboBox<E> {
    private String option;

    public ROptionBox(String option, List<E> rOptions) {
        super(rOptions);
        this.option = option;
    }

    public ROptionBox(String option, E[] rOptions) {
        this(option, Arrays.asList(rOptions));
    }

    public ROptionBox(String option, List<String> readableOptions, List<E> rOptions) {
        super(rOptions, readableOptions);
        this.option = option;
    }

    public ROptionBox(String option, String[] readableOptions, E[] rOptions) {
        this(option, Arrays.asList(readableOptions), Arrays.asList(rOptions));
    }
}
