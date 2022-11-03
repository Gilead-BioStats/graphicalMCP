package org.mutoss.gui;

import java.awt.Image;
import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

public class TransferableImage implements Transferable {

	protected Image image;
	
	public TransferableImage(Image image) {
		this.image = image;
	}
	
	public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
		if (!flavor.equals(DataFlavor.imageFlavor)) {
			throw new UnsupportedFlavorException(flavor);
		}
		return image;
	}

	public DataFlavor[] getTransferDataFlavors() {
		return new DataFlavor[] { DataFlavor.imageFlavor };
	}

	public boolean isDataFlavorSupported(DataFlavor flavor) {
		return flavor.equals(DataFlavor.imageFlavor);
	}

	public static void copyImageToClipboard(Image image) {
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new TransferableImage(image), null);
    }
}
