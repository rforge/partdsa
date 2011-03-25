/*
 * This file contains the definition of the NodeInfoModel class.
 * It underlies the NodeInfoComponent class.
 */
package dsa;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.Icon;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.PlainDocument;

public class NodeInfoModel {
  final private static Logger logger = Logger.getLogger(NodeInfoModel.class.getName());

  final private String title;
  final private String tooltip;
  final private String[] labels;
  final private Map<String,Document> documents;
  final private PropertyChangeSupport changeSupport;
  private Icon image;

  public NodeInfoModel(String title, String tooltip, String... labels) {
    this.title = title;
    this.tooltip = tooltip;
    this.labels = labels;

    changeSupport = new PropertyChangeSupport(this);

    documents = new HashMap<String,Document>();
    for (String label: labels) {
      documents.put(label, new PlainDocument());
    }
  }

  public String getTitle() {
    return title;
  }

  public String getToolTip() {
    return tooltip;
  }

  public String[] getLabels() {
    return labels;
  }

  public Document getDocument(String label) {
    return documents.get(label);
  }

  public Icon getImage() {
    return image;
  }

  public void update(Icon image, Map<String,String> newdata) {
    logger.log(Level.FINE, "new data has arrived");

    this.image = image;

    for (String label: labels) {
      // Get the document object to be modified
      Document doc = documents.get(label);

      // Remove the existing text from the document object
      try {
        doc.remove(0, doc.getLength());

        // Get the new string, and insert it into the document object
        Object value = newdata.get(label);
        if (value != null && value instanceof String) {
          doc.insertString(0, (String) value, null);
        }
      } catch(BadLocationException ble) {
        logger.log(Level.SEVERE, "caught unexpected exception", ble);
      }
    }

    changeSupport.firePropertyChange("imageChange", null, null);
  }

  // This allows the NodeInfoComponent to register for any changes of the
  // Node to display
  public void addPropertyChangeListener(PropertyChangeListener listener) {
    changeSupport.addPropertyChangeListener(listener);
  }
}
