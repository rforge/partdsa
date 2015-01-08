/*
 * This file contains the definition of the NodeInfoComponent class.
 * It extends the JComponent class, and is used for displaying
 * information about a partition that is represented by a node.
 */
package dsa;

import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.text.Document;

@SuppressWarnings("serial")
public class NodeInfoComponent extends JComponent implements PropertyChangeListener {
  private static Logger logger = Logger.getLogger(NodeInfoComponent.class.getName());

  private NodeInfoModel model;
  final private JLabel plot;

  public NodeInfoComponent(NodeInfoModel model) {
    this.model = model;
    model.addPropertyChangeListener(this);
    setLayout(new GridBagLayout());

    GridBagConstraints gbc = new GridBagConstraints();
    gbc.weightx = 100;
    gbc.weighty = 0;
    gbc.gridx = 0;
    gbc.gridy = 0;
    gbc.gridwidth = 2;
    gbc.gridheight = 1;
    gbc.insets = new Insets(12, 6, 6, 6);
    JLabel title = new JLabel(model.getTitle());
    if (model.getToolTip() != null)
      title.setToolTipText(model.getToolTip());
    title.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 20));
    add(title, gbc);

    String[] labels = model.getLabels();
    for (int i = 0; i < labels.length; i++) {
      // Get the document to modify
      String label = labels[i];
      Document doc = model.getDocument(label);

      // Put a label into the panel
      JLabel lab = new JLabel(label);
      GridBagConstraints lgbc = new GridBagConstraints();
      lgbc.weightx = 0;
      lgbc.weighty = 0;
      lgbc.gridx = 0;
      lgbc.gridy = i + 1;
      lgbc.gridwidth = 1;
      lgbc.gridheight = 1;
      lgbc.anchor = GridBagConstraints.WEST;
      add(lab, lgbc);

      // Put a text field into the panel
      JTextField tf = new JTextField(doc, "", 10);
      tf.setEditable(false);
      GridBagConstraints tgbc = new GridBagConstraints();
      tgbc.weightx = 100;
      tgbc.weighty = 0;
      tgbc.weighty = 0;
      tgbc.gridx = 1;
      tgbc.gridy = i + 1;
      tgbc.gridwidth = 1;
      tgbc.gridheight = 1;
      tgbc.fill = GridBagConstraints.HORIZONTAL;
      add(tf, tgbc);
    }

    // Add a JLabel component to fill the space at the bottom
    gbc = new GridBagConstraints();
    gbc.weightx = 100;
    gbc.weighty = 100;
    gbc.gridx = 0;
    gbc.gridy = labels.length + 1;
    gbc.gridwidth = 2;
    gbc.gridheight = 1;
    Icon image = model.getImage();
    plot = new JLabel(image);
    add(plot, gbc);
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    // The model may have changed to select a different node to display
    plot.setIcon(model.getImage());
  }
}
