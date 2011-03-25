/*
 * This file contains the definition of the NodeTreeModel class,
 * which is used in conjunction with the NodeTreeComponent class.
 * See that class for more information.
 */
package dsa;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.logging.Level;
import java.util.logging.Logger;

public class NodeTreeModel {
  private static Logger logger = Logger.getLogger(NodeTreeModel.class.getName());

  private Node node;
  private Node selected;
  private PropertyChangeSupport changeSupport;

  public NodeTreeModel() {
    node = null;
    selected = null;
    changeSupport = new PropertyChangeSupport(this);
  }

  // This is called by a TreeSelectionListener object in the SimpleTreeFrame
  // class when the user clicks on a new Node.  We change the Node that we
  // care about, and tell our listeners (currently the single NodeTreeComponent)
  // that we've changed so they can repaint a new tree of nodes.
  void setNode(Node node, Node selected) {
    // node can be null, but it's up to listeners to do the right thing
    logger.log(Level.FINE, "setNode called with node: " +
               node == null ? "null" : node.toString());
    logger.log(Level.FINE, "setNode called with selected node: " +
               selected == null ? "null" : selected.toString());
    this.node = node;
    this.selected = selected;
    changeSupport.firePropertyChange("ruleChange", null, null);
  }

  Node getNode() {
    logger.log(Level.FINE, "getNode returning node: " + node);
    return node;
  }

  Node getSelected() {
    logger.log(Level.FINE, "getSelected returning node: " + selected);
    return selected;
  }

  // This allows the NodeTreeComponent to register for any changes of the
  // Node to display
  public void addPropertyChangeListener(PropertyChangeListener listener) {
    changeSupport.addPropertyChangeListener(listener);
  }

  // Not currently used, but is standard
  public void removePropertyChangeListener(PropertyChangeListener listener) {
    changeSupport.removePropertyChangeListener(listener);
  }

  // Not currently used, but is standard
  public PropertyChangeListener[] getPropertyChangeListeners() {
    return changeSupport.getPropertyChangeListeners();
  }
}
