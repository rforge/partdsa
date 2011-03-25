/*
 * This file contains the definition of the NodeTreeComponent class.
 * It extends the JComponent class, and is used for displaying a
 * decision tree.  Most of the work, however, is done in an inner
 * class, called TreeBox.
 */
package dsa;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.FontRenderContext;
import java.awt.font.LineMetrics;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JComponent;

@SuppressWarnings("serial")
public class NodeTreeComponent extends JComponent implements PropertyChangeListener {
  private static Logger logger = Logger.getLogger(NodeTreeComponent.class.getName());

  private NodeTreeModel model;
  private TreeBox treeBox;
  private Font font;
  private List<ActionListener> listenerList;
  private int eventID = 0;

  public NodeTreeComponent(NodeTreeModel model) {
    this.model = model;
    if (model.getNode() != null)
      this.treeBox = new TreeBox(model.getNode(), model.getSelected());
    else
      this.treeBox = null;
    this.font = new Font(Font.SANS_SERIF, Font.BOLD, 14);
    addMouseListener(new MouseHandler());
    model.addPropertyChangeListener(this);
    listenerList = new ArrayList<ActionListener>();
  }

  @Override
  public Dimension getPreferredSize() {
    return new Dimension(200, 160);
  }

  @Override
  public Dimension getMinimumSize() {
    return new Dimension(140, 100);
  }

  @Override
  public void paintComponent(Graphics g) {
    Graphics2D g2 = (Graphics2D) g;
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

    if (treeBox != null) {
      logger.log(Level.FINE, "calling treeBox.display");
      Dimension d = getSize();
      treeBox.setdim(0.0, 0.0, d.width, d.height);
      treeBox.display(g2, null);
    } else {
      logger.log(Level.FINE, "treeBox is null");
    }
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    // The model may have changed to select a different tree to display
    if (model.getNode() != null) {
      logger.log(Level.FINE, "constructing a new treeBox");
      treeBox = new TreeBox(model.getNode(), model.getSelected());
    } else {
      logger.log(Level.FINE, "setting treeBox to null");
      treeBox = null;
    }

    repaint();
  }

  // XXX Currently ignoring mouse events
  private class MouseHandler extends MouseAdapter {
    public void mouseClicked(MouseEvent event) {
      // String command = find(event.getPoint());
      String command = "foo";
      fireActionPerformed(new ActionEvent(this, eventID, command));
      eventID += 1;
    }
  }

  public void addActionListener(ActionListener l) {
    listenerList.add(l);
  }

  public void removeActionListener(ActionListener l) {
    if (l != null)
      listenerList.remove(l);
  }

  public ActionListener[] getActionListeners() {
    return listenerList.toArray(new ActionListener[0]);
  }

  protected void fireActionPerformed(ActionEvent event) {
    for (ActionListener l: listenerList) {
      l.actionPerformed(event);
    }
  }

  class TreeBox {
    private final Node node;
    private final Node selected;
    private final TreeBox left;
    private final TreeBox right;

    private double xorigin;
    private double yorigin;
    private double width;
    private double height;
    private Double lwidth;
    private Double lheight;
    private boolean fixed;

    TreeBox(Node node, Node selected) {
      assert node != null;
      this.node = node;
      this.selected = selected;

      if (node.left != null && node.right != null) {
        left = new TreeBox(node.left, selected);
        right = new TreeBox(node.right, selected);

        // Get the height of the larger child
        double cheight = Math.max(left.getLHeight(), right.getLHeight());

        // Set the height of shorter child to the height of the taller
        left.setLHeight(cheight);
        right.setLHeight(cheight);

        // These need to be null when there are children
        lwidth = null;
        lheight = null;
      } else {
        // No children, just the one node
        left = null;
        right = null;
        lwidth = 1.0;
        lheight = 1.0;
      }

      // This is set to true by setdim
      fixed = false;
    }

    boolean display(Graphics2D g2, Point2D.Double pcoord) {
      assert fixed;
      assert node != null;

      // Indicates if this node is along the path to a selected node
      boolean selectedPath = false;

      // Indicates if this node is a leaf node.
      // Note that we should have either zero or two children: never one!
      boolean leafNode = ! (getLeft() != null && getRight() != null);

      // Compute the coordinates of this node
      Point2D.Double p = new Point2D.Double(getX(), getY());

      // Process any child nodes, and determine if we are along the path
      // to a selected node
      if (! leafNode) {
        if (getLeft().display(g2, p))
          selectedPath = true;
        if (getRight().display(g2, p))
          selectedPath = true;
      }

      // This causes us to highlight the selected path to non-leaf nodes
      if (node == selected || (node.pname() != null && node.pname().equals(selected.pname()))) {
        selectedPath = true;
      }

      // Draw a connecting line if we have a parent node
      if (pcoord != null) {
        Line2D.Double line = new Line2D.Double(pcoord, p);
        float thickness = selectedPath ? 5.0F : 2.0F;
        BasicStroke stroke = new BasicStroke(thickness);
        g2.setStroke(stroke);
        Color lineColor = selectedPath ? new Color(247,186,92) : new Color(50,56,60);
        g2.setPaint(lineColor);
        g2.draw(line);
      }

      // Determine the label to use for this node
      String msg = "???";
      if (node != null) {
        if (node.split != null) {
          msg = node.split.toString();
        } else {
          msg = node.toString();
        }
      }

      // Get "bounds" information for this node based on
      // the label that we just determined
      FontRenderContext context = g2.getFontRenderContext();
      LineMetrics metrics = font.getLineMetrics(msg, context);
      Rectangle2D bounds = font.getStringBounds(msg, context);

      // Create rectangle objects for the node and its shadow
      double border = 4.0;
      double w = bounds.getWidth() + 2.0 * border;
      double h = bounds.getHeight() + 2.0 * border;
      double arc = h / 2.0;
      double x1 = p.x - w / 2.0;
      double y1 = p.y - h / 2.0;
      RoundRectangle2D.Double rect = new RoundRectangle2D.Double(x1, y1, w, h, arc, arc);
      RoundRectangle2D.Double shadow = new RoundRectangle2D.Double(x1 + 2.0, y1 + 4.0,
                                                                   w, h, arc, arc);

      // Draw the shadow
      Color shadowColor = new Color(50,56,60,100);
      g2.setPaint(shadowColor);
      g2.fill(shadow);

      // Get the colors to use for the node and its label
      Color nodeColorLight;
      Color nodeColorDark;
      Color textColor;

      if (leafNode) {
        if (node == selected) {
          // This is a selected leaf node
          nodeColorLight = new Color(62,117,108);
          nodeColorDark = new Color(19,102,88);
          textColor = Color.white;
        } else if (node.pname() != null && node.pname().equals(selected.pname())) {
          // This is a leaf node in the same partition as the selected leaf node
          nodeColorLight = new Color(62,117,108);
          nodeColorDark = new Color(19,102,88);
          textColor = Color.white;
        } else {
          // This is a non-selected leaf node
          nodeColorLight = new Color(135,206,194);
          nodeColorDark = new Color(109,206,190);
          textColor = Color.black;
        }
      } else {
        if (node == selected) {
          // This is a selected non-leaf node
          nodeColorLight = new Color(69,93,121);
          nodeColorDark = new Color(22,61,105);
          textColor = Color.white;
        } else {
          // This is a non-selected, non-leaf node
          nodeColorLight = new Color(142,173,208);
          nodeColorDark = new Color(118,161,208);
          textColor = Color.black;
        }
      }

      // Draw the node
      Point2D lpoint = new Point2D.Double(p.x - h / 6.0, y1);
      Point2D mpoint = new Point2D.Double(p.x + h / 6.0, y1 + h);
      GradientPaint gp = new GradientPaint(lpoint, nodeColorLight, mpoint, nodeColorDark);
      g2.setPaint(gp);
      g2.fill(rect);

      // Draw the node's label
      g2.setFont(font);
      g2.setPaint(textColor);
      int ix = (int) (x1 + border);
      int iy = (int) (y1 + border + metrics.getAscent());
      g2.drawString(msg, ix, iy);

      // Propagate the "selected path" status to our parent
      return selectedPath;
    }

    void setLHeight(double lheight) {
      assert ! fixed;
      if (getLeft() != null && getRight() != null) {
        getLeft().setLHeight(lheight - 1.0);
        getRight().setLHeight(lheight - 1.0);
      } else {
        this.lheight = lheight;
      }
    }

    void setdim(double xorigin, double yorigin, double width, double height) {
      this.fixed = true;
      this.xorigin = xorigin;
      this.yorigin = yorigin;
      this.width = width;
      this.height = height;

      if (getLeft() != null && getRight() != null) {
        double pheight = 1.0 * height / getLHeight();
        double cheight = height - pheight;
        double lcwidth = getLeft().getLWidth() * width / getLWidth();
        double rcwidth = width - lcwidth;
        getLeft().setdim(xorigin, yorigin + pheight,
                         lcwidth, cheight);
        getRight().setdim(xorigin + lcwidth, yorigin + pheight,
                          rcwidth, cheight);
      }
    }

    double getLWidth() {
      if (lwidth != null) {
        return lwidth;
      } else {
        assert getLeft() != null && getRight() != null;
        return getLeft().getLWidth() + getRight().getLWidth();
      }
    }

    double getWidth() {
      assert fixed;
      return width;
    }

    double getLHeight() {
      if (lheight != null) {
        return lheight;
      } else {
        double h = 1.0;
        if (getLeft() != null && getRight() != null)
          h += Math.max(getLeft().getLHeight(), getRight().getLHeight());
        return h;
      }
    }

    double getHeight() {
      assert fixed;
      return height;
    }

    double getLX() {
      if (getLeft() != null && getRight() != null) {
        return (getLeft().getLWidth() + getLeft().getLX() +
                getRight().getLX()) / 2.0;
      } else {
        return getLWidth() / 2.0;
      }
    }

    double getX() {
      assert fixed;
      return xorigin + getLX() * getWidth() / getLWidth();
    }

    double getLY() {
      return 0.5;
    }

    double getY() {
      assert fixed;
      return yorigin + getLY() * getHeight() / getLHeight();
    }

    TreeBox getLeft() {
      return left;
    }

    TreeBox getRight() {
      return right;
    }
  }
}
