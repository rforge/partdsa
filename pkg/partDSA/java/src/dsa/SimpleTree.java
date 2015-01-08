/*
 * This file contains the definition of the SimpleTree class, which
 * is the "main program" of an application that is used to display
 * models created by the partDSA package.  The models are stored as
 * XML files that are created via the partDSA "dumpDSA" method.
 *
 * When this program is started, it displays a blank screen.  You
 * can open one or more "dumpDSA" file, using the "File -> Open"
 * menu item.  Each model that is opened will be displayed in a
 * different tab.
 *
 * Each model actually contains multiple trees, with each tree having
 * a different maximum number of partitions.  You can select the
 * tree to view via the left panel, which contains a Swing JTree
 * component.  The JTree also allows you to display information about
 * different nodes within the decision tree.
 */
package dsa;

import java.awt.BorderLayout;
import java.awt.Event;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTree;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;

public class SimpleTree {
  private static Logger logger = Logger.getLogger(SimpleTree.class.getName());
  private static String OSNAME = System.getProperty("os.name");

  public static void main(final String[] args) {
    if (OSNAME.startsWith("Mac")) {
      System.setProperty("apple.laf.useScreenMenuBar", "true");
    }

    EventQueue.invokeLater(new Runnable() {
      public void run() {
        JFrame frame = new SimpleTreeFrame(args);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
      }
    });
  }
}

@SuppressWarnings("serial")
class SimpleTreeFrame extends JFrame {
  private static Logger logger = Logger.getLogger(SimpleTreeFrame.class.getName());
  private static String OSNAME = System.getProperty("os.name");
  private static final int DEFAULT_WIDTH = 900;
  private static final int DEFAULT_HEIGHT = 900;

  private JTabbedPane tpane;
  private JLabel statusBar;
  File curdir = new File(System.getProperty("user.dir", "."));

  public SimpleTreeFrame(String[] args) {
    setTitle("SimpleTree");
    setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);

    JMenuBar menuBar = new JMenuBar();
    setJMenuBar(menuBar);

    JMenu fileMenu = new JMenu("File");
    menuBar.add(fileMenu);

    Action openAction = new FileOpenAction();
    Action clearAction = new FileClearAction();
    Action closeAction = new FileCloseAction();
    JMenuItem openItem = fileMenu.add(openAction);
    JMenuItem clearItem = fileMenu.add(clearAction);
    fileMenu.addSeparator();
    JMenuItem closeItem = fileMenu.add(closeAction);

    if (OSNAME.startsWith("Mac")) {
      openItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, Event.META_MASK));
      clearItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L, Event.META_MASK));
      closeItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_W, Event.META_MASK));
    } else {
      openItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, Event.CTRL_MASK));
      clearItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L, Event.CTRL_MASK));
      closeItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_W, Event.CTRL_MASK));

      Action quitAction = new FileQuitAction();
      JMenuItem quitItem = fileMenu.add(quitAction);
      quitItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, Event.CTRL_MASK));
    }

    tpane = new JTabbedPane();
    tpane.setToolTipText("Open a partDSA visualization file using File -> Open menu");
    add(tpane, BorderLayout.CENTER);

    statusBar = new JLabel(" ");
    add(statusBar, BorderLayout.SOUTH);

    // Open any input files specified on the command line
    for (String arg: args) {
      File file = new File(arg);
      if (file.exists()) {
        try {
          XMLStreamReader parser = getParser(file);
          Map<String,String[]> factors = getFactors(parser);
          List<Node> nodelist = mknode(parser);
          JComponent contents = makeContents(nodelist);
          tpane.addTab(file.getName(), null, contents);
          tpane.setSelectedIndex(tpane.getTabCount() - 1);
        } catch(Exception e) {
          logger.log(Level.WARNING, "error opening/processing data file " +
                     file.getName(), e);
          setStatus("error opening/processing data file");
        }
      } else {
        logger.log(Level.WARNING, "file does not exist: " + file.getName());
      }
    }
  }

  public void setStatus(String status) {
    statusBar.setText(status);
  }

  XMLStreamReader getParser(File fobj) {
    XMLStreamReader parser = null;
    try {
      InputStream in = new FileInputStream(fobj);
      XMLInputFactory factory = XMLInputFactory.newInstance();
      parser = factory.createXMLStreamReader(in);
    } catch (XMLStreamException xse) {
      logger.log(Level.WARNING, "unable to create XML parser", xse);
      setStatus("unable to create XML parser");
    } catch (FileNotFoundException fnfe) {
      logger.log(Level.WARNING, "unable to read data file", fnfe);
      setStatus("unable to read file: " + fobj.getPath());
    }
    return parser;
  }

  Map<String,String[]> getFactors(XMLStreamReader parser) {
    Map<String,String[]> factors = new HashMap<String,String[]>();

    if (parser != null) {
      try {
        String name = "FAKE";
        String varname = "FAKE";
        String type = "UNKNOWN";
        List<String> levels = null;
        boolean EOV = false;

        while (! EOV && parser.hasNext()) {
          int event = parser.next();
          switch(event) {
          case XMLStreamConstants.START_ELEMENT:
            name = parser.getLocalName();

            if (name.equals("variables")) {
              ;
            } else if (name.equals("variable")) {
              varname = parser.getAttributeValue(null, "name");
              type = parser.getAttributeValue(null, "type");
              if (type.equals("factor")) {
                levels = new ArrayList<String>();
              }
            } else if (name.equals("level")) {
              levels.add(parser.getAttributeValue(null, "name"));
            }

            break;
          case XMLStreamConstants.END_ELEMENT:
            name = parser.getLocalName();

            if (name.equals("variables")) {
              // Reached the end of the "variables" section, so quit
              EOV = true;
            } else if (name.equals("variable")) {
              if (type.equals("factor")) {
                factors.put(varname, levels.toArray(new String[0]));
              }
            }

            break;
          default:
            break;
          }
        }
      } catch (XMLStreamException e) {
        logger.log(Level.WARNING, "error parsing XML file", e);
        setStatus("error parsing XML file");
      }
    }

    return factors;
  }

  List<Node> mknode(XMLStreamReader parser) {
    List<Node> nodelist = new ArrayList<Node>();

    if (parser != null) {
      try {
        String name;
        double lower = Double.NEGATIVE_INFINITY;
        double upper = Double.POSITIVE_INFINITY;
        List<Partition> partlist = null;
        List<byte[]> blist = null;
        Icon image = null;
        boolean needData = false;
        Partition partition = null;
        String partname = "UNKNOWN";
        String predictedValue = "UNKNOWN";
        String numobs = "UNKNOWN";
        String section = "UNKNOWN";
        String numsections = "UNKNOWN";
        String maxpartitions = "UNKNOWN";
        final Interval[] intervalArray = new Interval[0];
        List<Interval> interlist = null;
        Interval interval = null;
        String varname = "FAKE";
        String intervaltype = "UNKNOWN";
        boolean EOP = false;

        while (! EOP && parser.hasNext()) {
          int event = parser.next();
          switch(event) {
          case XMLStreamConstants.START_ELEMENT:
            name = parser.getLocalName();

            if (name.equals("partlists")) {
              // A container for partlist elements
            } else if (name.equals("partlist")) {
              // Start collecting Partitions in a list so we can later
              // create a Node from them
              partlist = new ArrayList<Partition>();
            } else if (name.equals("partition")) {
              // Start collecting Intervals in a list so we can later
              // create a Partition from them using the specified name
              interlist = new ArrayList<Interval>();
              partname = parser.getAttributeValue(null, "name");
              numsections = parser.getAttributeValue(null, "numsections");
              section = parser.getAttributeValue(null, "section");
              predictedValue = parser.getAttributeValue(null, "predictedvalue");
              numobs = parser.getAttributeValue(null, "numobs");
              maxpartitions = parser.getAttributeValue(null, "max");
            } else if (name.equals("image")) {
              blist = new ArrayList<byte[]>();
            } else if (name.equals("data")) {
              needData = true;
            } else if (name.equals("interval")) {
              // Get the name of the variable associated with this Interval
              varname = parser.getAttributeValue(null, "varname");

              // Get the type of this Interval: not currently used
              intervaltype = parser.getAttributeValue(null, "type");
              lower = Double.NEGATIVE_INFINITY;
              upper = Double.POSITIVE_INFINITY;
            } else if (name.equals("range")) {
              // Get the upper and lower bounds of this interval
              String tmp = parser.getAttributeValue(null, "lower");
              lower = tmp.equals("-Inf") ? Double.NEGATIVE_INFINITY :
                                           Double.parseDouble(tmp);
              tmp = parser.getAttributeValue(null, "upper");
              upper = tmp.equals("Inf") ? Double.POSITIVE_INFINITY :
                                          Double.parseDouble(tmp);
            }

            break;
          case XMLStreamConstants.END_ELEMENT:
            name = parser.getLocalName();

            if (name.equals("partlists")) {
              EOP = true;
            } else if (name.equals("partlist")) {
              // End of the partlist, so create the Node and
              // add it to our list of Nodes
              Node node = new Node(0, partlist);
              node.splitup(node.getsplits());
              nodelist.add(node);
            } else if (name.equals("partition")) {
              // End of the Partition, so create it using all of the
              // Interval objects that we've created
              partition = new Partition(partname, predictedValue, numobs,
                  numsections, section, maxpartitions, image, interlist.toArray(intervalArray));
              partlist.add(partition);
            } else if (name.equals("image")) {
              // Create an ImageIcon from the list of byte arrays
              logger.log(Level.FINE, "creating ImageIcon");
              byte[] pngdata = concat(blist);
              image = new ImageIcon(pngdata);
              blist = null;
            } else if (name.equals("data")) {
              needData = false;
            } else if (name.equals("interval")) {
              // End of the Interval, so create it using the bounds
              // that we've created
              interval = new NumericInterval(varname, lower, upper);
              interlist.add(interval);
            } else if (name.equals("range")) {
              // End of range, but we won't do anything until the end
              // Interval
            }

            break;
          case XMLStreamConstants.CHARACTERS:
            if (needData) {
              String hexstr = parser.getText();
              logger.log(Level.FINE, "decoding image data: " + hexstr);
              blist.add(Hex.decodeHex(hexstr));
            }
            break;
          default:
            // Only interested in start and end elements
            break;
          }
        }
      } catch (XMLStreamException e) {
        logger.log(Level.WARNING, "error parsing XML file", e);
        setStatus("error parsing XML file");
      }
    }

    return nodelist;
  }

  /**
   * This is a very important method.  It creates a JTree to
   * allow the user to browse through the various nodes.
   * It also creates a NodeTreeComponent that displays the
   * selected node.  When a node is selected (either a top level
   * node, or a child node), it updates the NodeTreeModel to
   * use the selected node, which will be automatically
   * displayed by the NodeTreeComponent, since it is listening
   * for any property changes in the NodeTreeModel.
   */
  private JComponent makeContents(List<Node> nodelist) {
    final NodeTreeModel model = new NodeTreeModel();

    // Create the nodes to put into a tree
    TreeNode root = mktree(nodelist);

    // Create a tree to display our nodes
    final JTree tree = new JTree(root);
    tree.setRootVisible(true);  // this is the default, but I'm waffling
    tree.getSelectionModel()
        .setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    JScrollPane sp1 = new JScrollPane(tree);
    JPanel p1 = new JPanel();
    p1.setLayout(new BorderLayout());
    p1.add(sp1, BorderLayout.CENTER);
    JLabel btitle = new JLabel("Node Browser");
    btitle.setToolTipText("Use this browser to select nodes for display below");
    btitle.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 20));
    btitle.setHorizontalAlignment(SwingConstants.CENTER);
    p1.add(btitle, BorderLayout.NORTH);

    String[] labels = Node.getLabels();
    String tooltip = "Select a node using the node browser above";
    final NodeInfoModel infomodel = new NodeInfoModel("Selected Node", tooltip, labels);
    final NodeInfoComponent selectedNodePanel = new NodeInfoComponent(infomodel);
    JScrollPane sp2 = new JScrollPane(selectedNodePanel);

    JSplitPane split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, p1, sp2);

    NodeTreeComponent nodetree = new NodeTreeComponent(model);
    JScrollPane sp3 = new JScrollPane(nodetree);

    // Register a selection listener that will update the NodeTreeModel
    tree.addTreeSelectionListener(new TreeSelectionListener () {
      public void valueChanged(TreeSelectionEvent e) {
        DefaultMutableTreeNode treenode = (DefaultMutableTreeNode)
            tree.getLastSelectedPathComponent();

        if (treenode == null) {
          logger.log(Level.FINE, "selected path component is null");
          return;
        }

        TreePath treepath = tree.getSelectionPath();
        if (treepath.getPathCount() < 2) {
          logger.log(Level.FINE, "path count is less than two");
          return;
        }

        DefaultMutableTreeNode toptreenode = (DefaultMutableTreeNode)
            treepath.getPathComponent(1);

        if (toptreenode == null) {
          logger.log(Level.FINE, "second node in selected path is null");
          return;
        }

        // The user object can be a string if they selected the top level node
        Object topnode = toptreenode.getUserObject();
        Object selected = treenode.getUserObject();
        if (topnode instanceof Node && selected instanceof Node) {
          Node selectedNode = (Node) selected;

          // Update the NodeTreeComponent
          model.setNode((Node) topnode, selectedNode);

          // Check if the selected node is a leaf
          if (selectedNode.left == null) {
            // Update the selected node panel
            infomodel.update(selectedNode.getImage(), selectedNode.description());
          } else {
            infomodel.update(selectedNode.getImage(), new HashMap<String,String>());
          }
        }
      }
    });

    return new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, split, sp3);
  }

  byte[] concat(List<byte[]> blist) {
    // Compute the total length of all the arrays in the list
    int n = 0;
    for (byte[] b: blist) {
      n += b.length;
    }

    // Create a result array of the appropriate length
    byte[] a = new byte[n];

    // Copy all of the arrays to the result array
    n = 0;
    for (byte[] b: blist) {
      System.arraycopy(b, 0, a, n, b.length);
      n += b.length;
    }

    return a;
  }

  TreeNode mktree(List<Node> nodelist) {
    DefaultMutableTreeNode root = new DefaultMutableTreeNode("Partitions");
    for (int i = 0; i < nodelist.size(); i++) {
      Node node = nodelist.get(i);
      mktreeRecurse(root, node, i + 2);
    }
    return root;
  }

  void mktreeRecurse(DefaultMutableTreeNode parent, Node node, int maxpartitions) {
    DefaultMutableTreeNode t = new MyMutableTreeNode(node, maxpartitions);
    assert parent != null;
    parent.add(t);

    if (node.left != null) {
      mktreeRecurse(t, node.left, maxpartitions);
    }

    if (node.right != null) {
      mktreeRecurse(t, node.right, maxpartitions);
    }
  }

  private class MyMutableTreeNode extends DefaultMutableTreeNode {
    private Node node;
    private int maxpartitions;

    MyMutableTreeNode(Node node, int maxpartitions) {
      super(node);
      this.node = node;
      this.maxpartitions = maxpartitions;
    }

    @Override
    public String toString() {
      if (node.level == 0)
        return String.format("Best of %d partitions", maxpartitions);
      else
        return node.toString();
    }
  }

  private class FileOpenAction extends AbstractAction {
    FileOpenAction() {
      super("Open...");
    }

    public void actionPerformed(ActionEvent event) {
      JFileChooser chooser = new JFileChooser(curdir);
      int result = chooser.showOpenDialog(SimpleTreeFrame.this);

      if (result == JFileChooser.APPROVE_OPTION) {
        curdir = chooser.getCurrentDirectory();
        File file = chooser.getSelectedFile();

        logger.log(Level.FINE, "adding new tab to SimpleTreeFrame from file " +
                   file.getPath());

        try {
          XMLStreamReader parser = getParser(file);
          Map<String,String[]> factors = getFactors(parser);
          List<Node> nodelist = mknode(parser);
          JComponent contents = makeContents(nodelist);
          tpane.addTab(file.getName(), null, contents);
          tpane.setSelectedIndex(tpane.getTabCount() - 1);
        } catch(Exception e) {
          logger.log(Level.WARNING, "error opening/processing data file " +
                     file.getName(), e);
          setStatus("error opening/processing data file");
        }
      }
    }
  }

  private class FileCloseAction extends AbstractAction {
    FileCloseAction() {
      super("Close Tab");
    }

    public void actionPerformed(ActionEvent event) {
      int selected = tpane.getSelectedIndex();
      if (selected != -1) {
        logger.log(Level.FINE, "removing tab from SimpleTreeFrame");
        tpane.remove(selected);
      }
    }
  }

  private class FileClearAction extends AbstractAction {
    FileClearAction() {
      super("Clear Status");
    }

    public void actionPerformed(ActionEvent event) {
      setStatus(" ");
    }
  }

  private class FileQuitAction extends AbstractAction {
    FileQuitAction() {
      super("Quit");
    }

    public void actionPerformed(ActionEvent event) {
      System.exit(0);
    }
  }
}
