package dsa;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import javax.swing.Icon;

// A Partition object is named, and contains zero or more Interval objects
class Partition {
  String name;
  String predictedValue;
  String numobs;
  String numsections;
  String section;
  String maxpartitions;
  Icon image;
  Map<String,Interval> intervals;

  Partition(String name, String predictedValue, String numobs, String numsections,
            String section, String maxpartitions, Icon image, Interval... intervals) {
    this.name = name;
    this.predictedValue = predictedValue;
    this.numobs = numobs;
    this.numsections = numsections;
    this.section = section;
    this.maxpartitions = maxpartitions;
    this.image = image;
    this.intervals = new HashMap<String,Interval>();
    for (Interval i: intervals)
      add(i);
  }

  // This method adds an Interval object to this Partion.
  // There shouldn't be more than one Interval for the same
  // variable.
  void add(Interval interval) {
    assert ! intervals.containsKey(interval.getVarName());
    intervals.put(interval.getVarName(), interval);
  }

  // Compute whether the specified Split is satisfied by this Partition
  // by calling the satisfied method on the appropriate Interval object.
  // If we don't have an appropriate Interval object, return DONTCARE.
  int satisfied(Split split) {
    if (intervals.containsKey(split.varname))
      return intervals.get(split.varname).satisfied(split);
    else
      return Node.DONTCARE;
  }

  // Return a set of Split objects for all of the Intervals in this Partition
  Set<Split> getsplits() {
    Set<Split> s = new HashSet<Split>();
    for (Interval i: intervals.values())
      s.addAll(i.getsplits());
    return s;
  }

  String getPredictedValue() {
    return predictedValue;
  }

  @Override public String toString() {
    return name;
  }

  public String description() {
    StringBuffer sb = new StringBuffer();
    sb.append(name);
    sb.append(": ");
    TreeSet<String> keys = new TreeSet<String>();
    keys.addAll(intervals.keySet());
    for (String k: keys) {
      sb.append(intervals.get(k).toString());
      if (! k.equals(keys.last()))
        sb.append(" and ");
    }
    return sb.toString();
  }
}

// A Split object is associated with a named variable and a "split" value for that variable
class Split {
  String varname;
  Double value;

  Split(String varname, double value) {
    this.varname = varname;
    this.value = value;
  }

  @Override public boolean equals(Object o) {
    if (! (o instanceof Split))
      return false;
    Split s = (Split) o;
    return varname.equals(s.varname) && value == s.value;
  }

  @Override public int hashCode() {
    int result = 17;
    result = 31 * result + varname.hashCode();
    long x = Double.doubleToLongBits(value);
    result = 31 * result + (int) (x ^ x >>> 32);
    return result;
  }

  @Override public String toString() {
    return String.format("%s > %.2f", varname, value);
  }
}

interface Interval {
  String getVarName();
  int satisfied(Split split);
  Set<Split> getsplits();
}

// A NumericInterval object represents a range of values of a named variable
class NumericInterval implements Interval {
  String varname;
  double lower;
  double upper;

  NumericInterval(String varname, double lower, double upper) {
    assert upper >= lower;
    this.varname = varname;
    this.lower = lower;
    this.upper = upper;
  }

  public String getVarName() {
    return varname;
  }

  // This methods determines whether the specified Split object
  // is satisfied by this Interval
  public int satisfied(Split split) {
    assert split.varname.equals(varname);
    if (split.value >= upper)
      return Node.NO;
    else if (split.value <= lower)
      return Node.YES;
    else
      return Node.MAYBE;
  }

  // This method returns a set of 0, 1, or 2 Split objects for this Interval
  public Set<Split> getsplits() {
    Set<Split> s = new HashSet<Split>();
    if (! Double.isInfinite(upper))
      s.add(new Split(varname, upper));
    if (! Double.isInfinite(lower))
      s.add(new Split(varname, lower));
    return s;
  }

  @Override public String toString() {
    if (lower > Double.NEGATIVE_INFINITY) {
      if (upper < Double.POSITIVE_INFINITY) {
        // Two non-infinity bounds
        return String.format("%f < %s <= %f", lower, varname, upper);
      } else {
        // Only a lower bound
        return String.format("%f < %s", lower, varname);
      }
    } else {
      if (upper < Double.POSITIVE_INFINITY) {
        // Only an upper bound
        return String.format("%s <= %f", varname, upper);
      } else {
        // No bounds should never happen, but if it could,
        // I would do this:
        return String.format("%f < %s <= %f", lower, varname, upper);
      }
    }
  }
}

// A Node object has a "level" and a non-empty list of Partition objects
public class Node {
  final static int MAYBE = -1;
  final static int NO = 0;
  final static int YES = 1;
  final static int DONTCARE = 2;

  int level;
  Split split;
  Node right;
  Node left;
  List<Partition> partlist;

  Node(int level, List<Partition> partlist) {
    assert partlist.size() > 0;
    this.level = level;
    this.split = null;
    this.right = null;
    this.left = null;
    this.partlist = partlist;
  }

  // Not currently used: replaced by toString
  String pnames() {
    StringBuffer sb = new StringBuffer();
    for (Partition p: partlist) {
      sb.append(", ");
      sb.append(p.name);
    }
    sb.delete(0, 2);
    return sb.toString();
  }

  // Return a set of Split objects for all of the Intervals in all of the
  // Partitions in this Node
  Set<Split> getsplits() {
    Set<Split> s = new HashSet<Split>();
    for (Partition p: partlist)
      s.addAll(p.getsplits());
    return s;
  }

  // This method is used to turn a flat Node into a tree of Nodes.
  // It does that by setting the "_split", "_right", and "_left"
  // attributes.  The "_split" attribute is set to a Split object,
  // and the "_right" and "_left" attributes are set to Node objects.
  void splitup(Set<Split> splits) {
    // check if we're done
    if (partlist.size() == 1)
      return;

    // sanity check
    assert splits.size() > 0;

    // find the best split, and make a copy of the splits minus the best
    Split best = bestsplit(splits);
    Set<Split> subsplits = new HashSet<Split>();
    subsplits.addAll(splits);
    subsplits.remove(best);

    // divide the partitions in this node based on the best
    List<Partition> maybe = new ArrayList<Partition>();
    List<Partition> no = new ArrayList<Partition>();
    List<Partition> yes = new ArrayList<Partition>();
    List<Partition> dont = new ArrayList<Partition>();

    for (Partition p: partlist) {
      switch (p.satisfied(best)) {
      case Node.MAYBE:
        maybe.add(p);
        break;
      case Node.NO:
        no.add(p);
        break;
      case Node.YES:
        yes.add(p);
        break;
      case Node.DONTCARE:
        dont.add(p);
        break;
      default:
        throw new RuntimeException("satisfied method returned unexpected value");
      }
    }

    if (maybe.size() > 0)
      System.err.printf("warning: tree resulted in %d maybe's", maybe.size());

    if (dont.size() > 0)
      System.err.printf("warning: tree resulted in %d don't cares", dont.size());

    List<Partition> leftPartitions = new ArrayList<Partition>();
    leftPartitions.addAll(yes);
    leftPartitions.addAll(dont);
    leftPartitions.addAll(maybe);

    List<Partition> rightPartitions = new ArrayList<Partition>();
    rightPartitions.addAll(no);
    rightPartitions.addAll(maybe);

    // create left and right child nodes and split them recursively
    left = new Node(level + 1, leftPartitions);
    left.splitup(subsplits);
    right = new Node(level + 1, rightPartitions);
    right.splitup(subsplits);

    // finish updating our state
    split = best;
  }

  // Return the Split from the specified list of Split objects with
  // the best score
  Split bestsplit(Set<Split> splits) {
    int best = Integer.MIN_VALUE;
    Split split = null;

    for (Split i: splits) {
      int s = score(i);
      if (s > best) {
        best = s;
        split = i;
      }
    }
    assert split != null;

    if (best < 0)
      throw new RuntimeException("no workable split");

    return split;
  }

  // Compute a "score" for the specified Split object
  int score(Split split) {
    // Compute lists of Partitions from this Node based on the value
    // returned by the "satisfied" method
    List<Partition> maybe = new ArrayList<Partition>();
    List<Partition> no = new ArrayList<Partition>();
    List<Partition> yes = new ArrayList<Partition>();
    List<Partition> dont = new ArrayList<Partition>();

    for (Partition p: partlist) {
      switch (p.satisfied(split)) {
      case Node.MAYBE:
        maybe.add(p);
        break;
      case Node.NO:
        no.add(p);
        break;
      case Node.YES:
        yes.add(p);
        break;
      case Node.DONTCARE:
        dont.add(p);
        break;
      default:
        throw new RuntimeException("satisfied method returned unexpected value");
      }
    }

    // The total number of partitions in this Node
    int npart = partlist.size();

    if (maybe.size() > 0 || (no.size() == 0 || yes.size() == 0))
      return -1;
    else
      return ((npart - dont.size()) * npart) - Math.abs(yes.size() - no.size());
  }

  // Keep this in sync with the following "description" method
  static String[] getLabels() {
    // Not displaying "section"
    return new String[] {
      "Partition name",
      "Predicted value",
      "Observations",
      "Nodes in partition",
      "Cut off growth"
    };
  }

  // Keep this in sync with the previous "getLabels" method
  Map<String,String> description() {
    Map<String,String> m = new HashMap<String,String>();
    Partition p = partlist.get(0);
    m.put("Partition name", p.name);
    m.put("Predicted value", p.predictedValue);
    m.put("Observations", p.numobs);
    m.put("Nodes in partition", p.numsections);
    m.put("Cut off growth", p.maxpartitions);
    return m;
  }

  // This is a bit of a hack which is used to support the display of
  // all nodes that are part of the same partition.  See the display
  // method of the TreeBox inner class in NodeTreeComponent.java.
  String pname() {
    if (partlist.size() == 1) {
      return partlist.get(0).name;
    } else {
      return null;
    }
  }

  Icon getImage() {
    if (partlist.size() == 1) {
      return partlist.get(0).image;
    } else {
      return null;
    }
  }

  @Override public String toString() {
    String s;

    if (split != null) {
      s = split.toString();
    } else {
      if (partlist.size() > 0) {
        String partname = partlist.get(0).name;
        String value = partlist.get(0).getPredictedValue();

        // Display only two decimal places if predicted value is floating point
        try {
          float val = Float.parseFloat(value);
          value = String.format("%.2f", val);
        } catch (NumberFormatException nfe) {
          // This will happen if the outcome class is categorical
        }
        s = String.format("%s: %s", partname, value);
      } else {
        // This probably indicates an error somewhere in the code
        s = "Error: node with no partitions";
      }
    }

    return s;
  }
}
