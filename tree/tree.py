#!/usr/bin/env python

MAYBE = -1
NO = 0
YES = 1
DONTCARE = 2

INF = float("infinity")
NINF = float("-infinity")

# A Partition object is named, and contains zero or more Interval objects
class Partition(object):
    def __init__(self, name, *intervals):
        self.name = name
        self._intervals = {}
        for i in intervals:
            self.add(i)

    # This method adds an Interval object to this Partion.
    # There shouldn't be more than one Interval for the same
    # variable.
    def add(self, interval):
        if self._intervals.has_key(interval.varname):
            raise RuntimeError("duplicate interval added to Partition")
        self._intervals[interval.varname] = interval

    # Compute whether the specified Split is satisfied by this Parition
    # by calling the satisfied method on the appropriate Interval object.
    # If we don't have an appropriate Interval object, return DONTCARE.
    def satisfied(self, split):
        if self._intervals.has_key(split.varname):
            return self._intervals[split.varname].satisfied(split)
        else:
            return DONTCARE

    # Return a set of Split objects for all of the Intervals in this Partition
    def getsplits(self):
        s = set()
        for i in self._intervals.values():
            s.update(i.getsplits())
        return s

    def __str__(self):
        keys = self._intervals.keys()
        keys.sort()
        return self.name + ": " + " and ".join([str(self._intervals[k]) for k in keys])

# A Split object is associated with a named variable and a "split" value for that variable
class Split(object):
    def __init__(self, varname, value):
        self.varname = varname
        self.value = value

    def __eq__(self, other):
        return other.varname == self.varname and other.value == self.value

    def __hash__(self):
        return hash(self.value)

    def __str__(self):
        return "%s > %f" % (self.varname, self.value)

    def __repr__(self):
        return str(self)

# A Interval object represents a range of values of a named variable
class Interval(object):
    def __init__(self, varname, lower, upper):
        if lower >= upper:
            raise RuntimeError("lower must be less than upper")
        self.varname = varname
        self.lower = lower
        self.upper = upper

    # This methods determines whether the specified Split object
    # is satisfied by this Interval
    def satisfied(self, split):
        if split.varname != self.varname:
            raise RuntimeError("invalid comparison")
        if split.value >= self.upper:
            return NO
        elif split.value <= self.lower:
            return YES
        else:
            return MAYBE

    # This method returns a set of 0, 1, or 2 Split objects for this Interval
    def getsplits(self):
        s = set()
        if self.upper < INF:
            s.add(Split(self.varname, self.upper))
        if self.lower > INF:
            s.add(Split(self.varname, self.lower))
        return s

    def __str__(self):
        return "(%f < %s <= %f)" % (self.lower, self.varname, self.upper)

# A Node object has a "level" and a non-empty list of Partition objects
class Node(object):
    def __init__(self, level, partlist):
        if len(partlist) == 0:
            raise RuntimeError("nodes need at least one partition")
        self._level = level
        self._split = None
        self._left = None
        self._right = None
        self._partlist = partlist

    # Return a set of Split objects for all of the Intervals in all of the
    # Partitions in this Node
    def getsplits(self):
        s = set()
        for p in self._partlist:
            s.update(p.getsplits())
        return s

    # This method is used to turn a flat Node into a tree of Nodes.
    # It does that by setting the "_split", "_left", and "_right"
    # attributes.  The "_split" attribute is set to a Split object,
    # and the "_left" and "_right" attributes are set to Node objects.
    def splitup(self, splits):
        # check if we're done
        if len(self._partlist) == 1:
            return

        # sanity check
        if len(splits) == 0:
           raise RuntimeError("splitup called with no splits")

        # find the best split, and make a copy of the splits minus the best
        best = self.bestsplit(splits)
        subsplits = splits.copy()
        subsplits.remove(best)

        # divide the partitions in this node based on the best
        maybe = [p for p in self._partlist if p.satisfied(best) == MAYBE]
        if len(maybe) > 0:
            print "warning: tree resulted in %d maybe's" % len(maybe)
        no = [p for p in self._partlist if p.satisfied(best) == NO]
        yes = [p for p in self._partlist if p.satisfied(best) == YES]
        dont = [p for p in self._partlist if p.satisfied(best) == DONTCARE]
        if len(dont) > 0:
            print "warning: tree resulted in %d don't cares" % len(dont)

        # create right and left child nodes and split them recursively
        right = Node(self._level + 1, yes + dont + maybe)
        right.splitup(subsplits)
        left = Node(self._level + 1, no + maybe)
        left.splitup(subsplits)

        # update our own state
        self._split = best
        self._left = left
        self._right = right

    # Return the Split from the specified list of Split objects with
    # the best score
    def bestsplit(self, splits):
        scores = [(self.score(i), i) for i in splits]
        best, split = max(scores)
        if best < 0:
            raise RuntimeError("no workable split")
        return split

    # Compute a "score" for the specified Split object
    def score(self, split):
        # Compute lists of Partitions from this Node based on the value
        # returned by the "satisfied" method
        maybe = [p for p in self._partlist if p.satisfied(split) == MAYBE]
        no = [p for p in self._partlist if p.satisfied(split) == NO]
        yes = [p for p in self._partlist if p.satisfied(split) == YES]
        dont = [p for p in self._partlist if p.satisfied(split) == DONTCARE]

        # The total number of partitions in this Node
        npart = len(self._partlist)

        if maybe or (not no or not yes):
            return -1
        else:
            return ((npart - len(dont)) * npart) - abs(len(yes) - len(no))

    def __str__(self):
        prefix = self._level * "   "
        if self._split:
            split = prefix + "if %s:" % str(self._split)
            right = str(self._right)
            els = prefix + "else:"
            left = str(self._left)
            s = "\n".join([split, right, els, left])
        else:
            s = "\n".join([(prefix + str(p)) for p in self._partlist])

        return s

if __name__ == "__main__":
    # two partitions
    a = Partition("A", Interval("lstat", NINF, 7.56))
    b = Partition("B", Interval("lstat", 7.56, INF))
    node = Node(0, [a, b])
    print "Original node with two partitions:"
    print node
    node.splitup(node.getsplits())
    print "\nAfter splitup:"
    print node

    # three partitions
    a = Partition("A", Interval("rm", NINF, 7.007),
                       Interval("lstat", NINF, 7.56))
    b = Partition("B", Interval("lstat", 7.56, INF))
    c = Partition("C", Interval("rm", 7.007, INF),
                       Interval("lstat", NINF, 7.56))
    node = Node(0, [a, b, c])
    print "\nOriginal node with three partitions:"
    print node
    node.splitup(node.getsplits())
    print "\nAfter splitup:"
    print node

    # four partitions
    a = Partition("A", Interval("rm", NINF, 7.007),
                       Interval("lstat", NINF, 7.56))
    b = Partition("B", Interval("lstat", 7.56, 16.21))
    c = Partition("C", Interval("rm", 7.007, INF),
                       Interval("lstat", NINF, 7.56))
    d = Partition("D", Interval("lstat", 16.21, INF))
    node = Node(0, [a, b, c, d])
    print "\nOriginal node with four partitions:"
    print node
    node.splitup(node.getsplits())
    print "\nAfter splitup:"
    print node

    # ten partitions
    a = Partition("A",
                  Interval("rm", NINF, 7.007),
                  Interval("lstat", NINF, 4.56))
    b = Partition("B",
                  Interval("age", NINF, 84.5),
                  Interval("lstat", 7.56, 11.64))
    c = Partition("C",
                  Interval("rm", 7.007, INF),
                  Interval("black", NINF, 392.78),
                  Interval("lstat", NINF, 5.12))
    d = Partition("D",
                  Interval("nox", NINF, 0.581),
                  Interval("lstat", 16.21, INF))
    e = Partition("E",
                  Interval("rm", 7.007, INF),
                  Interval("lstat", 5.12, 7.56))
    f = Partition("F",
                  Interval("nox", 0.581, INF),
                  Interval("lstat", 16.21, INF))
    g = Partition("G",
                  Interval("rm", NINF, 7.007),
                  Interval("lstat", 4.56, 7.56))
    h = Partition("H",
                  Interval("lstat", 11.64, 16.21))
    i = Partition("I",
                  Interval("age", 84.5, INF),
                  Interval("lstat", 7.56, 11.64))
    j = Partition("J",
                  Interval("rm", 7.007, INF),
                  Interval("black", 392.78, INF),
                  Interval("lstat", NINF, 5.12))

    node = Node(0, [a, b, c, d, e, f, g, h, i, j])
    print "\nOriginal node with ten partitions:"
    print node
    node.splitup(node.getsplits())
    print "\nAfter splitup:"
    print node
