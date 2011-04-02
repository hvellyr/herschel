#!/usr/bin/env python

import xml.dom.minidom as minidom
import getopt
import sys

def compareTextNode(a, b):
    return a.strip() == b.strip()


def isEqualElement(a, b):
    if a.tagName != b.tagName:
        return False
    if sorted(a.attributes.items()) != sorted(b.attributes.items()):
        return False
    if len(a.childNodes) != len(b.childNodes):
        return False
    for ac, bc in zip(a.childNodes, b.childNodes):
        if ac.nodeType != bc.nodeType:
            return False
        if ac.nodeType == ac.TEXT_NODE and not compareTextNode(ac.data, bc.data):
            return False
        if ac.nodeType == ac.ELEMENT_NODE and not isEqualElement(ac, bc):
            return False
    return True


def compareXMLfiles(file1, file2):
    da, db = minidom.parse(file1), minidom.parse(file2)
    return isEqualElement(da.documentElement, db.documentElement)


def compareXML(dom1, dom2):
    return isEqualElement(dom1.documentElement, dom2.documentElement)


def usage():
    print "Usage: comparexml.py file1 file2"

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hv", ["help"])
    except getopt.GetoptError, err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)

    verbose = False
    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o in ("-h", "--help"):
            usage()
            sys.exit()
        else:
            assert False, "unhandled option"

    if len(args) < 2:
        usage()
        sys.exit(2)

    try:
        dom1 = None
        dom2 = None

        try:
            dom1 = minidom.parse(args[0])
        except Exception, e:
            print "File '%s' is not a valid XML file: %s" % (args[0], e)
            return

        try:
            dom2 = minidom.parse(args[1])
        except Exception, e:
            print "File '%s' is not a valid XML file: %s" % (args[1], e)
            return

        if not compareXML(dom1, dom2):
            print "Files '%s' with '%s' differ" % (args[0], args[1])
        else:
            print "Files '%s' with '%s' are equal" % (args[0], args[1])
    except Exception, e:
        print "Comparing '%s' with '%s' failed: " % (args[0], args[1], e)


if __name__ == "__main__":
    main()
