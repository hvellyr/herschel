#!/usr/bin/env python

import xml.dom.minidom as minidom
import subprocess
import os
import sys
import comparexml

class TestRunner:

    def __init__(self):
        self.heather_path = "../temp/debug/heather"

    def run_heather_on_test(self, test_file, traces):
        return subprocess.Popen([self.heather_path, "-T", traces, "-P", test_file],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE).communicate()


    def compare_XML_result_with_file(self, test_name, test_str, expected_file):
        try:
            dom1 = None
            dom2 = None

            try:
                dom1 = minidom.parseString(test_str)
            except Exception, e:
                print "FAILED: %s: not a valid XML file: %s" % (test_name, e)
                return

            try:
                dom2 = minidom.parse(expected_file)
            except Exception, e:
                print "FAILED: %s: not a valid XML file: %s" % (test_name, e)
                return

            if not comparexml.compareXML(dom1, dom2):
                print "FAILED: %s: differs from expected data" % (test_name)
            else:
                print "OK: %s: test succeeds" % (test_name)
        except Exception, e:
            print "FAILED: %s: comparing results failed with exception: %s" % (test_name, e)


    def find_expected_xml(self, test_file, passid):
        path, ext = os.path.splitext(test_file)
        expected_file = path + passid + ".xml"

        if os.path.exists(expected_file):
            return expected_file
        return False


    def run_pass_test_impl(self, test_file, traces, passid):
        output, erroutput = self.run_heather_on_test(test_file, traces)

        what_tag = "[%s] %s" % (passid, test_file)
        if erroutput:
            print "FAILED: %s: %s" % (what_tag, erroutput)
            return

        expected_file = self.find_expected_xml(test_file, "_%s" % (passid))
        if expected_file:
            self.compare_XML_result_with_file(what_tag, output, expected_file)
#        else:
#            print "INFO: %s: No expected xml file for pass %s found" % (what_tag, passid)


    def run_pass_test(self, test_file):
        self.run_pass_test_impl(test_file, "pass1", "1")
        self.run_pass_test_impl(test_file, "pass2", "2")


    def run_pass_failed_test(self, test_file):
        print "????: %s: not tested yet" % (test_file)


    def run_all_tests(self, test_dir):
        for f in os.listdir(test_dir):
            if f.endswith(".hea"):
                test_file = os.path.join(test_dir, f)

                if f.startswith("failed-"):
                    self.run_pass_failed_test(test_file)
                else:
                    self.run_pass_test(test_file)


if __name__ == "__main__":
    tr = TestRunner()
    tr.heather_path = sys.argv[1]

    test_resource = sys.argv[2]
    if os.path.isdir(test_resource):
        tr.run_all_tests(test_resource)
    else:
        tr.run_pass1_test(test_resource)

