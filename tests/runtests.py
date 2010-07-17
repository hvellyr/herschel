#!/usr/bin/env python2.5

from optparse import OptionParser
import xml.dom.minidom as minidom
import subprocess
import os
import sys
import comparexml
import re


class TestRunner:

    def __init__(self):
        self.heather_path = "../temp/debug/heather"
        self.verbose = False
        self.test_succeeded = 0
        self.test_run = 0

        self.SYNTAX_PASS1_OPT = ["-T", "pass1", "-P", "--dont-import", "--parse-1" ]
        self.SYNTAX_PASS2_OPT = ["-T", "pass2", "-P", "--dont-import" ]
        self.IMPORT_PASS1_OPT = ["-T", "pass1", "-P", "--parse-1" ]
        self.IMPORT_PASS2_OPT = ["-T", "pass2", "-P" ]


    def run_heather_on_test(self, test_file, options):
        cmd = [self.heather_path]
        cmd.extend(options)
        cmd.append(test_file)
        return subprocess.Popen(cmd,
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
                if self.verbose:
                    print "  OUTPUT: %s" % (test_str)
                return False

            try:
                dom2 = minidom.parse(expected_file)
            except Exception, e:
                print "FAILED: %s: not a valid XML file: %s" % (test_name, e)
                return False

            if not comparexml.compareXML(dom1, dom2):
                print "FAILED: %s: differs from expected data" % (test_name)
                return False
            else:
                return True
        except Exception, e:
            print "FAILED: %s: comparing results failed with exception: %s" % (test_name, e)
            return False


    def find_expected_xml(self, test_file, passid):
        path, ext = os.path.splitext(test_file)
        expected_file = path + passid + ".xml"

        if os.path.exists(expected_file):
            return expected_file
        return False


    #----------------------------------------------------------------------------

    def run_pass_test_impl(self, test_file, options, passid,
                           errtest_func=None):
        test_count = 0
        if self.verbose:
            print "Run test '%s'" % (test_file)
        output, erroutput = self.run_heather_on_test(test_file, options)
        what_tag = "[%s] %s" % (passid, test_file)
        if erroutput:
            if errtest_func == None:
                print "FAILED: %s: %s" % (what_tag, erroutput)
                return
            test_count += 1
            if not errtest_func(test_file, what_tag, passid, erroutput):
                self.test_run += 1
                return


        expected_file = self.find_expected_xml(test_file, "_%s" % (passid))
        if expected_file:
            test_count += 1
            if not self.compare_XML_result_with_file(what_tag, output, expected_file):
                self.test_run += 1
                return
        else:
            if self.verbose:
                print "INFO: %s: No expected xml file found" % (what_tag)

        print "OK: %s: test succeeds" % (what_tag)
        self.test_succeeded += test_count
        self.test_run += test_count


    def run_pass_test(self, test_file, domain):
        if domain == "syntax":
            opts1 = self.SYNTAX_PASS1_OPT
            opts2 = self.SYNTAX_PASS2_OPT

        elif domain == "import":
            opts1 = self.IMPORT_PASS1_OPT
            opts2 = self.IMPORT_PASS2_OPT

        else:
            assert(0)

        self.run_pass_test_impl(test_file, opts1, "1")
        self.run_pass_test_impl(test_file, opts2, "2")



    #----------------------------------------------------------------------------

    def run_pass_failed_test_impl(self, test_file):
        pass


    def load_expected_syntax_errors_desc(self, test_file, passid):
        path, ext = os.path.splitext(test_file)
        expected_file = path + "_" + passid + ".expsynerr"

        if os.path.isfile(expected_file):
            try:
                fd = open(expected_file, "rb")
                expected_lines = fd.read().split('\n')
            finally:
                fd.close()
            return expected_lines
        return []


    def split_experr_line(self, errline):
        values = errline.split(' ; ')
        line_no = ''
        level = ''
        error_code = ''

        if values:
            if len(values) > 0:
                line_no = values[0].strip()
                if len(values) > 1:
                    level = values[1].strip()
                    if len(values) > 2:
                        error_code = values[2].strip()
        return [line_no, level, error_code]


    def check_for_errors(self, test_file, test_name, passid, erroutput):
        eo = erroutput.strip()
        if eo == None or len(eo) == 0:
            print "FAILED: %s: expected errors" % (test_name)
            return False

        expected_errors = self.load_expected_syntax_errors_desc(test_file, passid)

        if len(expected_errors) == 0:
            if self.verbose:
                print "INFO: %s: No expected syntax errors file found" % (what_tag)

        retval = True
        for experr in expected_errors:
            line_no, level, error_code = self.split_experr_line(experr)

            if len(line_no) > 0:
                if len(error_code) > 0:
                    pattern = '%s:%s: %s: \(%s\)' % (test_file, line_no, level, error_code)
                else:
                    pattern = '%s:%s: %s:' % (test_file, line_no, level)
                # print "  PATTERN: ", pattern
                error_re = re.compile(pattern)
                m = error_re.search(eo)
                if m is None:
                    print "FAILED: %s: expected %s " \
                          "message at line %s not found" % (test_name, level, line_no)
                    # print "  EXPECTED: ", expected_errors
                    # print "  PATTERN: ", pattern
                    retval = False
        return retval


    def run_pass_failed_test(self, test_file, domain):
        if domain == "syntax":
            opts1 = self.SYNTAX_PASS1_OPT
            opts2 = self.SYNTAX_PASS2_OPT

        elif domain == "import":
            opts1 = self.IMPORT_PASS1_OPT
            opts2 = self.IMPORT_PASS2_OPT

        else:
            assert(0)

        self.run_pass_test_impl(test_file, opts1, "1", self.check_for_errors)
        self.run_pass_test_impl(test_file, opts2, "2", self.check_for_errors)


    #----------------------------------------------------------------------------

    def run_test(self, test_dir, src_file, domain):
        test_file = os.path.join(test_dir, src_file)

        if src_file.startswith("failed-"):
            self.run_pass_failed_test(test_file, domain)
        elif src_file.startswith("ignore-"):
            pass
        else:
            self.run_pass_test(test_file, domain)


    def run_all_tests(self, test_dir, domain):
        for f in os.listdir(test_dir):
            if f.endswith(".hea"):
                self.run_test(test_dir, f, domain)

        if self.test_succeeded <> self.test_run:
            print "SUMMARY: %d tests of %d failed" % (self.test_run - self.test_succeeded,
                                                      self.test_run)
        else:
            print "SUMMARY: %d tests succeeded" % (self.test_succeeded)


#----------------------------------------------------------------------------

def main():
    parser = OptionParser()
    parser.add_option("-e", "--executable", dest="executable",
                      help="sets the path to the compiler to use", metavar="FILE")
    parser.add_option("-V", "--verbose", action="store_true",
                      dest="verbose", default=False,
                      help="be more verbose")
    parser.add_option("-D", "--domain",
                      dest="domain", default="syntax",
                      help="select the domain of tests to run")

    (options, args) = parser.parse_args()

    tr = TestRunner()

    if options.executable is not None:
        tr.heather_path = options.executable

    if options.verbose:
        tr.verbose = options.verbose

    for arg in args:
        if os.path.isdir(arg):
            tr.run_all_tests(arg, options.domain)
        else:
            d, f = os.path.split(arg)
            tr.run_test(d, f, options.domain)


if __name__ == "__main__":
    main()
