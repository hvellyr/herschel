#!/usr/bin/env python2.5

from optparse import OptionParser
import xml.dom.minidom as minidom
import subprocess
import os
import sys
import comparexml
import re

OPTIONS = { 'syntax':    { '1': ['-T', 'pass1',     '-P', '--dont-import', '--parse-1' ],
                           '2': ['-T', 'pass2',     '-P', '--dont-import', '--parse-2' ],
                           '3': ['-T', 'transform', '-P', '--dont-import', '--parse-3' ],
                           '4': ['-T', 'annotate',  '-P', '--dont-import' ] },
            'import':    { '1': ['-T', 'pass1',     '-P', '--parse-1' ],
                           '2': ['-T', 'pass2',     '-P', '--parse-2' ],
                           '3': ['-T', 'transform', '-P', '--parse-3' ],
                           '4': ['-T', 'annotate',  '-P' ] },
            'transform': { '3': ['-T', 'transform', '-P', '--parse-3' ] },
            'annotate':  { '3': ['-T', 'transform', '-P', '--parse-3' ],
                           '4': ['-T', 'annotate',  '-P' ], }
            }


class TestRunner:

    def __init__(self):
        self.heather_path = "../temp/debug/heather"
        self.verbose = False
        self.test_succeeded = 0
        self.test_run = 0
        self.input_dir = False
        self.last_passid = -1
        self.failures = []


    def report_failure(self, msg):
        full_msg = "FAILED: %s" % msg
        if self.verbose:
            print full_msg
        else:
            if self.last_passid >= 0:
                sys.stdout.write('\b.')
            sys.stdout.write("E")
            self.failures.append(full_msg)
            self.last_passid = -1


    def report_info(self, msg):
        full_msg = "INFO: %s" % msg
        if self.verbose:
            print full_msg
        else:
            if self.last_passid >= 0:
                sys.stdout.write('\b.')
            sys.stdout.write("i")
            self.failures.append(full_msg)
            self.last_passid = -1


    def report_success(self, passid, msg):
        if self.verbose:
            print "OK: %s" % msg
        else:
            np = int(passid)
            if self.last_passid >= 0:
                if np <= self.last_passid:
                    sys.stdout.write("\b")
                else:
                    sys.stdout.write("\b.")
            sys.stdout.write("%d" % np)
            sys.stdout.flush()
            #print "%d" % passid,
            self.last_passid = np


    def run_heather_on_test(self, test_file, options):
        cmd = [self.heather_path]
        cmd.extend(options)
        if self.input_dir:
            cmd.append('-I')
            cmd.append(self.input_dir)
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
                self.report_failure("%s: not a valid XML file: %s" % (test_name, e))
                if self.verbose:
                    self.failures.append("  OUTPUT: %s" % test_str)
                return False

            try:
                dom2 = minidom.parse(expected_file)
            except Exception, e:
                self.report_failure("%s: not a valid XML file: %s" % (test_name, e))
                return False

            if not comparexml.compareXML(dom1, dom2):
                self.report_failure("%s: differs from expected data" % test_name)
                return False
            else:
                return True
        except Exception, e:
            self.report_failure("%s: comparing results failed with exception: %s" % (test_name, e))
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
        expected_file = self.find_expected_xml(test_file, "_%s" % (passid))
        if not expected_file:
            return

        test_count = 0

        output, erroutput = self.run_heather_on_test(test_file, options)
        what_tag = "[%s] %s" % (passid, test_file)
        if erroutput:
            if errtest_func == None:
                self.report_failure("%s: %s" % (what_tag, erroutput))
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
            self.report_info("%s: No expected xml file found" % what_tag)

        self.report_success(passid, "%s: test succeeds" % what_tag)

        self.test_succeeded += test_count
        self.test_run += test_count


    def run_pass_test(self, test_file, domain):
        for level in [ '1', '2', '3', '4' ]:
            if level in OPTIONS[domain]:
                self.run_pass_test_impl(test_file, OPTIONS[domain][level], level)



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
            self.report_failure("%s: expected errors" % test_name)
            return False

        expected_errors = self.load_expected_syntax_errors_desc(test_file, passid)

        if len(expected_errors) == 0:
            self.report_info("%s: No expected syntax errors file found" % test_file)

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
                    self.report_failure("%s: expected %s "
                                        "message at line %s not found" % (test_name, level, line_no))
                    # print "  EXPECTED: ", expected_errors
                    # print "  PATTERN: ", pattern
                    retval = False
        return retval


    def run_pass_failed_test(self, test_file, domain):
        for level in [ '1', '2', '3' ]:
            if level in OPTIONS[domain]:
                self.run_pass_test_impl(test_file, OPTIONS[domain][level], level,
                                        self.check_for_errors)


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

        if not self.verbose:
            sys.stdout.write("\b.\n")

            if len(self.failures) > 0:
                print
                print "FAILURES:"
                for f in self.failures:
                    print f

            print

        if self.test_succeeded <> self.test_run:
            print "SUMMARY: %d tests of %d failed in %s" % (self.test_run - self.test_succeeded,
                                                            self.test_run,
                                                            domain)
        else:
            print "SUMMARY: %d tests succeeded in %s" % (self.test_succeeded, domain)


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
    parser.add_option("-I", "--input",
                      dest="input", default=".",
                      help="give the input directory to the compiler")

    (options, args) = parser.parse_args()

    tr = TestRunner()

    if options.executable is not None:
        tr.heather_path = options.executable

    if options.verbose:
        tr.verbose = options.verbose

    if options.input:
        tr.input_dir = options.input

    for arg in args:
        if os.path.isdir(arg):
            tr.run_all_tests(arg, options.domain)
        else:
            d, f = os.path.split(arg)
            tr.run_test(d, f, options.domain)


if __name__ == "__main__":
    main()
