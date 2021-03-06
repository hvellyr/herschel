#!/usr/bin/env python2.5

#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

from __future__ import with_statement

from optparse import OptionParser
import xml.dom.minidom as minidom
import subprocess
import os
import sys
import comparexml
import re
import tempfile
import shutil
import platform
import threading
import signal

OPTIONS = { 'syntax':    { '1': ['-T', 'pass1',     '-P', '--dont-import', '--parse-1' ],
                           '2': ['-T', 'pass2',     '-P', '--dont-import', '--parse-2' ],
                           '3': ['-T', 'transform', '-P', '--dont-import', '--parse-3' ],
                           '4': ['-T', 'annotate',  '-P', '--dont-import' ] },
            'import':    { '1': ['-T', 'pass1',     '-P', '--parse-1' ],
                           '2': ['-T', 'pass2',     '-P', '--parse-2' ],
                           '3': ['-T', 'transform', '-P', '--parse-3' ],
                           '4': ['-T', 'annotate',  '-P', '--parse-4' ],
                           '5': ['-T', 'typify',    '-P'              ] },
            'transform': { '3': ['-T', 'transform', '-P', '--parse-3' ] },
            'annotate':  { '3': ['-T', 'transform', '-P', '--parse-3' ],
                           '4': ['-T', 'annotate',  '-P', '--parse-4' ],
                           '5': ['-T', 'typify',    '-P'              ] }
            }

PHASES = ['1', '2', '3', '4', '5']

EXECUTABLES = { 'syntax': 'hrc',
                'import': 'hrc',
                'transform': 'hrc',
                'annotate': 'hrc',
                'compile': 'herschel' }


#----------------------------------------------------------------------------

class TimeoutException(Exception):
    pass


class DirSaver:
    def __init__(self, new_dir=None):
        self.new_dir = new_dir

    def __enter__(self):
        self.old_dir = os.getcwd()
        if self.new_dir:
            os.chdir(self.new_dir)

    def __exit__(self, type, value, tb):
        os.chdir(self.old_dir)


class TmpDirCtx:
    def __init__(self, prefix, delete_on_exit=True, base_dir=None):
        self.prefix = prefix
        self.delete_on_exit = delete_on_exit
        self.base_dir = base_dir

    def __enter__(self):
        self.old_dir = os.getcwd()

        if self.base_dir is not None:
            self.tmp_dir = tempfile.mkdtemp(prefix=self.prefix, dir=self.base_dir)
        else:
            self.tmp_dir = tempfile.mkdtemp(prefix=self.prefix)

        return self.tmp_dir

    def __exit__(self, type, value, tb):
        if self.delete_on_exit:
            shutil.rmtree(self.tmp_dir)


#----------------------------------------------------------------------------

def kill_process(p, timed_out_mutable, process_name):
    status = p.poll()
    if status is None:
        timed_out_mutable[0] = True

        if platform.system() in ("Darwin", "Linux"):
            os.killpg(p.pid, signal.SIGTERM)
#        elif platform.system() == "Windows":
#            call(['taskkill', '/PID', str(p.pid), '/T', '/F'], raise_on_error=False)


#----------------------------------------------------------------------------

class TestRunner:

    def __init__(self):
        self.executable_path = os.path.abspath("../temp/debug/")
        self.current_executable = ''
        self.verbose = False
        self.test_succeeded = 0
        self.test_run = 0
        self.total_test_succeeded = 0
        self.total_test_run = 0
        self.input_dir = False
        self.sys_dir = False
        self.temp_dir = None
        self.last_passid = -1
        self.column_count = 0
        self.failures = []


    def break_cols(self):
        if self.column_count > 75:
            sys.stdout.write("\n")
            self.column_count = 0


    def report_failure(self, msg):
        full_msg = "FAILED: %s" % msg
        if self.verbose:
            print full_msg
        else:
            if self.last_passid >= 0:
                sys.stdout.write('\b.')
            self.break_cols()
            sys.stdout.write("E")
            sys.stdout.flush()
            self.column_count = self.column_count + 1
            self.failures.append(full_msg)
            self.last_passid = -1


    def report_info(self, msg):
        full_msg = "INFO: %s" % msg
        if self.verbose:
            print full_msg
        else:
            if self.last_passid >= 0:
                sys.stdout.write('\b.')
            self.break_cols()
            sys.stdout.write("i")
            sys.stdout.flush()
            self.column_count = self.column_count + 1
            self.failures.append(full_msg)
            self.last_passid = -1


    def report_success(self, passid, msg):
        if self.verbose:
            print "OK: %s" % msg
        else:
            if self.last_passid >= 0:
                sys.stdout.write("\b")
                self.column_count = self.column_count - 1
            self.break_cols()
            np = int(passid)
            sys.stdout.write(passid)
            self.column_count = self.column_count + len(passid)
            sys.stdout.flush()
            #print "%d" % passid,
            self.last_passid = np


    def open_report(self):
        self.last_passid = -1

    def close_report(self):
        if not self.verbose:
            if self.last_passid >= 0:
                sys.stdout.write("\b.")
            self.break_cols()


    def reset_status_count(self):
        self.failures = []
        self.test_run = 0
        self.test_succeeded = 0
        self.column_count = 0


    def report_summary(self, title, run, succeeded):
        if succeeded <> run:
            print "%s: %d tests of %d failed" % (title, run - succeeded, run)
        else:
            print "%s: %d tests succeeded" % (title, succeeded)
        print


    def run_herschel_on_test(self, test_file, options, env=None):
        cmd = [os.path.join(self.executable_path, self.current_executable)]
        cmd.extend(options)
        if self.input_dir:
            cmd.append('-I')
            cmd.append(self.input_dir)
        if self.sys_dir:
            cmd.append('--isys=' + self.sys_dir)
        cmd.append(test_file)

#        print ""
#        print os.environ['PWD']
#        print cmd

        use_env = os.environ
        if env is not None:
            use_env = use_env.copy()
            use_env.update(env)

        return subprocess.Popen(cmd,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,
                                env=use_env).communicate()


    def run_make_on_test(self, options, env=None):
        cmd = ["make"]
        cmd.extend(options)

        use_env = os.environ
        if env is not None:
            use_env = use_env.copy()
            use_env.update(env)
        return subprocess.Popen(cmd,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,
                                env=use_env).communicate()


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


    def find_expected_file(self, test_file, passid, ext=".xml"):
        path, srcext = os.path.splitext(test_file)
        expected_file = path + passid + ext

        if os.path.exists(expected_file):
            return expected_file
        return False


    #----------------------------------------------------------------------------

    def run_pass_test_impl(self, test_file, options, passid,
                           errtest_func=None):
        expected_file = self.find_expected_file(test_file, "_%s" % (passid))
        if not expected_file:
            return

        test_count = 0

        output, erroutput = self.run_herschel_on_test(test_file, options)
        what_tag = "[%s] %s" % (passid, test_file)
        if erroutput:
            if errtest_func == None:
                self.report_failure("%s: %s" % (what_tag, erroutput))
                return
            test_count += 1
            if not errtest_func(test_file, what_tag, passid, erroutput):
                self.test_run += 1
                return


        expected_file = self.find_expected_file(test_file, "_%s" % (passid))
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
        for level in PHASES:
            if level in OPTIONS[domain]:
                self.run_pass_test_impl(test_file, OPTIONS[domain][level], level)



    #----------------------------------------------------------------------------

    def run_pass_failed_test_impl(self, test_file):
        pass


    def load_expected_syntax_errors_desc(self, test_file, passid):
        path, ext = os.path.splitext(test_file)
        expected_file = path + "_" + passid + ".expsynerr"

#        print "Print expect expsynerr file: %s" % expected_file
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
        for level in PHASES:
            if level in OPTIONS[domain]:
                self.run_pass_test_impl(test_file, OPTIONS[domain][level], level,
                                        self.check_for_errors)


    #----------------------------------------------------------------------------

    def os_ext(self):
        # TODO: need platform
        if platform.system() == "Windows":
            return ".exe"
        elif platform.system() == "Darwin":
            return ""
        elif platform.system() == "Linux":
            return ""
        else:
            return ""


    # returns [ Success, output, errout, returncode ]
    def run_test_binary(self, test_file, options, timeout=20, env=None):
        cmd = [test_file]
        cmd.extend(options)

        use_env = os.environ
        if env is not None:
            use_env = use_env.copy()
            use_env.update(env)

        p = subprocess.Popen(cmd,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE,
                             env=use_env)
        timed_out_mutable = [False]

        timer = threading.Timer(float(timeout), kill_process,
                                [p, timed_out_mutable, cmd[0] ])
        timer.start()

        timed_out = False
        try:
            output, erroutput = p.communicate()
        finally:
            timed_out = timed_out_mutable[0]
            timer.cancel()

        if timed_out:
            raise TimeoutException(test_file)

        return p.returncode, output, erroutput 


    def load_testdesc(self, test_file):
        compiledesc_file = self.find_expected_file(test_file, "", ext=".testdesc")
        if not compiledesc_file:
            return None

        try:
            f = file(compiledesc_file)
            desc = eval(f.read())
            return desc;
        finally:
            if f is not None:
                f.close()
        return None


    def check_contains_patterns(self, text, patterns, what_tag):
        for p in patterns:
            repattern = re.compile(p)
            if repattern.search(text) is None:
                self.report_failure("%s: exp pattern '%s' not found in output" % (what_tag, p))
                return False
        # all patterns match
        return True


    def check_tmp_run_simple(self, passid, what_tag, retc, binout, binerr):
        if retc != 0:
            self.report_info("%s: %d" % (what_tag, retc))
            self.report_failure("%s: test failed with returncode != 0" % what_tag)
        else:
            self.report_success(passid, "%s: test succeeds" % what_tag)
            self.test_succeeded += 1

    def check_output(self, compiledesc, key, output, what_tag):
        if compiledesc.has_key(key):
            if not self.check_contains_patterns(output, compiledesc[key], what_tag):
                print "BIN%s: <%s>" % (key, output)
                print "EXP%s: <%s>" % (key, compiledesc[key])
                self.report_failure("%s: differs from expected %s output" % (what_tag, key))
                return

    def check_tmp_run_detailed(self, passid, what_tag, retc, binout, binerr, compiledesc):
        if "retc" in compiledesc:
            expected_retc = compiledesc["retc"]
        else:
            expected_retc = 0

        if retc != expected_retc:
            self.report_info("%s: %d" % (what_tag, retc))
            self.report_failure("%s: test failed with returncode != 0" % what_tag)
            return

        self.check_output(compiledesc, "stdout", binout, what_tag)
        self.check_output(compiledesc, "stderr", binerr, what_tag)

        self.report_success(passid, "%s: test succeeds" % what_tag)
        self.test_succeeded += 1


    def run_test_binary_impl(self, tmp_binary, compiledesc, passid, what_tag):
        try:
            env = { }
            if compiledesc is not None:
                if 'runtime-options' in compiledesc:
                    rt_options = { 'HR_RUNTIME_DEBUG': compiledesc['runtime-options'] }
                    env.update(rt_options)

            retc, binout, binerr = self.run_test_binary(tmp_binary, [], env=env)

            if compiledesc is None:
                self.check_tmp_run_simple(passid, what_tag, retc, binout, binerr)
            else:
                self.check_tmp_run_detailed(passid, what_tag,
                                            retc, binout, binerr, compiledesc)

        except TimeoutException:
            self.report_failure("%s: test timed out" % what_tag)

        self.test_run += 1


    def run_compile_test(self, test_file, options, passid, errtest_func=None):
        compiledesc = self.load_testdesc(os.path.abspath(test_file))

        with TmpDirCtx("test_", base_dir=self.temp_dir, delete_on_exit=True) as tmp_dir:
            abs_test_file = os.path.abspath(test_file)

            with DirSaver(tmp_dir):
                basename, ext = os.path.splitext(os.path.basename(test_file))
                tmp_binary = os.path.abspath(basename + self.os_ext())

                options = [ "--Xl,On",            # disable link optimization
                            "-o", tmp_binary ]
                output, erroutput = self.run_herschel_on_test(abs_test_file, options)
                what_tag = "[%s] %s" % (passid, test_file)
                if erroutput:
                    self.report_failure("%s: %s" % (what_tag, erroutput))
                    self.test_run += 1
                    return

#                if output:
#                    print "Output: %s" % output

                self.run_test_binary_impl(tmp_binary, compiledesc, passid, what_tag)


    def run_complex_compile_test(self, test_dir, f, passid):
        """Run a makefile inside of the folder $test_dir/$f
We expect that the makefile generates an executable named $f in the temporary
folder we give in the environment variable TESTDIR.  The expected name is passed
in the environment variable APPNAME to the makefile.

The expected test data is expected in a test description file $test_dir/$f/$f.testdesc."""
        full_test_path = os.path.join(test_dir, f)

        with TmpDirCtx("test_", base_dir=self.temp_dir, delete_on_exit=True) as tmp_dir:
            abs_tmp_dir = os.path.abspath(tmp_dir)

            with DirSaver(full_test_path):
                compiledesc = self.load_testdesc(os.path.abspath(f + ".testdesc"))
                tmp_binary = os.path.abspath(os.path.join(abs_tmp_dir, f + self.os_ext()))

                output, erroutput = self.run_make_on_test([],
                                                          {'TESTDIR': abs_tmp_dir,
                                                           'APPNAME': f})
                what_tag = "[%s] %s" % (passid, f)

                if erroutput:
                    self.report_failure("%s: %s" % (what_tag, erroutput))
                    self.test_run += 1
                    return
#                if output:
#                    print "Output: %s" % output

                self.run_test_binary_impl(tmp_binary, compiledesc, passid, what_tag)


    #----------------------------------------------------------------------------

    def run_test(self, test_dir, src_file, domain):
        test_file = os.path.join(test_dir, src_file)

        self.current_executable = EXECUTABLES[domain]

        if domain == "compile":
            self.run_compile_test(test_file, "", "6")
        else:
            if src_file.startswith("failed-"):
                self.run_pass_failed_test(test_file, domain)
            elif src_file.startswith("ignore-"):
                pass
            else:
                self.run_pass_test(test_file, domain)
        self.current_executable = ''


    def run_all_tests(self, test_dir, domain):
        print "Run tests in %s:" % (domain)
        for f in os.listdir(test_dir):
            if f.endswith(".hr") or f.endswith(".h7"):
                self.open_report()
                self.run_test(test_dir, f, domain)
                self.close_report()
            elif os.path.isdir(os.path.join(test_dir, f)):
                self.open_report()
                self.run_complex_compile_test(test_dir, f, "6")
                self.close_report()

        if not self.verbose:
            sys.stdout.write("\b.\n")

            if len(self.failures) > 0:
                print
                print "FAILURES:"
                for f in self.failures:
                    print f

        #self.report_summary("SUMMARY", self.test_run, self.test_succeeded)

        self.update_statistics()


    def update_statistics(self):
        self.total_test_succeeded = self.total_test_succeeded + self.test_succeeded
        self.total_test_run = self.total_test_run + self.test_run


#----------------------------------------------------------------------------

def run_tests(tr, domains):
    for dom in domains:
        if os.path.isdir(arg):
            tr.run_all_tests(dom, dom)

    tr.report_summary("TOTAL", tr.total_test_run, tr.total_test_succeeded)


def main():
    parser = OptionParser()
    parser.add_option("-e", "--executable", dest="executable",
                      help="sets the dir to the compiler to use", metavar="FILE")
    parser.add_option("-V", "--verbose", action="store_true",
                      dest="verbose", default=False,
                      help="be more verbose")
    parser.add_option("-D", "--domain",
                      dest="domain", default="syntax",
                      help="select the domain of tests to run")
    parser.add_option("-I", "--input",
                      dest="input", default=".",
                      help="give the input directory to the compiler")
    parser.add_option("-J", "--isys",
                      dest="sysdir", default=".",
                      help="give the system input directory to the compiler")
    parser.add_option("-t", "--temp-dir",
                      dest="temp_dir", default=None,
                      help="give a directory where temporary data can be stored")
    parser.add_option("-T", "--test",
                      dest="test", default=None,
                      help="a comma separated list of test names")
    (options, args) = parser.parse_args()

    tr = TestRunner()

    if options.executable is not None:
        tr.executable_path = os.path.abspath(options.executable)

    if options.verbose:
        tr.verbose = options.verbose

    if options.input:
        tr.input_dir = options.input
    if options.sysdir:
        tr.sys_dir = options.sysdir

    if options.temp_dir:
        tr.temp_dir = options.temp_dir

    if options.test is not None:
        for test in options.test.split(','):
            tr.reset_status_count()

            fullpath = os.path.abspath(test)
            d, f = os.path.split(fullpath)
            dummy, domain = os.path.split(d)

            tr.open_report()
            tr.run_test(d, f, domain)
            tr.close_report()

            tr.update_statistics()
    else:
        for arg in args:
            tr.reset_status_count()

            if os.path.isdir(arg):
                d, f = os.path.split(arg)
                tr.run_all_tests(arg, f)
            else:
                d, f = os.path.split(arg)
                tr.run_test(d, f, options.domain)

    tr.report_summary("TOTAL", tr.total_test_run, tr.total_test_succeeded)


if __name__ == "__main__":
    main()
