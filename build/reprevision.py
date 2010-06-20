#!/bin/env python2.5
#
#  This file is part of the heather package
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

from optparse import OptionParser
import subprocess
import os

def get_repository_base_revision(hg_path):
    output, outputerr = subprocess.Popen([hg_path, "id", "--debug"],
                                         stdout=subprocess.PIPE).communicate()

    rep_id = output.split(" ")[0].split("+")[0]

    print rep_id


if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-H", "--hg", dest="hg", default="hg",
                      help="sets the path to mercurial to use", metavar="FILE")

    (options, args) = parser.parse_args()

    get_repository_base_revision(options.hg)
