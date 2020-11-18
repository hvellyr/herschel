# -*-meson-*-
#
#   This file is part of the herschel package
#
#   Copyright (c) 2020 Gregor Klinke
#   All rights reserved.
#
#   This source code is released under the BSD License.

hr_test_sources = [
  'tst_main.cpp',
  'tst_apt.cpp',
  'tst_file.cpp',
  'tst_log.cpp',
  'tst_numbers.cpp',
  'tst_port.cpp',
  'tst_scope.cpp',
  'tst_srcpos.cpp',
  'tst_str.cpp',
  'tst_strbuf.cpp',
  'tst_symbol.cpp',
  'tst_tokenizer.cpp',
  'tst_token.cpp',
  'tst_tokeneval.cpp',
  'tst_tokenport.cpp',
  'tst_type.cpp',
  'tst_utils.cpp',
  'tst_valuesaver.cpp'
]

bootstrap_test_executable = executable('boostrap-test',
                                       hr_test_sources,
                                       include_directories : [cxxopts_inc, configuration_inc,
                                                              sysconf_inc, catch_inc],
                                       dependencies: [hrc_platform_dep],
                                       link_with : hrlib)

test('bootstrap', bootstrap_test_executable,
     workdir : meson.current_source_dir() / '../..') # change to project_source_root() when on 0.56
