# -*-meson-*-
#
#   This file is part of the herschel package
#
#   Copyright (c) 2020 Gregor Klinke
#   All rights reserved.
#
#   This source code is released under the BSD License.

project('Herschel', ['cpp'],
        version : '0.0.1',
        license : 'BSD',
        default_options : ['cpp_std=c++14',
                           'buildtype=debug'])

set_variable('copyright_year', '2002-2020')
set_variable('copyright_owner', 'Gregor C. Klinke')

if get_option('buildtype') == 'debug'
  add_project_arguments('-DIS_DEBUG=1', language: 'cpp')
endif

conf_data = configuration_data()
conf_data.set('version', meson.project_version())

prefix = get_option('prefix')
conf_data.set('prefix', prefix)
conf_data.set('bindir', prefix / get_option('bindir'))
conf_data.set('libdir', prefix / get_option('libdir'))
conf_data.set('pkglibdir', prefix / get_option('libdir') / 'herschel')
conf_data.set('includedir', prefix / get_option('includedir'))
conf_data.set('pkgincludedir', prefix / get_option('includedir') / 'herschel')
conf_data.set('datadir', prefix / get_option('datadir'))
conf_data.set('pkgdatadir', prefix / get_option('datadir') / 'herschel')

if host_machine.system() == 'darwin'
  conf_data.set('OS_mac', 1)
elif host_machine.system() == 'windows'
  conf_data.set('OS_win', 1)
elif host_machine.system() == 'linux'
  conf_data.set('OS_linux', 1)
endif


cxxopts_inc = include_directories('third-party/cxxopts/include')
catch_inc = include_directories('third-party/catch/include')


subdir('bootstrap')
