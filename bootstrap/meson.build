# -*-meson-*-
#
#   This file is part of the herschel package
#
#   Copyright (c) 2020 Gregor Klinke
#   All rights reserved.
#
#   This source code is released under the BSD License.

versionconf_data = configuration_data()
versionconf_data.set('version', meson.project_version())
versionconf_data.set('name', meson.project_name())
versionconf_data.set('copyright_year', copyright_year)
versionconf_data.set('copyright_owner', copyright_owner)


configure_file(input : 'version.h.in',
               output : 'version.h',
               configuration : versionconf_data)

configure_file(input : 'config-local.h.in',
               output : 'config-local.h',
               configuration : conf_data)

configuration_inc = include_directories('.')


hrc_platform_dep = dependency('', required: false)

if host_machine.system() == 'darwin'
  hrc_platform_dep = dependency('CoreFoundation')
endif


libhr_sources = [
  'abstrpass.cpp',
  'annotate.cpp',
  'annotate2.cpp',
  'ast.cpp',
  'codegen-utils.cpp',
  'compilepass.cpp',
  'compiler.cpp',
  'exprpass.cpp',
  'file.cpp',
  'filetool.cpp',
  'job.cpp',
  'log.cpp',
  'macro.cpp',
  'nodifypass.cpp',
  'numbers.cpp',
  'parsertypes.cpp',
  'port.cpp',
  'predefined.cpp',
  'properties.cpp',
  'require.cpp',
  'rootscope.cpp',
  'scope.cpp',
  'srcpos.cpp',
  'str.cpp',
  'strbuf.cpp',
  'symbol.cpp',
  'token.cpp',
  'tokeneval.cpp',
  'tokenizer.cpp',
  'tokenport.cpp',
  'type.cpp',
  'typecheck.cpp',
  'typectx.cpp',
  'typeenum.cpp',
  'typify.cpp',
  'xmlout.cpp',
  'xmlrenderer.cpp'
]


if host_machine.system() == 'darwin'
  libhr_sources += [
    'setup-unix.cpp',
    'setup-mac.cpp']
elif host_machine.system() == 'windows'
  libhr_sources += [
    'setup-win.cpp'
  ]
elif host_machine.system() == 'linux'
  libhr_sources += [
    'setup-unix.cpp',
    'setup-linux.cpp'
  ]
endif


hrlib = shared_library('hr',
                       libhr_sources,
                       include_directories : [configuration_inc],
                       dependencies: [hrc_platform_dep],
                       install : false)

hrc_sources = ['hrc.cpp']
hrc_executable = executable('hrc',
                            hrc_sources,
                            include_directories : [cxxopts_inc, configuration_inc],
                            dependencies: [hrc_platform_dep],
                            link_with : hrlib)



subdir('test')
