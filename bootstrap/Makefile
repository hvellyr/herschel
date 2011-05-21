#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

top_srcdir=..

include $(top_srcdir)/config.mk

#------------------------------------------------------------------------------
# libhrcommon - common library stuff
#------------------------------------------------------------------------------

LIBHRCOMMON_SOURCES =  \
	file.cpp             \
	filetool.cpp         \
	job.cpp              \
	log.cpp              \
	option.cpp           \
	port.cpp             \
	require.cpp          \
	str.cpp              \
	strbuf.cpp           \
	valuesaver.cpp

LIBHRCOMMON_HEADERS =  \
	common.h             \
	file.h               \
	filetool.h           \
	job.h                \
	log.h                \
	option.h             \
	port.h               \
	require.h            \
	setup.h              \
	str.h                \
	strbuf.h             \
	valuesaver.h

ifeq ($(TARGET_OS),mac)
# HRC_HEADERS +=
LIBHRCOMMON_SOURCES += setup-unix.cpp setup-mac.cpp
# HRC_LIBS = $(LIBS) -framework Carbon
endif

ifeq ($(TARGET_OS),linux)
# HRC_HEADERS +=
LIBHRCOMMON_SOURCES += setup-unix.cpp setup-linux.cpp
endif

ifeq ($(TARGET_OS),win)
# HRC_HEADERS +=
LIBHRCOMMON_SOURCES += setup-win.cpp
endif


LIBHRCOMMON_OBJS = $(LIBHRCOMMON_SOURCES:%.cpp=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

COMMON_LIB = $(BUILDDIR)/$(BUILDSTYLE)/libhrcommon$(LIBEXT)


#------------------------------------------------------------------------------
# hrc - compiler
#------------------------------------------------------------------------------

# LLVM_CPPFLAGS = $(shell $(LLVM_EXE)/llvm-config --cppflags backend bitwriter)
# LLVM_LDFLAGS = $(shell $(LLVM_EXE)/llvm-config --ldflags backend bitwriter)
# LLVM_LIBS = $(shell $(LLVM_EXE)/llvm-config --libs backend bitwriter)

CXXFLAGS += -I$(top_srcdir) -I$(top_srcdir)/src \
	-I$(top_srcdir)/external/UnitTest++/src $(LLVM_CPPFLAGS)

HRC_LDFLAGS = $(LDFLAGS) $(LLVM_LDFLAGS)

HRC_LIBS = $(LIBS) $(BUILDDIR)/$(BUILDSTYLE)/libhrcommon$(LIBEXT)

HRC_SOURCES =       \
	annotate.cpp      \
	apt.cpp           \
	codegen.cpp       \
	codegen-alloc.cpp \
	codegen-binnode.cpp \
	codegen-init.cpp  \
	codegen-func.cpp  \
	codegen-tools.cpp \
	codegen-types.cpp \
	compilepass.cpp   \
	compiler.cpp      \
	externc.cpp       \
	hrc.cpp           \
	macro.cpp         \
	numbers.cpp       \
	parsertypes.cpp   \
	pass.cpp          \
	pass1.cpp         \
	pass2.cpp         \
	predefined.cpp    \
	properties.cpp    \
	rootscope.cpp     \
	scope.cpp         \
	srcpos.cpp        \
	symbol.cpp        \
	token.cpp         \
	tokeneval.cpp     \
	tokenizer.cpp     \
	tokenport.cpp     \
	transform.cpp     \
	traverse.cpp      \
	type.cpp          \
	typeenum.cpp      \
	typectx.cpp       \
	typify.cpp        \
	xmlout.cpp

HRC_HEADERS =       \
	annotate.h        \
	apt.h             \
	codegen.h         \
	codegen-binnode.h \
	codegen-init.h    \
	codegen-tools.h   \
	codegen-types.h   \
	compilepass.h     \
	compiler.h        \
	errcodes.h        \
	exception.h       \
	externc.h         \
	macro.h           \
	numbers.h         \
	parsertypes.h     \
	pass.h            \
	pass1.h           \
	pass2.h           \
	predefined.h      \
	properties.h      \
	ptr.h             \
	refcountable.h    \
	registry.h        \
	rootscope.h       \
	scope.h           \
	srcpos.h          \
	symbol.h          \
	token.h           \
	tokeneval.h       \
	tokenizer.h       \
	tokenport.h       \
	transform.h       \
	traverse.h        \
	type.h            \
	typeenum.h        \
	typectx.h         \
	typify.h          \
	xmlout.h


ifeq ($(TARGET_OS),mac)
# HRC_HEADERS +=
HRC_LIBS += -framework Carbon
endif

ifeq ($(TARGET_OS),linux)
# HRC_HEADERS +=
endif

ifeq ($(TARGET_OS),win)
# HRC_HEADERS +=
endif

HRC_OBJS = $(HRC_SOURCES:%.cpp=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

HRC_APPBINARY = $(BUILDDIR)/$(BUILDSTYLE)/hrc$(APPEXT)


#------------------------------------------------------------------------------
# herschel - driver
#------------------------------------------------------------------------------

HERSCHEL_SOURCES = herschel.cpp
HERSCHEL_HEADERS =

HERSCHEL_LIBS = $(LIBS) $(BUILDDIR)/$(BUILDSTYLE)/libhrcommon$(LIBEXT)

ifeq ($(TARGET_OS),mac)
# HRC_HEADERS +=
HERSCHEL_LIBS += -framework Carbon
endif

ifeq ($(TARGET_OS),linux)
# HRC_HEADERS +=
endif

ifeq ($(TARGET_OS),win)
# HRC_HEADERS +=
endif


HERSCHEL_OBJS = $(HERSCHEL_SOURCES:%.cpp=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

HERSCHEL_LDFLAGS = $(LDFLAGS)


#------------------------------------------------------------------------------
# targets
#------------------------------------------------------------------------------

SOURCES = $(HRC_SOURCES) $(LIBHRCOMMON_SOURCES) $(HERSCHEL_SOURCES)
HEADERS = $(HRC_HEADERS) $(LIBHRCOMMON_HEADERS) $(HERSCHEL_HEADERS)

include $(top_srcdir)/build/pre.mk

all-local: $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE) COMMON_LIBX HRC_APPX HERSCHEL_APPX


#--------
COMMON_LIBX: $(COMMON_LIB)

$(COMMON_LIB): $(LIBHRCOMMON_OBJS)
	$(AR) cru $@ $^


#--------
HRC_APPX: $(HRC_APPBINARY)

$(HRC_APPBINARY): $(HRC_OBJS) $(COMMON_LIB)
	$(CXX) $(CXXFLAGS) $(HRC_LDFLAGS) -o $@ $^ $(HRC_LIBS) $(LLVM_LIBS)


#--------
HERSCHEL_APPX: $(HERSCHEL_APPBINARY)

$(HERSCHEL_APPBINARY): $(HERSCHEL_OBJS) $(COMMON_LIB)
	$(CXX) $(CXXFLAGS) $(HERSCHEL_LDFLAGS) -o $@ $^ $(HERSCHEL_LIBS)


#------------------------------------------------------------------------------
DISTFILES = Makefile \
	$(SOURCES) $(HEADERS)

clean-local:
	rm -f $(BUILDDIR)/$(BUILDSTYLE)/*$(OBJEXT) $(HRC_APPBINARY) $(COMMON_LIB)

distclean-local: clean-local
	-rm -rf $(DEPS_DIR) TAGS


# dist-local:
# 	for d in . lang sys; do \
# 		mkdir $(top_distdir)/src/$$d ; \
# 		for f in $$d/*.xxx; do \
# 			cp -p $$f $(top_distdir)/src/$$f >/dev/null 2>&1; \
# 		done; \
# 	done


rebuild: clean all



TAGS: $(HRC_HEADERS) $(SOURCES)
	find -E . -regex ".*\.mm$$|.*\.cpp$$|.*\.h$$" -print | etags --members -

DEPS = $(patsubst %.cpp,$(DEPS_DIR)/%.d,$(SOURCES))

.PRECIOUS: $(DEPS_DIR)/%.d

-include $(DEPS)

include $(top_srcdir)/build/generic.mk
