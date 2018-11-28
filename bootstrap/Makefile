#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

top_srcdir=..

include $(top_srcdir)/config.mk

#------------------------------------------------------------------------------

SUBDIRS = test

ALL_SUBDIRS = test


#------------------------------------------------------------------------------
# libhrcommon - common library stuff
#------------------------------------------------------------------------------

LIBHRCOMMON_SOURCES =  \
	file.cpp             \
	filetool.cpp         \
	job.cpp              \
	log.cpp              \
	port.cpp             \
	require.cpp          \
	str.cpp              \
	strbuf.cpp           \
	xmlout.cpp

LIBHRCOMMON_HEADERS =  \
	common.hpp           \
	file.hpp             \
	filetool.hpp         \
	job.hpp              \
	log.hpp              \
	port.hpp             \
	require.hpp          \
	setup.hpp            \
	str.hpp              \
	strbuf.hpp           \
	valuesaver.hpp       \
	xmlout.hpp

ifeq ($(TARGET_OS),mac)
# HRC_HEADERS +=
LIBHRCOMMON_SOURCES += setup-unix.cpp setup-mac.cpp
#HRC_LIBS = $(LIBS) -framework Foundation
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
# libhrc - compiler code
#------------------------------------------------------------------------------

CXXFLAGS += -I$(top_srcdir) -I$(top_srcdir)/src \
	-I$(top_srcdir)/third-party/cxxopts/src \
	$(LLVM_CPPFLAGS)

LIBHRC_LDFLAGS = $(LDFLAGS) $(LLVM_LDFLAGS)

LIBHRC_LIBS = $(LIBS) $(BUILDDIR)/$(BUILDSTYLE)/libhrcommon$(LIBEXT)

LIBHRC_SOURCES =         \
	abstrpass.cpp       \
	ast.cpp             \
	compilepass.cpp     \
	compiler.cpp        \
	exprpass.cpp        \
	macro.cpp           \
	nodifypass.cpp      \
	numbers.cpp         \
	parsertypes.cpp     \
	predefined.cpp      \
	properties.cpp      \
	rootscope.cpp       \
	scope.cpp           \
	srcpos.cpp          \
	symbol.cpp          \
	token.cpp           \
	tokeneval.cpp       \
	tokenizer.cpp       \
	tokenport.cpp       \
	type.cpp            \
	typectx.cpp         \
	typeenum.cpp        \
	xmlrenderer.cpp


LIBHRC_HEADERS =         \
	abstrpass.hpp       \
	ast.hpp             \
	compilepass.hpp     \
	compiler.hpp        \
	errcodes.hpp        \
	exception.hpp       \
	exprpass.hpp        \
	macro.hpp           \
	nodifypass.hpp      \
	numbers.hpp         \
	parsertypes.hpp     \
	predefined.hpp      \
	properties.hpp      \
	registry.hpp        \
	rootscope.hpp       \
	scope.hpp           \
	srcpos.hpp          \
	symbol.hpp          \
	token.hpp           \
	tokeneval.hpp       \
	tokenizer.hpp       \
	tokenport.hpp       \
	type.hpp            \
	typectx.hpp         \
	typeenum.hpp        \
	typeprops.hpp       \
	typeprops-bool.hpp  \
	typeprops-char.hpp  \
	typeprops-float.hpp \
	typeprops-int.hpp   \
	typeprops-keyword.hpp \
	typeprops-string.hpp \
	xmlrenderer.hpp

LIBHRC_SOURCES2 =        \
	$(LIBHRC_SOURCES)   \
	annotate.cpp        \
	apt.cpp             \
	codegen.cpp         \
	codegen-apply.cpp   \
	codegen-binnode.cpp \
	codegen-if.cpp      \
	codegen-init.cpp    \
	codegen-func.cpp    \
	codegen-slot.cpp    \
	codegen-tools.cpp   \
	codegen-types.cpp   \
	codegen-vardef.cpp  \
	codegen-while.cpp   \
	externc.cpp         \
	transform.cpp       \
	traverse.cpp        \
	typify.cpp

LIBHRC_HEADERS2 =         \
	$(LIBHRC_HEADERS)   \
	annotate.hpp        \
	apt.hpp             \
	codegen.hpp         \
	codegen-apply.hpp   \
	codegen-binnode.hpp \
	codegen-func.hpp    \
	codegen-if.hpp      \
	codegen-init.hpp    \
	codegen-proxy.hpp   \
	codegen-slot.hpp    \
	codegen-tools.hpp   \
	codegen-types.hpp   \
	codegen-vardef.hpp  \
	codegen-while.hpp   \
	externc.hpp         \
	transform.hpp       \
	traverse.hpp        \
	typify.hpp          \
	utils.hpp


ifeq ($(TARGET_OS),mac)
# LIBHRC_HEADERS +=
endif

ifeq ($(TARGET_OS),linux)
# LIBHRC_HEADERS +=
endif

ifeq ($(TARGET_OS),win)
# LIBHRC_HEADERS +=
endif

LIBHRC_OBJS = $(LIBHRC_SOURCES:%.cpp=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

LIBHRC_LIB = $(BUILDDIR)/$(BUILDSTYLE)/libhrc$(LIBEXT)



#------------------------------------------------------------------------------
# hrc - compiler executable
#------------------------------------------------------------------------------

# LLVM_CPPFLAGS = $(shell $(LLVM_EXE)/llvm-config --cppflags backend bitwriter)
# LLVM_LDFLAGS = $(shell $(LLVM_EXE)/llvm-config --ldflags backend bitwriter)
# LLVM_LIBS = $(shell $(LLVM_EXE)/llvm-config --libs backend bitwriter)

CXXFLAGS += -I$(top_srcdir) -I$(top_srcdir)/src \
	-I$(top_srcdir)/third-party/cxxopts/src \
	$(LLVM_CPPFLAGS)

HRC_LDFLAGS = $(LDFLAGS) $(LLVM_LDFLAGS)

HRC_LIBS = $(LIBS) $(BUILDDIR)/$(BUILDSTYLE)/libhrcommon$(LIBEXT)

HRC_SOURCES = hrc.cpp

HRC_HEADERS =


ifeq ($(TARGET_OS),mac)
# HRC_HEADERS +=
HRC_LIBS += -framework CoreFoundation
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
HERSCHEL_LIBS += -framework CoreFoundation
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

#all-local: $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE) \
#	COMMON_LIBX LIBHRC_LIBX HRC_APPX HERSCHEL_APPX

all-local: $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE) \
	COMMON_LIBX LIBHRC_LIBX HRC_APPX


#--------
COMMON_LIBX: $(COMMON_LIB)

$(COMMON_LIB): $(LIBHRCOMMON_OBJS)
	@echo "[LIB] $@"
	@$(AR) cru $@ $^


#--------
LIBHRC_LIBX: $(LIBHRC_LIB)

$(LIBHRC_LIB): $(LIBHRC_OBJS)
	@echo "[LIB] $@"
	@$(AR) cru $@ $^


#--------
HRC_APPX: $(HRC_APPBINARY)

$(HRC_APPBINARY): $(HRC_OBJS) $(COMMON_LIB) $(LIBHRC_LIB)
	@echo "[LINK] $@"
	@$(CXX) $(CXXFLAGS) $(HRC_LDFLAGS) -o $@ $^ $(HRC_LIBS) $(LLVM_LIBS)


#--------
HERSCHEL_APPX: $(HERSCHEL_APPBINARY)

$(HERSCHEL_APPBINARY): $(HERSCHEL_OBJS) $(COMMON_LIB)
	@echo "[LINK] $@"
	@$(CXX) $(CXXFLAGS) $(HERSCHEL_LDFLAGS) -o $@ $^ $(HERSCHEL_LIBS)


#------------------------------------------------------------------------------
DISTFILES = Makefile \
	$(SOURCES) $(HEADERS)

clean-local:
	rm -f $(BUILDDIR)/$(BUILDSTYLE)/*$(OBJEXT) $(HRC_APPBINARY)
	rm -f $(HERSCHEL_APPBINARY) $(COMMON_LIB) $(LIBHRC_LIB)

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
