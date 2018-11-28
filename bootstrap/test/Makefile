#  This file is part of the herschel package
#
#  Copyright (c) 2015 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

top_srcdir=../..

include $(top_srcdir)/config.mk

#------------------------------------------------------------------------------
# test executable
#------------------------------------------------------------------------------

CXXFLAGS += -I$(top_srcdir) -I$(top_srcdir)/bootstrap \
	-I$(top_srcdir)/third-party/catch/include \
	-I$(top_srcdir)/third-party/cxxopts/src \
	$(LLVM_CPPFLAGS)

TEST_SOURCES =         \
	tst_main.cpp         \
	tst_apt.cpp          \
	tst_file.cpp         \
	tst_log.cpp          \
	tst_numbers.cpp      \
	tst_port.cpp         \
	tst_scope.cpp        \
	tst_srcpos.cpp       \
	tst_str.cpp          \
	tst_strbuf.cpp       \
	tst_symbol.cpp       \
	tst_tokenizer.cpp    \
	tst_token.cpp        \
	tst_tokeneval.cpp    \
	tst_tokenport.cpp    \
	tst_type.cpp         \
	tst_utils.cpp        \
	tst_valuesaver.cpp

TEST_HEADERS =

TEST_OBJS = $(TEST_SOURCES:%.cpp=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

TEST_LDFLAGS = $(LDFLAGS) $(LLVM_LDFLAGS)

TEST_LIBS = $(LIBS) $(BUILDDIR)/$(BUILDSTYLE)/libhrcommon$(LIBEXT) \
	$(BUILDDIR)/$(BUILDSTYLE)/libhrc$(LIBEXT)

ifeq ($(TARGET_OS),mac)
# TEST_HEADERS +=
TEST_LIBS += -framework CoreFoundation
endif

ifeq ($(TARGET_OS),linux)
# TEST_HEADERS +=
endif

ifeq ($(TARGET_OS),win)
# TEST_HEADERS +=
endif

TEST_APPBINARY = $(BUILDDIR)/$(BUILDSTYLE)/bootstrap-test$(APPEXT)


#------------------------------------------------------------------------------
# targets
#------------------------------------------------------------------------------

SOURCES = $(TEST_SOURCES)
HEADERS = $(TEST_HEADERS)

include $(top_srcdir)/build/pre.mk

all-local: $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE) TEST_APPX

TEST_APPX: $(TEST_APPBINARY)

$(TEST_APPBINARY): $(TEST_OBJS)
	@echo "[LINK] $@"
	@$(CXX) $(CXXFLAGS) $(TEST_LDFLAGS) -o $@ $^ $(TEST_LIBS) $(LLVM_LIBS)


#------------------------------------------------------------------------------
DISTFILES = Makefile \
	$(SOURCES) $(HEADERS)

clean-local:
	rm -f $(BUILDDIR)/$(BUILDSTYLE)/*$(OBJEXT) $(TEST_APPBINARY)

distclean-local: clean-local
	-rm -rf $(DEPS_DIR) TAGS

rebuild: clean all



TAGS: $(HRC_HEADERS) $(SOURCES)
	find -E . -regex ".*\.mm$$|.*\.cpp$$|.*\.h$$" -print | etags --members -

DEPS = $(patsubst %.cpp,$(DEPS_DIR)/%.d,$(SOURCES))

.PRECIOUS: $(DEPS_DIR)/%.d

-include $(DEPS)

include $(top_srcdir)/build/generic.mk
