#  This file is part of the heather package
#
#  Copyright (c) 2010 Gregor Klinke
#  All rights reserved.
#

top_srcdir=..

include $(top_srcdir)/config.mk

LLVM_ROOT = $(top_srcdir)/external/llvm/

LLVM_CPPFLAGS = $(shell $(LLVM_ROOT)/Release/bin/llvm-config --cppflags)
LLVM_LDFLAGS = $(shell $(LLVM_ROOT)/Release/bin/llvm-config --ldflags --libs core)

CXXFLAGS += -I$(top_srcdir) -I$(top_srcdir)/src -I$(top_srcdir)/external/UnitTest++/src \
	$(LLVM_CPPFLAGS)

LDFLAGS += $(LLVM_LDFLAGS)

SOURCES =         \
	apt.cpp         \
	file.cpp        \
	heather.cpp     \
	log.cpp         \
	macro.cpp       \
	numbers.cpp     \
	option.cpp      \
	parser.cpp      \
	parsertypes.cpp \
	pass.cpp        \
	pass1.cpp       \
	pass2.cpp       \
	port.cpp        \
	properties.cpp  \
	scope.cpp       \
	srcpos.cpp      \
	str.cpp         \
	strbuf.cpp      \
	symbol.cpp      \
	token.cpp       \
	tokeneval.cpp   \
	tokenizer.cpp   \
	tokenport.cpp   \
	type.cpp        \
	typeenum.cpp    \
	typectx.cpp     \
	valuesaver.cpp

HEADERS =         \
	apt.h           \
	common.h        \
	errcodes.h      \
	exception.h     \
	file.h          \
	log.h           \
	macro.h         \
	numbers.h       \
	option.h        \
	parser.h        \
	parsertypes.h   \
	pass.h          \
	pass1.h         \
	pass2.h         \
	port.h          \
	properties.h    \
	ptr.h           \
	refcountable.h  \
	registry.h      \
	scope.h         \
	srcpos.h        \
	str.h           \
	strbuf.h        \
	symbol.h        \
	token.h         \
	tokeneval.h     \
	tokenizer.h     \
	tokenport.h     \
	type.h          \
	typeenum.h      \
	typectx.h       \
	valuesaver.h


# ifeq ($(TARGET_OS),mac)
# HEADERS +=
# endif

# ifeq ($(TARGET_OS),linux)
# HEADERS +=
# endif

# ifeq ($(TARGET_OS),win)
# HEADERS +=
# endif

CPP_OBJS =  $(SOURCES:%.cpp=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

OBJS = $(CPP_OBJS)

DISTFILES = Makefile \
	$(SOURCES) $(HEADERS)



include $(top_srcdir)/build/pre.mk

all-local: $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE) APPX

APPBINARY = $(BUILDDIR)/$(BUILDSTYLE)/heather$(APPEXT)
APPX: $(APPBINARY)

$(APPBINARY): $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS)



clean-local:
	rm -f $(BUILDDIR)/$(BUILDSTYLE)/*$(OBJEXT) $(APP) $(DISTAPP)

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


TAGS: $(HEADERS) $(SOURCES)
	find -E . -regex ".*\.mm$$|.*\.cpp$$|.*\.h$$" -print | etags --members -


DEPS = $(patsubst %.cpp,$(DEPS_DIR)/%.d,$(SOURCES))

.PRECIOUS: $(DEPS_DIR)/%.d

-include $(DEPS)

include $(top_srcdir)/build/generic.mk
