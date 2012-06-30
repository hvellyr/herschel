#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#
#  This source code is released under the BSD License.

top_srcdir=..

include $(top_srcdir)/config.mk

CFLAGS += -I$(top_srcdir) -I$(top_srcdir)/runtime

SOURCES =         \
	builtin.c       \
	hash.c          \
	keyword.c       \
	list.c          \
	method.c        \
	rt.c            \
	rtstr.c         \
	trace.c         \
  typeid.c

HEADERS =         \
	hash.h          \
	herschel.h      \
	list.h          \
	trace.h         \
  typeid.h


C_OBJS =  $(SOURCES:%.c=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

OBJS = $(C_OBJS)


#--------
RTTESTS_SOURCES = unittest.c

RTTESTS_OBJS = $(RTTESTS_SOURCES:%.c=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

RTTESTS_LDFLAGS = $(LDFLAGS)

RTTESTS_APPBINARY = $(BUILDDIR)/$(BUILDSTYLE)/rt-tests$(APPEXT)

#--------
DISTFILES = Makefile \
	$(SOURCES) $(HEADERS) $(RTTESTS_SOURCES)




include $(top_srcdir)/build/pre.mk

all-local: $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE) RTLIBX RTTESTS_APPX

#--------
RTLIB = $(BUILDDIR)/$(BUILDSTYLE)/libhr$(LIBEXT)
RTLIBX: $(RTLIB)


#--------
RTTESTS_APPX: $(RTTESTS_APPBINARY)

$(RTTESTS_APPBINARY): $(RTTESTS_OBJS) $(RTLIB)
	$(CC) $(CFLAGS) $(RTTESTS_LDFLAGS) -o $@ $^ $(RTLIB)


# %$(LIBEXT)(%$(OBJEXT)): %$(OBJEXT)
# 	$(AR) cru $@ $%

$(RTLIB): $(OBJS)
	$(AR) cru $@ $^

# 	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS) $(LLVM_LIBS)



clean-local:
	rm -f $(RTLIB) $(RTTESTS_APPBINARY)

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


DEPS = $(patsubst %.c,$(DEPS_DIR)/%.d,$(SOURCES)) \
	$(patsubst %.c,$(DEPS_DIR)/%.d,$(RTTESTS_SOURCES))

.PRECIOUS: $(DEPS_DIR)/%.d

-include $(DEPS)

include $(top_srcdir)/build/generic.mk
