#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#

top_srcdir=..

include $(top_srcdir)/config.mk

CFLAGS += -I$(top_srcdir) -I$(top_srcdir)/runtime 

SOURCES =         \
	rt.c

HEADERS =         \
	rt.h


C_OBJS =  $(SOURCES:%.c=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

OBJS = $(C_OBJS)

DISTFILES = Makefile \
	$(SOURCES) $(HEADERS)



include $(top_srcdir)/build/pre.mk

all-local: $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE) RTLIBX

RTLIB = $(BUILDDIR)/$(BUILDSTYLE)/libhr$(LIBEXT)
RTLIBX: $(RTLIB)

# %$(LIBEXT)(%$(OBJEXT)): %$(OBJEXT)
# 	$(AR) cru $@ $%

$(RTLIB): $(OBJS)
	$(AR) cru $@ $^

# 	$(CXX) $(LDFLAGS) -o $@ $^ $(LIBS) $(LLVM_LIBS)



clean-local:
	rm -f $(RTLIB)

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


DEPS = $(patsubst %.c,$(DEPS_DIR)/%.d,$(SOURCES))

.PRECIOUS: $(DEPS_DIR)/%.d

-include $(DEPS)

include $(top_srcdir)/build/generic.mk
