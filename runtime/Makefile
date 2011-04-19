#  This file is part of the herschel package
#
#  Copyright (c) 2010-2011 Gregor Klinke
#  All rights reserved.
#

top_srcdir=..

include $(top_srcdir)/config.mk

CFLAGS += -I$(top_srcdir) -I$(top_srcdir)/runtime 

SOURCES =         \
	hash.c          \
	list.c          \
	method.c        \
	rt.c            \
  typeid.c

HEADERS =         \
	hash.h          \
	list.h          \
	rt.h


C_OBJS =  $(SOURCES:%.c=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

OBJS = $(C_OBJS)


#--------
RTUT_SOURCES = unittest.c

RTUT_OBJS = $(RTUT_SOURCES:%.c=$(BUILDDIR)/$(BUILDSTYLE)/%$(OBJEXT))

RTUT_LDFLAGS = $(LDFLAGS)

RTUT_APPBINARY = $(BUILDDIR)/$(BUILDSTYLE)/rtut$(APPEXT)

#--------
DISTFILES = Makefile \
	$(SOURCES) $(HEADERS) $(RTUT_SOURCES)




include $(top_srcdir)/build/pre.mk

all-local: $(BUILDDIR) $(BUILDDIR)/$(BUILDSTYLE) RTLIBX RTUT_APPX

#--------
RTLIB = $(BUILDDIR)/$(BUILDSTYLE)/libhr$(LIBEXT)
RTLIBX: $(RTLIB)


#--------
RTUT_APPX: $(RTUT_APPBINARY)

$(RTUT_APPBINARY): $(RTUT_OBJS) $(RTLIB)
	$(CC) $(CFLAGS) $(RTUT_LDFLAGS) -o $@ $^ $(RTLIB)


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
