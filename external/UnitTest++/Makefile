top_srcdir = ../..

PKG = /UnitTest++

include $(top_srcdir)/config.mk

CXX ?= g++
CXXFLAGS ?= -g -Wall -W -ansi # -pedantic
LDFLAGS ?=
SED ?= sed
MV ?= mv
RM ?= rm
AR ?= ar

.SUFFIXES: .o .cpp

LIB = $(BUILDDIR)/$(BUILDSTYLE)$(PKG)/libUnitTest++.a
TEST = $(BUILDDIR)/$(BUILDSTYLE)$(PKG)/TestUnitTest++

src = src/AssertException.cpp \
	src/Test.cpp \
	src/Checks.cpp \
	src/TestRunner.cpp \
	src/TestResults.cpp \
	src/TestReporter.cpp \
	src/TestReporterStdout.cpp \
	src/ReportAssert.cpp \
	src/TestList.cpp \
	src/TimeConstraint.cpp \
	src/TestDetails.cpp \
	src/MemoryOutStream.cpp \
	src/DeferredTestReporter.cpp \
	src/DeferredTestResult.cpp \
	src/XmlTestReporter.cpp \
	src/CurrentTest.cpp

ifeq ($(MSYSTEM), MINGW32)
  src += src/Win32/TimeHelpers.cpp
else
  src += src/Posix/SignalTranslator.cpp \
	src/Posix/TimeHelpers.cpp
endif

test_src = src/tests/Main.cpp \
	src/tests/TestAssertHandler.cpp \
	src/tests/TestChecks.cpp \
	src/tests/TestUnitTest++.cpp \
	src/tests/TestTest.cpp \
	src/tests/TestTestResults.cpp \
	src/tests/TestTestRunner.cpp \
	src/tests/TestCheckMacros.cpp \
	src/tests/TestTestList.cpp \
	src/tests/TestTestMacros.cpp \
	src/tests/TestTimeConstraint.cpp \
	src/tests/TestTimeConstraintMacro.cpp \
	src/tests/TestMemoryOutStream.cpp \
	src/tests/TestDeferredTestReporter.cpp \
	src/tests/TestXmlTestReporter.cpp \
	src/tests/TestCurrentTest.cpp

CPP_OBJS = $(src:%.cpp=$(BUILDDIR)/$(BUILDSTYLE)$(PKG)/%$(OBJEXT))
TEST_OBJS = $(test_src:%.cpp=$(BUILDDIR)/$(BUILDSTYLE)$(PKG)/%$(OBJEXT))

DEPS = $(patsubst %.cpp,$(DEPS_DIR)/%.d,$(src))
TEST_DEPS = $(patsubst %.cpp,$(DEPS_DIR)/%.d,$(test_src))

define make-depend
  $(CXX) $(CXXFLAGS) -M $1 | \
  $(SED) -e 's,\($(notdir $2)\) *:,$(dir $2)\1: ,' > $3.tmp
  $(SED) -e 's/#.*//' \
      -e 's/^[^:]*: *//' \
      -e 's/ *\\$$//' \
      -e '/^$$/ d' \
      -e 's/$$/ :/' $3.tmp >> $3.tmp
  $(MV) $3.tmp $3
endef


include $(top_srcdir)/build/pre.mk

all-local: lib

lib: $(LIB)

$(BUILDDIR)/$(BUILDSTYLE)$(PKG): $(BUILDDIR)/$(BUILDSTYLE)
	@if [ ! -d $@ ]; then mkdir $@; fi

$(BUILDDIR)/$(BUILDSTYLE)$(PKG)/UnitTest++: $(BUILDDIR)/$(BUILDSTYLE)$(PKG)
	@if [ ! -d $@ ]; then mkdir $@; fi


$(LIB): $(BUILDDIR)/$(BUILDSTYLE)$(PKG) $(CPP_OBJS)
	@echo Creating $@ library...
	@$(AR) cru $@ $(CPP_OBJS)

$(TEST): $(BUILDDIR)/$(BUILDSTYLE)$(PKG) $(LIB) $(TEST_OBJS)
	@echo Linking $(TEST)...
	@$(CXX) $(LDFLAGS) -o $(TEST) $(TEST_OBJS) $(LIB)
	@echo Running unit tests...
	@$(TEST)

clean:
	-@$(RM) $(OBJS) $(TEST_OBJS) $(DEPS) $(TEST_DEPS) \
				  $(TEST) $(LIB) 2> /dev/null

-include $(DEPS)
-include $(TEST_DEPS)

include $(top_srcdir)/build/generic.mk
