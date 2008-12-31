TOP = .
include $(TOP)/mk/boilerplate.mk

SUBDIRS = timeout

CLEAN_FILES += mk/wordsize.mk

all ::
	cd tests/liskell && $(MAKE) $(MFLAGS)

include $(TOP)/mk/target.mk
