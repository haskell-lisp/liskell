PACKAGE=LskPrelude
VERSION=0.1

LskPrelude.o:
	liskell -package-name LskPrelude -package ghc -package liskell --make LskPrelude
libHSLskPrelude.a:
	ar r libHSLskPrelude.a LskPrelude.o

HSLskPrelude.o:
	ld -r -o HSLskPrelude.o LskPrelude.o

.PHONY: install

install-inplace: package.conf.inplace libHSLskPrelude.a HSLskPrelude.o
	ghc-pkg update package.conf.inplace

PACKAGE_CPP_OPTS += -DPACKAGE=${PACKAGE}
PACKAGE_CPP_OPTS += -DVERSION=${VERSION}

IMPORT_DIR_INPLACE=${PWD}
LIB_DIR_INPLACE=${PWD}

DEPENDS += $(shell ghc-pkg latest ghc)
DEPENDS += $(shell ghc-pkg latest base)

package.conf.inplace   : package.conf.in
	$(CPP) $(RAWCPP_FLAGS) -P \
		-DIMPORT_DIR='"$(IMPORT_DIR_INPLACE)"' \
		-DLIB_DIR='"$(LIB_DIR_INPLACE)"' \
		-DINCLUDE_DIR='"$(INCLUDE_DIR_INPLACE)"' \
		-DDATA_DIR='"$(DATA_DIR_INPLACE)"' \
		-DHTML_DIR='"$(HTML_DIR_INPLACE)"' \
		-DDEPENDS='$(DEPENDS)' \
		-DHADDOCK_IFACE='"$(HADDOCK_IFACE_INPLACE)"' \
		-DFPTOOLS_TOP_ABS='"${FPTOOLS_TOP_ABS}"' \
		-x c $(PACKAGE_CPP_OPTS) $< | \
	grep -v '^#pragma GCC' | \
	sed -e 's/""//g' -e 's/:[ 	]*,/: /g' >$@

clean:
	rm LskPrelude.hi LskPrelude.o libHSLskPrelude.a HSLskPrelude.o package.conf.inplace

