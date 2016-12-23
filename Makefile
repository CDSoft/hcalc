# Handy Calc
# Copyright (C) 2016 Christophe Delord
# http://cdsoft.fr/hcalc
#
# This file is part of Handy Calc.
#
# Handy Calc is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Handy Calc is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Handy Calc.  If not, see <http://www.gnu.org/licenses/>.

MODULES	= $(wildcard src/[A-Z]*.hs) $(wildcard src/*.c)
SRC 	= src/hcalc.hs
TEST 	= src/hcalcTest.hs
MANUAL 	= src/hcalcManual.md
CSS		= src/hcalc.css

# Directory structure:
# src: source files
# doc: generated documentation
# doc/test: test result reports
# bin: binaries
# build: intermediate file, test executables, ...

UNAME   = $(shell uname)
WINEGHC = $(shell wine ghc --version 2>/dev/null || false)
GCCVERSION = $(shell gcc --version | head -1 | sed 's/.* \([0-9][0-9]*\)\..*/\1/')

ifeq "$(UNAME)" "Linux"

all: bin test doc

# Compilation on Linux

bin: bin/hcalc
test: doc/test/hcalcTest/hcalcTest.txt
doc: doc/hcalcManual.html README.md

ifneq "$(WINEGHC)" ""
bin: bin/hcalc.exe
GCC_WIN	= $(shell ls /usr/bin/i686-*mingw*-gcc | head -1 | sed 's/gcc$$//')
WINE    = wine
endif

else

# Compilation on Windows

bin: bin/hcalc.exe
test: doc/test/hcalcTest/hcalcTest.txt
doc: doc/hcalcManual.html

GCC_WIN	=
EXE		= .exe

endif

.DELETE_ON_ERROR:

#####################################################################
# Configuration
#####################################################################

DEPENDENCIES = here

setup:
	cabal update
	cabal install $(DEPENDENCIES)
ifneq "$(WINEGHC)" ""
	$(WINE) cabal update
	$(WINE) cabal install $(DEPENDENCIES)
endif

#####################################################################
# Compilation
#####################################################################

GHC_OPT = -O3 -Werror -Wall -fwarn-unused-do-bind

# On Ubuntu 16.10, ghc 8.0.1 fails compiling hCalc with gcc 6 without -no-pie
ifeq "$(GCCVERSION)" "6"
GHC_OPT_LINUX = -optl-no-pie
endif

clean:
	-rm -rf bin doc build .hpc

bin/hcalc: $(SRC) $(MODULES)
	@mkdir -p $(dir $@)
	@mkdir -p build/$(notdir $@)
	ghc $(GHC_OPT) $(GHC_OPT_LINUX) -outputdir build/$(notdir $@) -o $@ --make $(SRC) $(MODULES)
	@-strip $@

bin/hcalc.exe: $(SRC) $(MODULES) build/hcalc.exe/icon.o
	@mkdir -p $(dir $@)
	@mkdir -p build/$(notdir $@)
	$(WINE) ghc $(GHC_OPT) -outputdir build/$(notdir $@) -o $@ --make $(SRC) $(MODULES)
	@rm $@
	$(WINE) ghc $(GHC_OPT) -outputdir build/$(notdir $@) -o $@ --make $(SRC) $(MODULES) build/$(notdir $@)/icon.o
	@-strip $@

build/icon.png:
	@mkdir -p $(dir $@)
	convert -size 64x64 xc:white \
		-fill blue -stroke blue -strokewidth 0 \
		-draw "rectangle 28,8 35,55" \
		-draw "rectangle 8,28 55,35" \
		$@

build/icon.ico: build/icon.png
	convert $< \
		-bordercolor white -border 0 \
		\( -clone 0 -resize 32x32 \) \
		-delete 0 -alpha off -colors 2 \
		$@

build/hcalc.exe/icon.rc: build/icon.ico
	@mkdir -p $(dir $@)
	echo "100 ICON \"$<\"" > $@

build/hcalc.exe/icon.o: build/hcalc.exe/icon.rc
	$(GCC_WIN)windres $< $@

#####################################################################
# Unit tests
#####################################################################

tests: test

build/hcalcTest/hcalcTest$(EXE): $(TEST) $(MODULES)
	@mkdir -p $(dir $@)
	ghc $(GHC_OPT) -fhpc -outputdir $(dir $@) -o $@ --make $(TEST) $(MODULES)

doc/test/hcalcTest/hcalcTest.txt: build/hcalcTest/hcalcTest$(EXE)
	@mkdir -p $(dir $@)
	@rm -f $(dir $<)/*.tix
	@cd $(dir $<) && $(notdir $<)
ifneq "$(EXE)" ""
	@mv $(dir $<)/hcalcTest$(EXE).tix $(dir $<)/hcalcTest.tix 2>/dev/null
endif
	hpc markup --exclude=Main --destdir=$(dir $@) $<
	hpc report --exclude=Main $< | tee $@

#####################################################################
# Documentation
#####################################################################

README.md: $(MANUAL) bin/hcalc$(EXE)
	export PATH=bin:$$PATH; LANG=en pp $(MANUAL) | LANG=en pandoc -f markdown -t markdown_github -o $@

doc/hcalcManual.html: $(MANUAL) $(CSS) bin/hcalc$(EXE)
	@mkdir -p $(dir $@)
	export PATH=bin:$$PATH; LANG=en pp $(MANUAL) | LANG=en pandoc -f markdown -t html5 -S -s --self-contained -N --toc -c $(CSS) -o $@

$(CSS):
	wget -O $@ http://fun.cdsoft.fr/fun.css
