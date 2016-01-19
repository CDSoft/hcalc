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

MODULES = $(wildcard [A-Z]*.hs) $(wildcard *.c)
SRC = hcalc.hs
TEST = hcalcTest.hs
ARCHIVE = $(SRC) $(TEST) $(MODULES) Makefile

DEPENDENCIES = here

ifeq "$(shell uname)" "Linux"

# Compilation on Linux
all: hcalc hcalc.exe hcalc.tgz
test: hcalcTest
WINE		= wine32
GCC_WIN		= $(shell ls /usr/bin/i686-*mingw32*-gcc | head -1 | sed 's/gcc$$//')

else

# Compilation on Windows
all: hcalc.exe
test: hcalcTest.exe
WINE		=
GCC_WIN		=

endif

UPX = upx -9qq

GHC_OPT 		= -O2 -Werror -Wall -fwarn-unused-do-bind
GHC_OPT_TEST 	=     -Werror -Wall -fwarn-unused-do-bind

BUILD			= build
BUILD_LINUX		= $(BUILD)/linux
BUILD_WIN		= $(BUILD)/win

GHC_OPT_LINUX	= $(GHC_OPT) -outputdir $(BUILD_LINUX)
GHC_OPT_WIN 	= $(GHC_OPT) -outputdir $(BUILD_WIN)

GHC_OPT_TEST_LINUX	= $(GHC_OPT_TEST) -outputdir $(BUILD_LINUX)/test -fhpc
GHC_OPT_TEST_WIN	= $(GHC_OPT_TEST) -outputdir $(BUILD_WIN)/test

.DELETE_ON_ERROR:

setup:
	cabal update
	cabal install $(DEPENDENCIES)
	$(WINE) cabal update
	$(WINE) cabal install $(DEPENDENCIES)

clean:
	-rm -rf hcalc hcalc.exe hcalcTest hcalcTest.exe $(BUILD) .hpc *.tix

tests: test

hcalc.tgz: $(ARCHIVE)
	tar czf $@ $^

hcalc: $(SRC) $(MODULES)
	@mkdir -p $(BUILD_LINUX)
	ghc $(GHC_OPT_LINUX) --make $^
	-strip $@
	-$(UPX) $@

hcalc.exe: $(SRC) $(MODULES) $(BUILD_WIN)/icon.o
	@mkdir -p $(BUILD_WIN)
	$(WINE) ghc $(GHC_OPT_WIN) --make $(SRC) $(MODULES)
	rm $@
	$(WINE) ghc $(GHC_OPT_WIN) --make $(SRC) $(MODULES) $(BUILD_WIN)/icon.o
	-strip $@
	-$(UPX) $@

hcalcTest: $(TEST) $(MODULES)
	@mkdir -p $(BUILD_LINUX)/test
	ghc $(GHC_OPT_TEST_LINUX) --make $(TEST) $(MODULES)
	-rm -f *.tix
	$@
	hpc markup --exclude=Main --destdir=$(BUILD_LINUX)/test $@
	hpc report --exclude=Main $@

hcalcTest.exe: $(TEST) $(MODULES)
	@mkdir -p $(BUILD_WIN)/test
	$(WINE) ghc $(GHC_OPT_TEST_WIN) --make $(TEST) $(MODULES)
	$(WINE) $@

$(BUILD)/icon.png:
	@mkdir -p $(dir $@)
	convert -size 64x64 xc:white \
		-fill blue -stroke blue -strokewidth 0 \
		-draw "rectangle 28,8 35,55" \
		-draw "rectangle 8,28 55,35" \
		$@

$(BUILD)/icon.ico: $(BUILD)/icon.png
	convert $< \
		-bordercolor white -border 0 \
		\( -clone 0 -resize 32x32 \) \
		-delete 0 -alpha off -colors 2 \
		$@

$(BUILD_WIN)/icon.rc: $(BUILD)/icon.ico
	@mkdir -p $(dir $@)
	echo "100 ICON \"$<\"" > $@

$(BUILD_WIN)/icon.o: $(BUILD_WIN)/icon.rc
	$(GCC_WIN)windres $< $@
