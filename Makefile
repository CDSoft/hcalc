# Handy Calc
# Copyright (C) 2016-2020 Christophe Delord
# http://cdelord.fr/hcalc
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

LIBRARIES	= $(wildcard src/*.hs) $(wildcard src/*.c)
MAIN		= app/hcalc.hs
TEST 		= test/hcalcTest.hs
MANUAL 		= doc/hcalcManual.md
CSS			= doc/hcalc.css
CONF		= package.yaml stack.yaml

BIN_DIR := $(shell stack path --local-install-root)/bin
BUILD = .stack-work

HCALC = $(BIN_DIR)/hcalc

all: $(HCALC) doc

$(HCALC): $(LIBRARIES) $(MAIN) $(CONF)
	stack build

install: $(HCALC)
	stack install

.PHONY: test
test:
	stack clean
	stack test --coverage

doc: doc/hcalcManual.html README.md

clean:
	stack clean
	rm -rf $(BUILD)
	rm -f doc/hcalcManual.html README.md
	rm -f *.tix

.DELETE_ON_ERROR:

export mdate := $(shell LANG=C date -r $(MANUAL) +'%d %B %Y')
export version := $(shell stack run -- version)
export hcalc_tar := $(shell stack run -- version | sed '/^$$/d' | sed 's/ //' | sed 's/ /-/').tgz
export hcalc_url := http://cdelord.fr/hcalc/$(hcalc_tar)

README.md: $(MANUAL) $(HCALC)
	export PATH=$(dir $(HCALC)):$$PATH; LANG=en pandoc -F abp -f markdown -t gfm $< -o $@

doc/hcalcManual.html: $(MANUAL) $(CSS) $(HCALC)
	@mkdir -p $(dir $@)
	export PATH=$(dir $(HCALC)):$$PATH; LANG=en pandoc -F abp -f markdown -t html -s --self-contained -N --toc -c $(CSS) $< -o $@
