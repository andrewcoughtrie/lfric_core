##############################################################################
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014. However, it has been created with the help of the
# GungHo Consortium, whose members are identified at
# https://puma.nerc.ac.uk/trac/GungHo/wiki
##############################################################################

PDFLATEX ?= pdflatex
BIBTEX   ?= bibtex
INKSCAPE ?= inkscape
EPSTOPDF ?= epstopdf

export TEXINPUTS = build:$(DYNAMO_COMMON_FIGURES):../tex:/opt/ukmo/TeX/tex/latex/brand:
export BIBINPUTS = ../bibliography:
export BSTINPUTS = ../bibliography:

DOCUMENT  = $(basename $(wildcard *.latex))

# Locate all the "includegraphics" commands in the document and extract the
# figure they include.
FIGURES = $(shell grep -o -e '\\includegraphics[^{]*{[^}]*}' <$(DOCUMENT).latex | sed -n 's/.*{\(.*\)}/\1/p')
FIGURES := $(patsubst %,build/%.pdf,$(FIGURES))

BIBLIOGRAPHY = $(wildcard bibliography/*.bib)

ifneq (x$(BIBLIOGRAPHY),x)
    BBLFILE = build/$(DOCUMENT).bbl
endif

$(DOCUMENT).pdf: build/$(DOCUMENT).pdf | build
	cp $< $@

.PRECIOUS: build/$(DOCUMENT).pdf
build/$(DOCUMENT).pdf: $(BBLFILE) build/$(DOCUMENT).aux | build
	while ( grep "Rerun to get" build/$(DOCUMENT).log ); \
	do \
	    echo Rerunning build for cross-references; \
	    $(PDFLATEX) -output-directory build $(DOCUMENT).latex; \
	done

.PRECIOUS: build/$(DOCUMENT).bbl
build/$(DOCUMENT).bbl: build/$(DOCUMENT).aux $(BIBLIOGRAPHY)
	@echo Building $@
	cd build; $(BIBTEX) $(DOCUMENT).aux

.PRECIOUS: build/$(DOCUMENT).aux
build/$(DOCUMENT).aux: $(DOCUMENT).latex $(FIGURES) | build
	@echo Building $@
	$(PDFLATEX) -output-directory build $<

build/%.pdf: figures/%.svg | build
	@echo Rendering $<
	$(Q)$(INKSCAPE) -z -f $< -A $@

build/%.pdf: figures/%.eps | build
	@echo Rendering $<
	$(Q)$(EPSTOPDF) --outfile=$@ $<

build/%.pdf: $(DYNAMO_COMMON_FIGURES)/%.svg | build
	@echo Rendering $<
	$(Q)$(INKSCAPE) -z -f $< -A $@

build:
	@echo Creating $@
	@mkdir -p $@

.PHONY: clean
clean:
	@echo Cleaning
	-rm -rf build *.pdf

