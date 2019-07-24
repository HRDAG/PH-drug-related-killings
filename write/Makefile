# vim: set ft=make noet ts=4
#
# Authors:     PB
# Maintainers: PB
# Copyright:   2019, HRDAG, GPL v2 or later
# ============================================
# PH/write/Makefile
#
HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')
LCMCR := $(HERE)/lcmcr/output

magics := output/PH-results.yaml \
	output/PH-estimates.csv.gz \
	output/PH-results.csv.gz

.PHONY: all clean

inputs := \
		$(LCMCR)/manila-unidentified-lcmcr.rds \
		$(LCMCR)/manila-police-lcmcr.rds \
		$(LCMCR)/quezon-unidentified-lcmcr.rds \
		$(LCMCR)/quezon-police-lcmcr.rds \
		$(LCMCR)/caloocan-unidentified-lcmcr.rds \
		$(LCMCR)/caloocan-police-lcmcr.rds \
		$(LCMCR)/manila-unidentified-upsetr.pdf \
		$(LCMCR)/manila-unidentified.yaml \
		$(LCMCR)/manila-unidentified-density.pdf \
		$(LCMCR)/manila-unidentified-trace.pdf \
		$(LCMCR)/manila-unidentified-omitted.pdf \
		$(LCMCR)/manila-police-upsetr.pdf \
		$(LCMCR)/manila-police.yaml \
		$(LCMCR)/manila-police-density.pdf \
		$(LCMCR)/manila-police-trace.pdf \
		$(LCMCR)/manila-police-omitted.pdf \
		$(LCMCR)/quezon-unidentified-upsetr.pdf \
		$(LCMCR)/quezon-unidentified.yaml \
		$(LCMCR)/quezon-unidentified-density.pdf \
		$(LCMCR)/quezon-unidentified-trace.pdf \
		$(LCMCR)/quezon-unidentified-omitted.pdf \
		$(LCMCR)/quezon-police-upsetr.pdf \
		$(LCMCR)/quezon-police.yaml \
		$(LCMCR)/quezon-police-density.pdf \
		$(LCMCR)/quezon-police-trace.pdf \
		$(LCMCR)/quezon-police-omitted.pdf \
		$(LCMCR)/caloocan-unidentified-upsetr.pdf \
		$(LCMCR)/caloocan-unidentified.yaml \
		$(LCMCR)/caloocan-unidentified-density.pdf \
		$(LCMCR)/caloocan-unidentified-trace.pdf \
		$(LCMCR)/caloocan-unidentified-omitted.pdf \
		$(LCMCR)/caloocan-police-upsetr.pdf \
		$(LCMCR)/caloocan-police.yaml \
		$(LCMCR)/caloocan-police-density.pdf \
		$(LCMCR)/caloocan-police-trace.pdf \
		$(LCMCR)/caloocan-police-omitted.pdf

all: $(magics) output/PH-Columbia-HRDAG-2019.pdf

clean:
	rm output/*


$(magics): \
		src/make-magic-numbers.R \
		$(inputs)
	Rscript --vanilla $<

output/PH-Columbia-HRDAG-2019.pdf: \
		src/PH-Columbia-HRDAG-2019.Rmd \
		$(inputs) \
		hand/hrdag.bib \
		src/header.tex \
		$(magics) \
		input/stabile_logo1.jpg \
		input/hrdag_ID_horiz_w-desc-sdahrj_rgb.pdf
	-rm src/PH-Columbia-HRDAG-2019.pdf src/PH-Columbia-HRDAG-2019.tex
	Rscript -e "rmarkdown::render('$<')"
	mv src/PH-Columbia-HRDAG-2019.pdf output/
	-rm src/PH-Columbia-HRDAG-2019.tex

# done
