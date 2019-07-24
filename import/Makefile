# -*- coding: utf-8 -*-
#
# Author: PB
# Maintainer(s): PB
# License: (c) HRDAG 2019, GPL v2 or newer
#
# PH/import/Makefile
# -----------------------------------------------------------
#
.PHONY: all clean

all: output/manila.rds output/quezon.rds output/caloocan.rds

clean:
	rm output/*

output/quezon.rds: \
		src/import-quezon.R \
		input/QuezonCity_Complete_Sources_clean.xlsx
	Rscript --vanilla $< \
		--input=input/QuezonCity_Complete_Sources_clean.xlsx \
		--output=$@

output/manila.rds: \
		src/import-manila.R \
		input/Manila_Complete_Sources_clean.xlsx
	Rscript --vanilla $< \
		--input=input/Manila_Complete_Sources_clean.xlsx \
		--output=$@

output/caloocan.rds: \
		src/import-caloocan.R \
		input/Caloocan_Complete_Sources_clean.xlsx
	Rscript --vanilla $< \
		--input=input/Caloocan_Complete_Sources_clean.xlsx \
		--output=$@

# done.
