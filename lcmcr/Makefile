# -*- coding: utf-8 -*-
# vim: set ts=4 noet ft=make
#
# Author: PB
# Maintainer(s): PB
# License: (c) HRDAG 2019, GPL v2 or newer
#
# PH/lcmcr/Makefile
#
# -----------------------------------------------------------
#
HERE := $(shell Rscript --vanilla -e 'write(here::here(), stdout())')
.PHONY: all clean

all: \
		output/manila-unidentified-upsetr.pdf \
		output/manila-unidentified-lcmcr.rds \
		output/manila-unidentified.yaml \
		output/manila-unidentified-density.pdf \
		output/manila-unidentified-trace.pdf \
		output/manila-unidentified-omitted.pdf \
	  output/manila-police-upsetr.pdf \
		output/manila-police-lcmcr.rds \
		output/manila-police.yaml \
		output/manila-police-density.pdf \
		output/manila-police-trace.pdf \
		output/manila-police-omitted.pdf \
		output/quezon-unidentified-upsetr.pdf \
		output/quezon-unidentified-lcmcr.rds \
		output/quezon-unidentified.yaml \
		output/quezon-unidentified-density.pdf \
		output/quezon-unidentified-trace.pdf \
		output/quezon-unidentified-omitted.pdf \
	  output/quezon-police-upsetr.pdf \
		output/quezon-police-lcmcr.rds \
		output/quezon-police.yaml \
		output/quezon-police-density.pdf \
		output/quezon-police-trace.pdf \
		output/quezon-police-omitted.pdf \
		output/caloocan-unidentified-upsetr.pdf \
		output/caloocan-unidentified-lcmcr.rds \
		output/caloocan-unidentified.yaml \
		output/caloocan-unidentified-density.pdf \
		output/caloocan-unidentified-trace.pdf \
		output/caloocan-unidentified-omitted.pdf \
	  output/caloocan-police-upsetr.pdf \
		output/caloocan-police-lcmcr.rds \
		output/caloocan-police.yaml \
		output/caloocan-police-density.pdf \
		output/caloocan-police-trace.pdf \
		output/caloocan-police-omitted.pdf

clean:
	rm output/*

output/manila-police-%: \
		src/lcmcr.R \
		$(HERE)/import/output/manila.rds
	cd ../import && make
	Rscript --vanilla $< \
		--region=manila \
		--perp=police

output/manila-unidentified-%: \
		src/lcmcr.R \
		$(HERE)/import/output/manila.rds
	cd ../import && make
	Rscript --vanilla $< \
		--region=manila \
		--perp=unidentified

output/quezon-police-%: \
		src/lcmcr.R \
		$(HERE)/import/output/quezon.rds
	cd ../import && make
	Rscript --vanilla $< \
		--region=quezon \
		--perp=police

output/quezon-unidentified-%: \
		src/lcmcr.R \
		$(HERE)/import/output/quezon.rds
	cd ../import && make
	Rscript --vanilla $< \
		--region=quezon \
		--perp=unidentified

output/caloocan-police-%: \
		src/lcmcr.R \
		$(HERE)/import/output/caloocan.rds
	cd ../import && make
	Rscript --vanilla $< \
		--region=caloocan \
		--perp=police

output/caloocan-unidentified-%: \
		src/lcmcr.R \
		$(HERE)/import/output/caloocan.rds
	cd ../import && make
	Rscript --vanilla $< \
		--region=caloocan \
		--perp=unidentified

# done.
