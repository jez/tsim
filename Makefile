all:
	@echo "No default target. See the Makefile for available targets."

.PHONY: mlton
mlton:
	mlton tsim.mlb

.PHONY: smlnj
smlnj:
	sml -m sources.cm
