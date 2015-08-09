.PHONY: clean strip all see re
SHELL=/bin/bash

files=$(wildcard *.tex)
pdfs=$(patsubst %.tex,%.pdf, $(files))
auxs=$(patsubst %.tex,%.aux, $(files))
logs=$(patsubst %.tex,%.log, $(files))
outs=$(patsubst %.tex,%.out, $(files))

all: informe.pdf

%.pdf: %.tex
	xelatex $<
	xelatex $<

clean:
	rm -f $(pdfs)
	rm -f $(auxs)
	rm -f $(logs)
	rm -f $(outs)

strip:
	rm -f $(auxs)
	rm -f $(logs)
	rm -f $(outs)

re: clean all

see: all
	evince $(pdfs)
