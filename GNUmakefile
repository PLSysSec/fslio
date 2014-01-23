RUBBER=rubber -Wrefs -Wmisc

pdf: main.pdf
.PHONY: pdf

main.pdf: main.tex 
	$(RUBBER) --pdf main.tex
.PHONY: main.pdf

main.tex: main.ltx #graphs
	lhs2TeX -o main.tex main.ltx

#graphs: cacheattack.svg internaltiming.svg
#	inkscape -D -f internaltiming.svg -e internaltiming.png
#	inkscape -D -f cacheattack.svg -e cacheattack.png

clean:
	$(RUBBER) --pdf --clean main.tex
	$(RUBBER) --ps --clean main.tex
	rm *.png
.PHONY: clean

ps: paper.ps
.PHONY: ps

