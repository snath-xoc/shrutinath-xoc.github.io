# scheme interpreter
SCHEME=scm -l

# list of pages to generate
PAGES=index.html papers.html

.SUFFIXES: .scm .html

.scm.html:
	$(SCHEME) $< >$@

all: $(PAGES) hacks/source.tar

clean:
	rm -f $(PAGES) core nohup.out a.out *~ */*~ abstracts/*.html 
	rm -f hacks/source.tar

$(PAGES): support/genhtml.scm support/gorgonzola.scm hacks/source.tar


index.html: index.scm

hacks/source.tar:
	rm -f hacks/source.tar
	make clean
	tar cf source.tar *.scm support/*.scm Makefile abstracts hacks
	mv source.tar hacks
