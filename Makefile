CFLAGS = -g -O2
SRC = mm.h mm_sys.h scutum.h $(CSRC)
CSRC = mm.c mm_test.c gentable.c testing.c $(SC_C)
SC_C = bootstrap.c main.c support.c $(PRIM_C)
PRIM_C = initialize.c syntax.c arithmetic.c primitives.c operations.c interp.c
ISRC = scutum.h testing.c support.c initialize.c syntax.c arithmetic.c primitives.c operations.c interp.c
MANIFEST = Makefile COPYING $(SCRIPTS) $(SRC) lib.scheme read.scheme
SCRIPTS = generate_test_items generate_prim_items generate_interp_items writeifnew testify
INDENT = indent -nce -br -i4 -npro -nip -di1 -nbc -npsl -ndj
COMMON_O = mm.o support.o initialize.o syntax.o arithmetic.o primitives.o operations.o interp.o bootstrap.o
TARGETS = scutum tests
EXTRATARGETS = ok.txt mm_test
ALLTARGETS = $(TARGETS) $(EXTRATARGETS)
GENHEADERS = primitives.h support.h mm_static_tables.h prim_items.h prim_init_items.h test_items.h interp_items.h


default: $(TARGETS)

mm_static_tables.h: gentable
	./gentable | ./writeifnew mm_static_tables.h

gentable: gentable.c mm.h mm_sys.h
	$(CC) $(CFLAGS) -o gentable gentable.c

primitives.h: $(PRIM_C)
	cat $(PRIM_C) | \
         grep '^Activation scp_' | \
         sed -e 's/)/);/' | \
         ./writeifnew primitives.h

prim_items.h: $(PRIM_C)
	./generate_prim_items $(PRIM_C) | ./writeifnew prim_items.h

prim_init_items.h: $(PRIM_C)
	./generate_prim_items -t void -p sc_pi_ $(PRIM_C) | \
         ./writeifnew prim_init_items.h

INTERP_C = interp.c primitives.c
interp_items.h: $(INTERP_C)
	./generate_interp_items $(INTERP_C) | ./writeifnew interp_items.h

support.h: $(SC_C)
	cat $(SC_C) | \
         grep '^[A-Za-z][^ ]* sc_' | sed -e 's/)/);/' | \
         ./writeifnew support.h

test_items.h: generate_test_items testing.c
	./generate_test_items testing*.c | ./writeifnew test_items.h

pretty:
	for i in $(ISRC); do             \
          $(INDENT) $$i;                 \
          expand $$i | ./writeifnew $$i; \
        done 

clean:
	rm -rf tags *.o *.dSYM *.BAK *~ $(ALLTARGETS) \
              *.bb *.bbg *.da *.d *.g???

cleaner: $(GENHEADERS) clean
	touch $(GENHEADERS)

squeakyclean: clean
	rm -f gentable coverage $(GENHEADERS) $(ALLTARGETS) listing.pdf depend

coverage: $(SRC)
	$(MAKE) clean
	# $(MAKE) 'CFLAGS = -g' testing.o
	$(MAKE) CC=gcc 'CFLAGS = -g -fprofile-arcs -ftest-coverage' tests
	./tests
	for i in *.c; do gcov -f $$i || true; done
	head -100000 *.c.gcov > coverage
	$(MAKE) clean

debug: clean
	$(MAKE) 'CFLAGS=-g' tests scutum

tags: $(SRC)
	ctags $(SRC)

scutum: $(COMMON_O) main.o
	$(CC) $(CFLAGS) -o scutum $(COMMON_O) main.o -lm

ok.txt:
	echo ok > ok.txt

tests: $(COMMON_O) testing.o ok.txt
	$(CC) $(CFLAGS) -o tests $(COMMON_O) testing.o -lm

mm_test: mm.o mm_test.o
	$(CC) $(CFLAGS) -o mm_test mm.o mm_test.o

tar:
	test -d .archive || mkdir .archive
	tar cvfz .archive/scutum-`date +%Y%m%d`.tgz $(MANIFEST)

depend: $(MANIFEST) $(GENHEADERS)
	gcc -std=c99 -MM $(CPPFLAGS) $(CSRC) > depend
	tail -n `cat depend | wc -l` Makefile | diff - depend

### Lines below here come from generated depend file.
### But they need to be pasted here by hand.

mm.o: mm.c mm_sys.h mm.h mm_static_tables.h
gentable.o: gentable.c mm_sys.h mm.h
testing.o: testing.c scutum.h mm.h support.h primitives.h test_items.h
bootstrap.o: bootstrap.c scutum.h mm.h support.h
main.o: main.c scutum.h mm.h support.h
support.o: support.c mm_sys.h mm.h scutum.h support.h
initialize.o: initialize.c scutum.h mm.h support.h primitives.h \
  prim_items.h prim_init_items.h
syntax.o: syntax.c scutum.h mm.h support.h
arithmetic.o: arithmetic.c scutum.h mm.h support.h primitives.h
primitives.o: primitives.c scutum.h mm.h support.h primitives.h
operations.o: operations.c scutum.h mm.h support.h
interp.o: interp.c scutum.h mm.h support.h primitives.h interp_items.h
