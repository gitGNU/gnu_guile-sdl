# GNUmakefile

all: coverage.tdt

coverage.tdt: api.tdt
	./scan -o $@ --coverage $<

INCDIR = /usr/include

api.tdt: $(INCDIR)/SDL/*.h
	./scan -o $@ --api $^

clean:
	rm -f *.tdt

# GNUmakefile ends here
