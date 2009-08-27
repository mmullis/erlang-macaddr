APPNAME=macaddr

SUB_DIRECTORIES = src test

include vsn.mk

DOC_OPTS={def,{version,\"$(MACADDR_VSN)\"}}

all: subdirs

subdirs:
	mkdir -p ebin
	@for d in $(SUB_DIRECTORIES); do \
		(cd $$d; $(MAKE)); \
		echo $$d; \
	done

clean:
	@for d in $(SUB_DIRECTORIES); do \
		(cd $$d; $(MAKE) clean); \
	done

docs:
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop

test: clean subdirs
	@echo Testing...
	@erl -noshell -pa ebin -s macaddr_tests test -s init stop

console: clean subdirs
	erl -pa ebin

coverage: subdirs
	git submodule init lib/coverize
	git submodule update lib/coverize
	make -C lib/coverize
	mkdir -p coverage
	erl -noshell \
		-pa ebin \
		-pa lib/coverize/ebin \
		-s eunit_helper run_cover \
		-s init stop


