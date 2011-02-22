ERLC=erlc
ERLCFLAGS=-o
SRCDIR=src
TESTDIR=tests
BEAMDIR=./ebin

all: compile erlang_js

compile:
	mkdir -p $(BEAMDIR) ;
	$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;

erlang_js:
	$(MAKE) -C erlang_js/ ;

test:
	echo "==> test $(MOD)" ;
	mkdir -p $(BEAMDIR) ;
ifeq ($(strip $(MOD)),)
	$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(TESTDIR)/*.erl ;
	cd $(BEAMDIR) ; erl -noinput -eval 'eunit:test({dir, "."}, [verbose]), init:stop()' ;
else
	$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(TESTDIR)/$(MOD)_test.erl ;
	cd $(BEAMDIR) ; erl -noinput -eval 'eunit:test($(MOD)_test, [verbose]), init:stop()' ;
endif

clean:
	rm -rf $(BEAMDIR)/*.beam ;
	rm -rf erl_crush.dump ;
	echo "==> clean ggs" ;
	$(MAKE) -C erlang_js/ clean

run:
	erl -sname ggs -mnesia -boot start_sasl -pa erlang_js/ebin/ -pa ebin -pa src -s start_ggs

    