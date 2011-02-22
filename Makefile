ERLC=erlc
ERLCFLAGS=-o
SRCDIR=src
TESTDIR=tests
BEAMDIR=./ebin

all: compile erlang_js

compile:
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;

erlang_js:
	$(MAKE) -C erlang_js/

test: compile erlang_js
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(TESTDIR)/*.erl ;
	@ cd $(BEAMDIR) ; erl -noinput -eval 'eunit:test({dir, "."}, [verbose]), init:stop()' ;

clean:
	@ rm -rf $(BEAMDIR)/*.beam ;
	@ rm -rf erl_crush.dump ;
	@ echo "==> clean ggs" ;
	@ $(MAKE) -C erlang_js/ clean

run:
	@ erl -sname ggs -mnesia -boot start_sasl -pa erlang_js/ebin/ -pa ebin -pa src -s start_ggs
