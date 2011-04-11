ERLC=erlc
ERLCFLAGS=-o
SRCDIR=src
TESTDIR=tests
BEAMDIR=ebin

all: compile erlang_js

compile:
	mkdir -p $(BEAMDIR) ;
	$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;

erlang_js: force_look
	cd erlang_js ; $(MAKE) $(MFLAGS);

test:
	echo "==> test $(MOD)" ;
	mkdir -p $(BEAMDIR) ;
ifeq ($(strip $(MOD)),)
	$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(TESTDIR)/*.erl ;
	cd $(BEAMDIR) ; erl -noinput -pa ../erlang_js/ebin -eval 'eunit:test({dir, "."}, [verbose]), init:stop()' ;
else
	$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(TESTDIR)/$(MOD)_test.erl ;
	cd $(BEAMDIR) ; erl -noinput -pa ../erlang_js/ebin -eval 'eunit:test($(MOD)_test, [verbose]), init:stop()' ;
endif

clean:
	rm -rf $(BEAMDIR)/*.beam ;
	rm -rf $(SRCDIR)/*.beam ;
	rm -rf erl_crush.dump ;
	echo "==> clean ggs" ;
	$(MAKE) -C erlang_js/ clean

run:
	erl \
		-sname ggs \
		-mnesia dir '"/tmp/ggs"' \
		-boot start_sasl \
		-pa erlang_js/ebin/ \
		-pa ebin \
		-pa src \
		-s start_ggs

force_look:
	true