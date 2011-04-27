ERLC=erlc
ERLCFLAGS=-o
SRCDIR=src
TESTDIR=tests
LIBDIR=lib
BEAMDIR=ebin

all: compile

compile:
	mkdir -p $(BEAMDIR) ;
	$(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;

erlang_js: force_look
	cd $(LIBDIR)/erlang_js ; $(MAKE) $(MFLAGS);

erlv8: force_look
	cd $(LIBDIR)/erlv8 ; $(MAKE) $(MFLAGS);

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
	$(MAKE) -C $(LIBDIR)/erlang_js/ clean
	$(MAKE) -C $(LIBDIR)/erlv8/ clean

run:
	erl \
		-sname ggs \
		-mnesia dir '"/tmp/ggs"' \
		-boot start_sasl \
		-pa $(LIBDIR)/erlv8/ebin/ \
		-pa ebin \
		-pa src \
		-s start_ggs

force_look:
	true