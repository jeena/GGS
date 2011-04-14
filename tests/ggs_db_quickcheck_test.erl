-module(ggs_db_quickcheck_test).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).


%prop_delete() ->
%	?FORALL({X, Xs}, {int(), list(int())}, 
%	  not lists:member(X, lists:delete(X, Xs))).

%test_delete() ->
%	quickcheck(prop_delete()).


prop_getitem ->
	ggs_db:init(),
	?FORALL({}).




