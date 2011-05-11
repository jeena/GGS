-module(ggs_network_eqc_test).

-include_lib("../../lib/eqc/include/eqc.hrl").

-compile(export_all).


start() ->
    eqc:quickcheck(prop_connect()).

prop_connect() ->
    {Atom, Value} = ggs_network:connect(),
   eqc:equals(Atom, ok).


%% new(length) == old(length) or 
%% new(length) == old(length) + 1
prop_setItem() ->
    ggs_db:init(),           
    F = (fun(T,N,K,V) -> ggs_db:setItem(T,N,K,V), ggs_db:length(T,N) end),
    ?FORALL({T,N,K,V},{bitstring(),bitstring(),bitstring(),bitstring()},
    (ggs_db:length(T,N) + 1 == F(T,N,K,V)) or 
    (ggs_db:length(T,N) == F(T,N,K,V))).


%% new(length) >= 0 and 
%% old(length) == new(length) or 
%% old(length) - 1 == new(length) 
prop_removeItem() ->
    ggs_db:init(),
    F = (fun(T,N,K) -> ggs_db:removeItem(T,N,K), ggs_db:length(T,N)  end),
    G = (fun(A,B) -> ((A == B) or (A == B + 1)) and (B >= 0) end),
    ?FORALL({T,N,K},{bitstring(),bitstring(),bitstring()},
    G(ggs_db:length(T,N), F(T,N,K))).


%% clear(X) -> (length(X,?) == 0)
prop_clear() ->
    ggs_db:init(),
    F = (fun(T,N) -> ggs_db:clear(T), ggs_db:length(T,N) end),
    ?FORALL({T,N},{bitstring(),bitstring()},
    F(T,N) == 0).

%% ? -> length(?,?) >= 0
prop_length() ->
    ggs_db:init(),
    F = fun(T,N,K,V) -> ggs_db:setItem(T,N,K,V), ggs_db:length(T,N) end,
    G = fun(T,N,K) -> ggs_db:removeItem(T,N,K), ggs_db:length(T,N) end,
    ?FORALL({{T,N,K,V},{T2,N2,K2}},
    {{bitstring(),bitstring(),bitstring(),bitstring()},
    {bitstring(),bitstring(),bitstring()}},
    (((F(T,N,K,V) >= 0) and (G(T2,N2,K2) >= 0)))).


%% key(X,Y,length(X,Y)) -> Exists
prop_key() ->
    ggs_db:init(),
    F = fun(T,N) -> case ggs_db:length(T,N) of 0 -> true; X -> 
                        case ggs_db:key(T,N,X) of _ -> true end  end end,
    ?FORALL({T,N},{bitstring(),bitstring()},
    F(T,N)).

