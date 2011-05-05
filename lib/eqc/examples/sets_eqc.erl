%%% File    : sets_eqc.erl
%%% Author  : Thomas Arts <thomas.arts@quviq.com>
%%% Description : QuickCheck properties for sets.erl
%%%               Based on "Testing Data Structures with QuickCheck"
%%% Created : 24 Mar 2010 by Thomas Arts

-module(sets_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

%% Create a generator for the opaque type "set". The generator will generate
%% symbolic calls which when evaluated computes a set. Each symbolic call has
%% the form {call, Module, Function, Arguments} and are evaluated using the
%% function eval/1.

%% To avoid generating infinite symbolic representations we pass the size
%% parameter to the generator and use it to make sure we stop eventually.

set(G) ->
  ?SIZED(Size,well_defined(set(Size,G))).

set(0,G) ->
  oneof([{call,sets,new,[]},
         {call,sets,from_list,[list(G)]}]);
set(N,G) ->
  frequency(
    [{5,set(0,G)},
     {3, ?LAZY(?LETSHRINK([Set],[set(N-1,G)],
                          {call,sets,add_element,[G, Set]}))},
     {1, ?LAZY(?LETSHRINK([Set],[set(N-1,G)],
                          {call,sets,del_element,[G, Set]}))},
     {1, ?LAZY(?LETSHRINK([Set1,Set2],[set(N div 2,G),set(N div 2,G)],
                          {call,sets,union,[Set1, Set2]}))},
     {1, ?LAZY(?LETSHRINK(Sets,list(set(N div 3,G)),
                          {call,sets,union,[Sets]}))},
     {1, ?LAZY(?LETSHRINK([Set1,Set2],[set(N div 2,G),set(N div 2,G)],
                          {call,sets,intersection,[Set1, Set2]}))},
     {1, ?LAZY(?LETSHRINK(Sets,?LET(L,nat(),vector(L+1,set(N div (L+1),G))),
                          {call,sets,intersection,[Sets]}))},
     {1, ?LAZY(?LETSHRINK([Set1,Set2],[set(N div 2,G),set(N div 2,G)],
                          {call,sets,subtract,[Set1, Set2]}))},
     {1, ?LAZY(?LETSHRINK([Set],[set(N div 2,G)],
                          {call,sets,filter,[function1(bool()), Set]}))}]).

%% The next step is to define a model interpretation, i.e. a simplified,
%% obviously correct implementation of the data type. In this case we use
%% usorted lists.

model(S) ->
  lists:sort(sets:to_list(S)).

%% Define the set operations on the model.

madd_element(E,S) ->
  lists:usort([E|S]).

mdel_element(E,S) ->
  S -- [E].

msize(S) ->
  length(S).

mis_element(E,S) ->
  lists:member(E,S).

munion(Ss) ->
  lists:usort(lists:append(Ss)).

mintersection(Sets) ->
  [ E || E <- lists:usort(lists:append(Sets)),
         lists:all(fun(Set) -> lists:member(E,Set) end, Sets)].

mis_disjoint(S1,S2) ->
    mintersection([S1,S2]) == [].
    
mfilter(Pred,S) ->
    [ E || E <- S, Pred(E)].

%% Define one property for each operation. We parameterize the properties on
%% the generator for the elements. To make it easy to run the properties we
%% also define special versions that use integers.

%% Each property have the same basic form: we check that a given operation
%% on sets has the same behaviour as the corresponding model operation.

prop_create() -> prop_create(int()).
prop_create(G) ->
    ?FORALL(Set,set(G),
            sets:is_set(eval(Set))).

prop_add_element() -> prop_add_element(int()).
prop_add_element(G) ->
    ?FORALL({E,Set},{G,set(G)},
            model(sets:add_element(E,eval(Set))) == madd_element(E,model(eval(Set)))).

prop_del_element() -> prop_del_element(int()).
prop_del_element(G) ->
    ?FORALL({E,Set},{G,set(G)},
            model(sets:del_element(E,eval(Set))) == mdel_element(E,model(eval(Set)))).


prop_union2() -> prop_union2(int()).
prop_union2(G) ->
    ?FORALL({Set1,Set2},{set(G),set(G)},
            model(sets:union(eval(Set1),eval(Set2))) == munion([model(eval(Set1)),model(eval(Set2))])).

prop_union() -> prop_union(int()).
prop_union(G) ->
    ?FORALL(Sets,list(set(G)),
            model(sets:union(eval(Sets))) == munion([model(Set) || Set<-eval(Sets)])).

prop_intersection2() -> prop_intersection2(int()).
prop_intersection2(G) ->
    ?FORALL({Set1,Set2},{set(G),set(G)},
            model(sets:intersection(eval(Set1),eval(Set2))) == 
            mintersection([model(eval(Set1)),model(eval(Set2))])).


prop_intersection() -> prop_intersection(int()).
prop_intersection(G) ->
    ?FORALL(Sets,eqc_gen:non_empty(list(set(G))),
            model(sets:intersection(eval(Sets))) == mintersection([model(Set) || Set<-eval(Sets)])).
    
prop_size() -> prop_size(int()).
prop_size(G) ->
    ?FORALL(Set,set(G),
            sets:size(eval(Set)) == msize(model(eval(Set)))).

prop_is_element() -> prop_is_element(int()).
prop_is_element(G) ->
    ?FORALL({E,Set},{G,set(G)},
            sets:is_element(E,eval(Set)) == mis_element(E,model(eval(Set)))).
    
prop_is_disjoint() -> prop_is_disjoint(int()).
prop_is_disjoint(G) ->
    ?FORALL({Set1,Set2},{set(G),set(G)},
            sets:is_disjoint(eval(Set1),eval(Set2)) == mis_disjoint(model(eval(Set1)),model(eval(Set2)))).
    
prop_filter() -> prop_filter(int()).
prop_filter(G) ->
    ?FORALL({Pred,Set},{function1(bool()),set(G)},
            model(sets:filter(Pred,eval(Set))) == mfilter(Pred,model(eval(Set)))).
    

