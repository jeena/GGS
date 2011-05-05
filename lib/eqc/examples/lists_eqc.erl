%%% File    : lists_eqc.erl
%%% Author  : Thomas Arts <thomas.arts@quviq.com>
%%%           Ulf Norell <ulf.norell@quviq.com>
%%% Description : QuickCheck tests for some functions from the lists library.
%%% Created : 23 Mar 2010 by Thomas Arts <thomas.arts@quviq.com>

-module(lists_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

% === Testing lists:delete/2 ===

% The lists:delete/2 function removes an element from a list. Here's a property
% we might want for this function: after you've removed an element from a list
% it's not there anymore. The corresponding QuickCheck property is:

prop_delete_0() ->
  ?FORALL({X, Xs}, {int(), list(int())},
    not lists:member(X, lists:delete(X, Xs))).

% Checking this property for 100 random values and lists it might actually
% pass.
test_delete_0() ->
  quickcheck(prop_delete_0()).

% However, rerunning the property a few more times will reveal a problem:
test_delete_0_more() ->
  quickcheck(numtests(2000,prop_delete_0())).

% We get output looking like this:
%   74> lists_eqc:test_delete_0_more().
%   ............................................................................
%   ............................................................................
%   ............................................................................
%   ............................................................................
%   ....................................................Failed! After 377 tests.
%   {7,[-6,1,23,24,7,7]}
%   Shrinking..(2 times)
%   {7,[7,7]}
%   false
% There is a problem with our specification. lists:delete/2 only removes the
% first occurrence of the element, something our specification fails to take
% into account.

% Before fixing the problem in the specification, it's worth thinking about why
% we needed so many tests to find the bug. In order to find the bug we need to
% generate a value and a list such that the value appears twice in the list.
% What's the probability of that? To answer that question we can write a new
% property:

prop_member_probability() ->
  ?FORALL({X, Xs}, {int(), list(int())},
    collect(lists:member(X, Xs), true)).

% This property always succeeds, but for every test case it records whether the
% generated value appears (even once) in the list. Running the property a large
% number of times reveals that the probability that a random value appears in a
% random list is around 8%. No wonder it's hard to find a test case where it
% appears at least twice!

% To make it easier to find a failing case, we can change our property to only
% look at cases where the value appears at least once in the list. To do this
% we use the ?IMPLIES macro.

prop_delete_1() ->
  ?FORALL({X, Xs}, {int(), list(int())},
  ?IMPLIES(lists:member(X, Xs),
    not lists:member(X, lists:delete(X, Xs)))).

% Now the output will look something like this:
%   102> eqc:quickcheck(lists_eqc:prop_delete_1()).
%   xxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxx.
%   .xxxxxxxxxxxxxx.xx.xxxxxxx.xxxxxxxxxxx.xxxxxxxxxxxxxxxxxxx.xxx.xxxxxxxxxxxxx
%   xxxxxxxxx.xxxxxxxxxxx.xxxxx.xxxxxxxxxxxxxxxxxxxxxx.xx.xxxxxxxxxxxx.xxxxxxxxx
%   xxxxxxxxxxxxxxxxxxxxxxxxx..xxxxx.xxxx.xxxxxxxxx.xxxxxFailed! After 22 tests.
%   {1,[31,35,35,34,-21,-13,7,1,2,1,-35,2]}
%   Shrinking.....(5 times)
%   {1,[1,1]}
%   false
% The 'x's represent test cases that were discarded because they didn't satisfy
% the condition of the ?IMPLIES.

% Now that we have a property that fails reliably we can use it to document our
% misconception about the behaviour of lists:delete/2. The fails/1 function
% takes a property that is expected to fail and fails it doesn't.

prop_delete_2() ->
  fails(
  ?FORALL({X, Xs}, {int(), list(int())},
  ?IMPLIES(lists:member(X, Xs),
    not lists:member(X, lists:delete(X, Xs))))).

% Let's fix the specification. One possibility would be to count the number of
% occurrences of the value we're removing before and after calling
% lists:delete/2, but we can write a more precise specification quite elegantly:

prop_delete_3() ->
  ?FORALL({Xs, X, Ys}, {list(int()), int(), list(int())},
  ?IMPLIES(not lists:member(X, Xs),
    equals(lists:delete(X, Xs ++ [X] ++ Ys),
           Xs ++ Ys))).

% The equals function compares its arguments for equality and, if they're not
% equal, prints the arguments. This lets us see what lists:delete actually
% returned in the case we get a failing test case. Try removing the ?IMPLIES
% and run the property to see what it looks like.

% === Testing lists:seq/3 ===

% The lists:seq/2 function has recently changed in Erlang.  The new
% specification allows lists:seq(1,0) and returns [] in that case.  Copying the
% specification from the manual still reveals an error
% eqc:quickcheck(lists_eqc:prop_seq0()) due to the fact that a special case is
% overlooked. The fix is in prop_seq().

% This is the property according to the documentation. This property fails with
% a badarith exception on the test case {0, 0, 0}. The problem is that the
% specification of the length is not correct for increment 0.
prop_seq0() ->
  ?FORALL({From,To,Incr},{int(),int(),int()},
  case catch lists:seq(From,To,Incr) of
    {'EXIT',_} ->
      (To < From-Incr andalso Incr > 0) orelse
      (To > From-Incr andalso Incr < 0) orelse
      (Incr==0 andalso From /= To);
    List ->
      is_list(List) andalso
          length(List) == (To-From+Incr) div Incr
  end).

% This is the property as it holds.
prop_seq() ->
  ?FORALL({From,To,Incr},{int(),int(),int()},
  case catch lists:seq(From,To,Incr) of
    {'EXIT',_} ->
      (To < From-Incr andalso Incr > 0) orelse
      (To > From-Incr andalso Incr < 0) orelse
      (Incr==0 andalso From /= To);
    List when Incr /= 0 ->
      is_list(List) andalso
      length(List) == (To-From+Incr) div Incr;
    List when Incr == 0 ->
      length(List) == 1
  end).

% This is probably how one would like seq to behave.
prop_seq_wish(Seq) ->
  ?FORALL({From,To,Incr},{int(),int(),int()},
  case catch Seq(From,To,Incr) of
    [] -> Incr > 0 andalso From > To orelse
          Incr < 0 andalso From < To;
    [_]  when Incr == 0 -> From == To;
    List when Incr /= 0 andalso is_list(List) ->
        length(List) == (To-From+Incr) div Incr;
    {'EXIT', _} ->
      Incr == 0 andalso From /= To;
    _ ->
      false
  end).

prop_seq_wish() ->
  prop_seq_wish(fun lists:seq/3).

% Here is a reference implementation satisfying prop_seq_wish.
seq(From, To, 0) when From /= To ->
  exit("seq: increment 0");
seq(From, From, _Incr) ->
  [From];
seq(From, To, Incr) when From > To andalso Incr > 0 ->
  [];
seq(From, To, Incr) when From < To andalso Incr < 0 ->
  [];
seq(From, To, Incr) ->
  [From | seq(From + Incr, To, Incr)].

prop_seq_wish_granted() ->
  prop_seq_wish(fun seq/3).

% The previous properties only specifies the length of the result of
% lists:seq/3. We also want to make sure that it contains the right elements.
% In particular, if lists:seq(From, To, Incr) returns a non-empty list, the
% first element of the list should be From, and the difference between adjacent
% elements should be Incr. We've already tested that the list has the right
% number of elements so we don't have to worry about when the list ends.

% First some helper functions:

% We're only interested in non-empty results of seq/3.
is_cons([_|_]) -> true;
is_cons(_)     -> false.

% We want to look at the difference between adjacent elements.
diff_adjacent([X,Y|Xs]) ->
  [Y - X|diff_adjacent([Y|Xs])];
diff_adjacent(_) ->
  [].

% We use ?IMPLIES to ignore the cases when lists:seq does not return a
% non-empty list. To make sure we still get interesting test cases we collect
% the lengths of the results and the increments we've chosen.
prop_seq_elements() ->
  ?FORALL({From, To, Incr}, {int(), int(), int()},
  begin
    Seq = (catch lists:seq(From, To, Incr)),
    ?IMPLIES(is_cons(Seq),
    begin
      Adj = diff_adjacent(Seq),
      ?WHENFAIL(io:format("Seq = ~w\nAdj = ~w", [Seq, Adj]),
        % When you have several collects in the same property you can give them
        % names using collect/3 and with_title/1 to distinguish them.
        % We divide the actual numbers by 5 to reduce the number of different
        % values collected.
        collect(with_title(lengths), length(Seq) div 5,
        collect(with_title(incr), Incr div 5,
          hd(Seq) == From andalso
          lists:all(fun(D) -> D == Incr end, Adj)
        ))
      )
    end)
  end).

