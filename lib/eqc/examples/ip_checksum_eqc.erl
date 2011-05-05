%%% File    : ip_checksum_eqc.erl
%%% Author  : Ulf Norell <ulf.norell@quviq.com>,
%%%           Thomas Arts <thomas.arts@quviq.com>
%%% Description : QuickCheck properties for ip_checksum.erl
%%% Created : 7 Jun 2010 by Ulf Norell
-module(ip_checksum_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

% == Testing IP checksum implementations ==

% In RFC 1071 efficient algorithms are discussed for computing the internet
% checksum, also known as IP checksum. Whenever you implement efficient
% algorithms, an error may sneak through.

% This article is meant to be used as test driven development specification for
% anyone that wants to implement one of the algorithms of RFC 1071 or even a new
% one to compute the IP checksum. The article can also be read as an example of
% specifying something without revealing its implementation details; a good
% example of using QuickCheck specifications.

% Whether you write your code in Erlang, C or Java, we assume that you can build
% an interface to a module called ip_checksum.erl in which Erlang functions
% either are the functions under test or call the functions under test.

% === IP Checksum ===

% The IP checksum is the 16 bit one's complement of the one's complement sum of
% all 16 bit words in the header.

% Ones complement is a way of representing negative numbers (see
% [http://en.wikipedia.org/wiki/Signed_number_representations#Ones.27_complement
% WikiPedia] for more details).

% The IP checksum uses 16 bit words. In 16 bits you can represent the numbers 0
% to 65535. The idea with ones complement is to use half the numbers in this
% interval for representing negative numbers.  Thus, 0 up to 32767 are the
% positive numbers and 65535 is -0, or an alternative representation of zero.
% The number 65534 is -1 etc. Until 32768 which is -32767. Hence the interval
% -32767 up to 32767 can be represented.

% In the remainder of this article we will present properties for functions that
% you probably would like to test. The properties are always parametrized by the
% word size.

% === Utility functions ===

% First we define some functions that will come in handy.

% The maximum number that can be represented in ''N'' bits. In the ones
% complement interpretation this will be the negative zero.
max_int(N) ->
  (1 bsl N) - 1.

negative_zero(N) ->
  max_int(N).

% === Ones complement ===

% The first function we might want to check is the ones' complement of a word,
% which in ones' complement representation corresponds to the negation.  We
% assume we have a function '''ip_checksum:negate/1''' implemented that takes a
% bit string as input and computes its ones' complement.

% Looking at the specification of ones' complement representation above we can
% see that adding the ones' complement representation of a number and the
% representation of its negation results in the representation of -0. For
% instance, the representation of -2 is 65533 and the representation of 2 is 2.
% Adding these we get 65535 which is the representation of -0. We use this
% property to test the implementation of the '''negate/2''' function.

prop_negate(N) ->
  ?FORALL(I, choose(0, max_int(N)),
  begin
    <<CI:N>> = ip_checksum:negate(<<I:N>>),
    equals(negative_zero(N), I + CI)
  end).

% The property above is parameterized by the word size N. We'll want to test
% our properties for a range of different word sizes, so we define a general
% function to transform a parameterized property to a property choosing random
% word sizes.

random_word_size(Prop) ->
  ?FORALL(N, choose(1, 64), Prop(N)).

prop_negate() ->
  random_word_size(fun prop_negate/1).

% === Padding ===

% It is not clear from the specification presented above, but if you need to
% compute the checksum of a list of bytes in base 16, then there should be an
% even number of bytes. Likewise, if we would like to do ones complement in 32
% bits base, we would need to extend a sequence of bytes such that it is
% divisible by 4.

% Extending a bit string such that it is divisible by the base is called padding.
% We assume that you implemented a padding function that added the necessary
% bits, given a bit string. We assume this function to be implemented as
% '''ip_checksum:pad/1''' taking a bit string as argument and returning a new
% bit string which is an extended version with as many zero bits as needed.

prop_padding() ->
  random_word_size(fun prop_padding/1).

prop_padding(N) ->
  ?FORALL(BitString, bitstring(),
  begin
    Bits = bit_size(BitString),
    <<B:Bits/bits, Padded/bits>> = ip_checksum:pad(BitString,N),
    Zeros = bit_size(Padded),
    % If this property fails we need to know what the pad function actually
    % returned in order to understand what went wrong. This is what the
    % ?WHENFAIL macro is for.
    ?WHENFAIL(io:format("B      = ~w\nPadded = ~w\nZeros  = ~w\n",
                        [B, Padded, Zeros]),
      ((Bits + Zeros) rem N) == 0 andalso   % the new length is divisible by N
          B == BitString          andalso   % we don't change the bit string
          <<0:Zeros>> == Padded   andalso   % we pad with zeros
          Zeros < N                         % we don't pad more than we have to
    )
  end).

% Confident that the padding function works we can write a generator for
% correctly padded bit strings.
padded_bitstring(N) ->
  ?LET(Bits, bitstring(), ip_checksum:pad(Bits, N)).

% An alternative definition of this generator would not use the padding
% function, but rather first generate the length of the bit string and then
% pass that to the bit string generator (see below). The advantage of the former
% definition is that it behaves better when shrinking. The version below will
% regenerate the bit string everytime the length is getting shorter.
padded_bitstring_2(N) ->
  ?LET(Len, nat(), bitstring(N * Len)).

% === Ones complement sum ===

% The ones complement sum is computed by adding a number of words in ones
% complement representation. We assume this function to be implemented as
% '''ip_checksum:sum/2''' which takes a bit string as first argument and a
% word size as second argument. We assume that padding is done outside the
% sum function and only test that the function works for bit strings of
% which the length is divisible by the given word size.

% Because of our test driven development method, we have already tested the
% '''negate/1''' function and therefore trust this function in our property.
% Remember that adding the representations of a number and its negation yields
% -0. This is in fact also true if we use one's complement addition rather than
% simply adding the representations (which is not the same thing). So if we
% concatenate a bit string with the negation of its sum, the sum of the
% resulting bit string should be -0 if our implementation of '''sum/2''' is
% correct. By testing the sum function in this way we don't have to worry about
% specifying the intricacies of ones' complement arithmetic (except for the
% fact that X + (-X) = -0).

prop_sum() -> 
  random_word_size(fun prop_sum/1).

prop_sum(N) ->
  ?FORALL(Bin, padded_bitstring(N),
  begin
    Sum  = ip_checksum:sum(Bin, N),
    CSum = ip_checksum:negate(Sum),
    equals(ip_checksum:sum(<<CSum/bits, Bin/bits>>, N),
           <<(negative_zero(N)):N>>)
  end).

% === Checksum ===

% After computing ones' complement sum, one has to take the ones' complement of
% the result to compute the checksum. Of course, we have all ingredients in
% house to do so, but in case you implement both functions as one you would
% like to test the final result '''ip_checksum:checksum/2''' with a bit string
% and word size as arguments.

% We first test that the '''checksum/2''' function takes care of padding, by
% checking that padding the bitstring before passing it to '''checksum/2'''
% doesn't change the result.

prop_checksum_pad() ->
  random_word_size(fun prop_checksum_pad/1).

prop_checksum_pad(N) ->
  ?FORALL(Bits, bitstring(),
    equals(ip_checksum:checksum(Bits, N),
           ip_checksum:checksum(ip_checksum:pad(Bits, N), N))).

% We can test the '''checksum/2''' function in the same way as we tested
% '''sum/2''' above. Taking a bit string and prepending its checksum should
% result in a bit string whose checksum is zero. Here's why:
%
%   % definition of checksum
%   checksum(Bits) == -sum(Bits)
%
%   checksum(checksum(Bits) ++ Bits) == {def. of checksum}
%   -sum(-sum(Bits) ++ Bits)         == {sum(Xs ++ Ys) == sum(Xs) + sum(Ys)}
%   -(-sum(Bits) + sum(Bits))        == {adding -X and X}
%   -(-0)                            ==
%   0
%
% Note that due to padding, the property doesn't hold if we append the checksum
% rather than prepending it.

prop_checksum() ->
  random_word_size(fun prop_checksum/1).

prop_checksum(N) ->
  ?FORALL(Bin, bitstring(),
  begin
    Sum = ip_checksum:checksum(Bin, N),
    equals(ip_checksum:checksum(<<Sum/bits, Bin/bits>>, N),
           <<0:N>>)
  end).

