%%% File    : ip_checksum.erl
%%% Author  : Ulf Norell <ulf.norell@quviq.com>,
%%%           Thomas Arts <thomas.arts@quviq.com>
%%% Description : Implementation of IP checksums.
%%% Created : 7 Jun 2010 by Ulf Norell
-module(ip_checksum).

-export([checksum/1, checksum/2, sum/2, pad/2, add/2, negate/1]).

checksum(Bin) ->
  checksum(Bin, 16).

checksum(Bin, N) ->
  negate(sum(pad(Bin, N), N)).

% Sum a binary of N bit words in ones complement representation.
sum(Bin, N) ->
  lists:foldl(fun(A, B) -> add(A, B) end, <<0:N>>,
              [ <<A:N>> || <<A:N>> <= Bin ]).

% Add two numbers in ones complement representation.
add(A, B) ->
  N = bit_size(A),
  <<X:N>> = A,
  <<Y:N>> = B,
  Carry = (X + Y) div (1 bsl N),
  <<(X + Y + Carry):N>>.

%% invert all bits... as simple as that.
negate(BitString) ->
    << <<(1-Bit):1>> || <<Bit:1>> <= BitString >>.

pad(Binary, Bits) ->
    PaddingLength = 
	case bit_size(Binary) rem Bits of
	    0 -> 0;
	    N -> Bits - N
	end,
    <<Binary/bits, 0:PaddingLength>>.

