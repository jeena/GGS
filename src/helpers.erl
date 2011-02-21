-module(helpers).
-export([not_implemented/0, get_new_token/0]).

not_implemented() ->
    exit("Not implemented").

get_new_token() ->
    string:strip(os:cmd("uuidgen"), right, $\n ).
