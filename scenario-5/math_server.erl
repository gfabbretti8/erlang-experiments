-module('math_server').

-export([start/0, sums/0]).

start() ->
  global:register_name(sum, erlang:spawn(?MODULE, sums, [])).

sums() ->
  receive {X, Y, P}
    -> P ! X + Y
  end.
