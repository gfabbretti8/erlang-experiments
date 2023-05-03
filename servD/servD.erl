-module(servD).

-export([init/1, router/0, backend/0, interface/0]).


init(router) ->
  erlang:register(router, erlang:spawn(?MODULE, router, []));
init(backend) ->
  erlang:register(backend, erlang:spawn(?MODULE, backend, [])).

interface() ->
  io:format("Initiating request~n"),
  erlang:register(interface, self()),
  spawn('app@router.com', fun () -> erlang:send(erlang:whereis(router), r1) end),
  receive
    res -> io:format("Response received~n", [])
  end.

router() ->
  receive
    r1 ->
      spawn('app@backend.com', fun () -> erlang:send(erlang:whereis(backend), req) end),
      router();
    r2 ->
      spawn('app@interface.com', fun () -> erlang:send(erlang:whereis(interface), res) end),
      router()
  end.

backend() ->
  receive
    req ->
      spawn('app@router.com', fun () -> erlang:send(erlang:whereis(router), r2) end),
      backend()
  end.

