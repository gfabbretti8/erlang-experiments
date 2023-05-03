-module(servDF).

-export([init/1, router/0, router_bug/0, backend/0, interface/0, controller/0]).


init(router) ->
  erlang:register(router, erlang:spawn(?MODULE, router, []));
init(router_bug) ->
  erlang:register(router_bug, erlang:spawn(?MODULE, router_bug, []));
init(backend) ->
  erlang:register(backend, erlang:spawn(?MODULE, backend, [])).

interface() ->
  erlang:register(interface, self()),
  spawn('app@router.com', fun () -> erlang:send(erlang:whereis(router_bug), r1) end),
  receive
    req -> io:format("Response received~n", []);
    retry -> spawn('app@router.com', fun () -> erlang:send(erlang:whereis(router), r1) end),
             receive
               req -> io:format("Response received~n", [])
             end
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

router_bug() ->
  receive
    r1 -> ok
  end.

backend() ->
  receive
    req ->
      spawn('app@router.com', fun () -> erlang:send(erlang:whereis(router), r2) end),
      backend()
  end.

controller() ->
  spawn('app@router.com', ?MODULE, init, [router]),
  spawn('app@interface.com', fun () -> erlang:send(erlang:whereis(interface), retry) end).
