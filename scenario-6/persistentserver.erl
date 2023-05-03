-module(persistentserver).

-export([init_db/0, database/0, init_st/0, storage/1]).


init_db() ->
  net_adm:ping('storage@l2.com'),
  global:register_name(database, spawn(?MODULE, database, [])).

database() ->
  receive
    {read, Pid, X} ->
      global:send(storage, {get, self()}),
        receive
          M ->
            Pid ! maps:get(X, M, 'not_found')
        end,
      database();
    {remove, X} ->
      global:send(storage, {remove, X}),
      database();
    {add, X, Y} ->
      global:send(storage, {add, X, Y}),
      database()
  end.

init_st() ->
  global:register_name(storage, spawn(?MODULE, storage, [#{}]))
    .

storage(M) ->
  receive
    {add, X, Y} ->
      storage(maps:put(X, Y, M));
    {remove, X} ->
      storage(maps:remove(X, M));
    {get, P} ->
      P ! M,
      storage(M)
  end.
