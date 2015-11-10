-module(main).
-compile([export_all]).

iterations() -> 3.

n() -> 10.
iters() -> 15.

run() ->
  StartTime = kmeans:now_ms(),
  timer:apply_after(5000, main, endit, [StartTime]).
  %Xs = read_points("../points.json"),
  %kmeans:start(Xs, n(), iters(), true, iterations()),
  %io:format("Starting~n", []).

endit(Start) ->
  End = kmeans:now_ms(),
  io:format("End time ~p~n", [(End - Start)]).


read_points(FileName) ->
    {ok, Data} = file:read_file(FileName),
    [<<>>|Bins] = re:split(Data, "[][,]+", [trim]),
    points([binary_to_float(X) || X <- Bins]).

points([]) -> [];
points([H1, H2 | T]) ->
  [{H1, H2} | points(T)].
