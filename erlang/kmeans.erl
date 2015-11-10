-module(kmeans).
-compile([export_all]).

kmeans_loop(_, Centroids, _, 0, Print) ->
    if
        Print =:= true ->
            io:format("Final Centroids ->~n"),
            [io:format("Centroid -> ~p~n", [C]) || C <- Centroids];
        true ->
            true
    end,
    controller ! done;
    
kmeans_loop(Xs, Centroids, N, Iters, Print) ->
    CentActorsPID = [spawn(kmeans, cent_loop, [[]]) || _ <- lists:seq(1, N)],
    PointActorsPID = [spawn(kmeans, point_loop, [X, CentActorsPID]) || X <- Xs],
    [P ! {closest, Centroids} || P  <- PointActorsPID],
    kmeans_run(CentActorsPID, Xs, N, Iters, 1, Print).

kmeans_run(CentActorsPID, Xs, N, Iters, Ps, Print) ->
    if
        Ps =:= length(Xs) ->
            [C ! sum || C <- CentActorsPID],
            kmeans_end(Xs, N, Iters, [], Print);
        true ->
            receive
                increment ->
                    kmeans_run(CentActorsPID, Xs, N, Iters, Ps+1, Print)
            end
    end.

kmeans_end(Xs, N, Iters, NewCentroids, Print) ->
    if
        length(NewCentroids) =:= N ->
            kmeans_loop(Xs, NewCentroids, N, Iters-1, Print);
        true ->
            receive
                {centroid, P} ->
                    kmeans_end(Xs, N, Iters, [P | NewCentroids], Print)
            end
    end.

cent_loop(ToAdd) ->
    receive
        {add, Point} ->
            cent_loop([Point | ToAdd]);
        sum ->
            kmeansp ! {centroid, average(ToAdd)},
            cent_loop([])
    end.

point_loop(Point, CentActorsPID) ->
    receive
        {closest, Centroids} ->
            lists:nth(closest(Point, Centroids), CentActorsPID) ! {add, Point},
            kmeansp ! increment
    end,
    point_loop(Point, CentActorsPID).

divide({Px,Py}, K) ->
    {Px/K, Py/K}.

add({Px1, Py1}, {Px2, Py2}) ->
    {(Px1+Px2), (Py1+Py2)}.

sub({Px1, Py1}, {Px2, Py2}) ->
    {(Px1-Px2), (Py1-Py2)}.

sq(X) ->
    X*X.

modulus({Px, Py}) ->
    math:sqrt((sq(Px) + sq(Py))).

dist(P1, P2) ->
    modulus(sub(P1,P2)).

average([]) -> {0, 0};
average(Q) ->
    divide(sum(Q),length(Q)).

sum(L) ->
    Add = fun(X, Acc) -> add(X, Acc) end,
    lists:foldl(Add, {0.0, 0.0}, L).

indexit(Q) -> indexit(Q, 1).
indexit([], _) -> [];
indexit([H | T], N) ->
    [{H, N} | indexit(T, N+1)].

closest(P, Centroids) ->
    element(3, lists:min([{dist(P, C), C, N} || {C, N} <- indexit(Centroids)])).

start(Xs, N, Iters, Print, Iterations) ->
    InitCentroids = lists:sublist(Xs, N),
    register(controller,
        spawn(kmeans, controller_loop, [Xs, InitCentroids, N, Iters, Print, Iterations])).

controller_loop(Xs, InitCentroids, N, Iters, Print, Iterations) ->
    StartTime = now_ms(),
    controller_loop(StartTime, Xs, InitCentroids, N, Iters, Print, Iterations, Iterations).

controller_loop(StartTime, Xs, InitCentroids, N, Iters, Print, Iterations, TotIterations) ->
    if
        Iterations > 0 ->
            register(kmeansp, spawn(kmeans, kmeans_loop, [Xs, InitCentroids, N, Iters, Print])),
            receive
                done -> controller_loop(StartTime, Xs, InitCentroids, N, Iters, Print, Iterations-1, TotIterations)
            end;
        true ->
            Time = (now_ms() - StartTime) / TotIterations,
            io:format("Average time is -> ~p~n", [Time])
    end.

now_ms() ->
    {MegaSecs,Secs,MicroSecs} = erlang:now(),
    ((MegaSecs*1000000 + Secs)*1000000 + MicroSecs) / 1000.    

