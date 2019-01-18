-module(pk).
-export([startPheromoneKiller/1]).

startPheromoneKiller(Map) ->
    spawn(fun() -> pheromoneKillerLoop(Map, 1000) end).

pheromoneKillerLoop(Map, Counter) ->
    case Counter of
        0 ->
            PheromoneCoords = ets:match(Map, {'$1', pheromone, '$2'}),
            lists:foreach(fun([Coords, Power])-> pheromoneDecrease(Coords, Power, Map) end, PheromoneCoords), 
            NewCounter = 1000;
        N ->
            receive
                {newAnt, {X, Y} = Coords, AntPid} ->
                    %io:format("~p - ~p \n", [Coords, AntPid]),
                    addElementInMap(Coords, AntPid, Map);
                {positionUpdate, {X, Y} = Coords, {NX, NY} = NewCoords, AntPid, State} ->
                    %io:format("~p - ~p \n", [Coords, NewCoords]),
                    moveElementInMap(Coords, NewCoords, AntPid, Map, State);
                {died, Coords, AntPid} ->
                        deleteAntInMap(Coords, AntPid, Map)
            end,
            NewCounter = N - 1
    end,
    pheromoneKillerLoop(Map, NewCounter).
pheromoneDecrease({250,250},_,_) -> ok;
pheromoneDecrease({250,251},_,_) -> ok;
pheromoneDecrease({251,250},_,_) -> ok;
pheromoneDecrease({251,251},_,_) -> ok;
pheromoneDecrease({249,250},_,_) -> ok;
pheromoneDecrease({250,249},_,_) -> ok;
pheromoneDecrease({249,249},_,_) -> ok;
pheromoneDecrease({249,251},_,_) -> ok;
pheromoneDecrease({251,249},_,_) -> ok;
pheromoneDecrease(Coords, 1, Map) ->
    ets:delete_object(Map, {Coords, pheromone, 1});
pheromoneDecrease(Coords, Power, Map) ->
    ets:delete_object(Map, {Coords, pheromone, Power}),
    ets:insert(Map, {Coords, pheromone, Power - 1}).

moveElementInMap(Key, NewKey, Elem, Map, 2) ->
    ets:delete_object(Map, {Key, ant, Elem}),
    ets:insert(Map, {NewKey, ant, Elem}),
    case ets:match(Map, {Key, pheromone, '$1'}) of
        [] -> 
            ets:insert(Map, {Key, pheromone, 5});
        [[Power]] ->
            ets:delete_object(Map, {Key, pheromone, Power}),
            ets:insert(Map, {Key, pheromone, Power + 5})
    end;
moveElementInMap(Key, NewKey, Elem, Map, _) ->
        ets:delete_object(Map, {Key, ant, Elem}),
        ets:insert(Map, {NewKey, ant, Elem}).
    

addElementInMap(Key, Elem, Map) ->
    ets:insert(Map, {Key, ant, Elem}).

deleteAntInMap(Key, Pid, Map) ->
    ets:delete_object(Map, {Key, ant, Pid}).
