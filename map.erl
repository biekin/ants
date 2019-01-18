-module(map).
-export([start/0]).

-import(ant, [start/1]).
-import(window, [startWindow/3]).
-import(pk, [startPheromoneKiller/1]).
-import(hole, [startHole/2]).
-include_lib("wx/include/wx.hrl").

-define(MapHeigth, 500).
-define(MapWidth, 500).

start() ->
    Map = ets:new(map, [public, {read_concurrency, true}, {write_concurrency, true}, bag]),
    ets:insert(Map, {{250, 250}, hole}),
    ets:insert(Map, {{250, 250}, pheromone, 20000000000000000}),
    ets:insert(Map, {{251, 250}, pheromone, 10000000000}),
    ets:insert(Map, {{250, 251}, pheromone, 10000000000}),
    ets:insert(Map, {{251, 251}, pheromone, 10000000000}),
    ets:insert(Map, {{250, 249}, pheromone, 10000000000}),
    ets:insert(Map, {{249, 250}, pheromone, 10000000000}),
    ets:insert(Map, {{249, 249}, pheromone, 10000000000}),
    for(30, fun() -> addRandomFood(Map) end),
    %Map = maps:from_list([{{1, 1}, [hole]}, {{19, 19}, [food]}]),
    init(Map).
    %Map.

init(Map) ->
    MapPid = spawn(fun() -> mapLoop(Map) end),
    PK = pk:startPheromoneKiller(Map),
    HolePid = hole:startHole(200, fun() -> ant:start(MapPid, PK) end),
    window:startWindow(Map, MapPid, HolePid).

for(0,_) -> ok; 
for(N,Fun) when N > 0 -> 
   Fun(),
   timer:sleep(50),
   for(N-1, Fun). 

mapLoop(Map) ->
    receive
        die -> 
            lists:foreach(fun([Pid]) -> Pid ! die end, ets:match(Map, {'_', ant, '$1'}))
        after 0 ->
            receive
                stopkataklizm ->
                        lists:foreach(fun([Pid]) -> Pid ! stopkataklizm end, ets:match(Map, {'_', ant, '$1'}));
                kataklizm ->
                    lists:foreach(fun([Pid]) -> Pid ! kataklizm end, ets:match(Map, {'_', ant, '$1'}));
                {pheromoneNear, {X, Y}, Direction, AntPid} ->
                    PheromoneCoords = lists:flatmap(fun(Coord = {X, Y}) -> ets:match_object(Map, {Coord, pheromone, '_'}) end, [{X+1, Y+1}, {X, Y+1}, {X+1, Y}, {X-1, Y+1}, {X+1, Y-1}, {X-1, Y-1}, {X-1, Y}, {X, Y-1}]),
                    AntPid ! {pheromone, PheromoneCoords};
                {foodNear, {X, Y} = Coords, AntPid} -> 
                    FoodCoords = ets:match(Map, {Coords, food}),
                    AntPid ! {food, FoodCoords};
                {sendState, Pid} ->
                    Pid ! {state, Map},
                    Map
            end,
            mapLoop(Map)
    end.

%%-----------------------------------------
%% Helper functions
%%-----------------------------------------
getDir({X, Y}, n) -> [{X-1, Y+1}, {X, Y+1}, {X+1, Y+1}];
getDir({X, Y}, ne) -> [{X, Y+1}, {X+1, Y+1}, {X+1, Y}];
getDir({X, Y}, e) -> [{X+1, Y+1}, {X+1, Y}, {X+1, Y-1}];
getDir({X, Y}, se) -> [{X+1, Y}, {X+1, Y-1}, {X, Y-1}];
getDir({X, Y}, s) -> [{X+1, Y-1}, {X, Y-1}, {X-1, Y-1}];
getDir({X, Y}, sw) -> [{X, Y-1}, {X-1, Y-1}, {X-1, Y}];
getDir({X, Y}, w) -> [{X-1, Y-1}, {X-1, Y}, {X-1, Y+1}];
getDir({X, Y}, nw) -> [{X-1, Y}, {X-1, Y+1}, {X, Y+1}].


addFood(X, Y, Map) ->
    ets:insert(Map, {{X, Y+1}, food}),
    ets:insert(Map, {{X, Y+2}, food}),
    ets:insert(Map, {{X, Y+3}, food}),
    ets:insert(Map, {{X, Y+4}, food}),
    ets:insert(Map, {{X, Y+5}, food}),
    ets:insert(Map, {{X, Y+6}, food}),
    ets:insert(Map, {{X+1, Y+1}, food}),
    ets:insert(Map, {{X+2, Y+2}, food}),
    ets:insert(Map, {{X+3, Y+3}, food}),
    ets:insert(Map, {{X+4, Y+4}, food}),
    ets:insert(Map, {{X+5, Y+5}, food}),
    ets:insert(Map, {{X+6, Y+6}, food}).

addRandomFood(Map) ->
    addFood(rand:uniform(400) + 50, rand:uniform(400) + 50, Map).