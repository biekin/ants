-module(map).
-export([start/0]).

-import(ant, [start/1]).
-import(window, [startWindow/1]).
-include_lib("wx/include/wx.hrl").

-define(MapHeigth, 800).
-define(MapWidth, 600).

start() ->
    Map = maps:from_list([{{1, 1}, [hole]}, {{19, 19}, [food]}]),
    init(Map).

init(Map) ->
    MapPid = spawn(fun() -> mapLoop(Map) end),
    window:startWindow(MapPid),
    for(200, fun() -> ant:start(MapPid) end).

for(0,_) -> ok; 
for(N,Fun) when N > 0 -> 
   Fun(),
   timer:sleep(50),
   for(N-1, Fun). 

mapLoop(Map) ->
    receive
        %{nearPheromone, {X, Y} = Coords, AntPid} -> 0;
        %{isFoodHere, {X, Y} = Coords, AntPid} -> 0;
        %{nearHole, {X, Y} = Coords, AntPid} -> 0;
        {sendState, Pid} ->
            Pid ! {state, Map},
            NewMap = Map;
        {newAnt, {X, Y} = Coords, AntPid} ->
            %io:format("~p - ~p \n", [Coords, AntPid]),
            NewMap = addElementInMap(Coords, {ant, AntPid}, Map);
        {positionUpdate, {X, Y} = Coords, {NX, NY} = NewCoords, AntPid} ->
            %io:format("~p - ~p \n", [Coords, NewCoords]),
            NewMap = moveElementInMap(Coords, NewCoords, {ant, AntPid}, Map)
    end,
    mapLoop(NewMap).

%%-----------------------------------------
%% Helper functions
%%-----------------------------------------
moveElementInMap(Key, NewKey, Elem, Map) ->
    OldKeyList = maps:get(Key, Map, []),
    NewKeyList = lists:delete(Elem, OldKeyList),
    OldNewKeyList = maps:get(NewKey, Map, []),
    NewNewKeyList = [Elem | OldNewKeyList],
    maps:put(NewKey, NewNewKeyList ,maps:put(Key, NewKeyList, Map)).

addElementInMap(Key, Elem, Map) ->
    OldList = maps:get(Key, Map, []),
    NewList = [Elem | OldList],
    maps:put(Key, NewList, Map).