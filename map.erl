-module(map).
-export([start/0]).

-import(ant, [start/1]).
-import(window, [startWindow/1]).
-include_lib("wx/include/wx.hrl").

-define(MapHeigth, 800).
-define(MapWidth, 600).

start() ->
    Map = ets:new(map, [public, {read_concurrency, true}, {write_concurrency, true}]),
    %Map = maps:from_list([{{1, 1}, [hole]}, {{19, 19}, [food]}]),
    init(Map),
    Map.

init(Map) ->
    MapPid = spawn(fun() -> mapLoop(Map) end),
    window:startWindow(Map),
    for(200, fun() -> ant:start(MapPid) end).

for(0,_) -> ok; 
for(N,Fun) when N > 0 -> 
   Fun(),
   timer:sleep(100),
   for(N-1, Fun). 

mapLoop(Map) ->
    receive
        %{nearPheromone, {X, Y} = Coords, AntPid} -> 0;
        %{isFoodHere, {X, Y} = Coords, AntPid} -> 0;
        %{nearHole, {X, Y} = Coords, AntPid} -> 0;
        {sendState, Pid} ->
            Pid ! {state, Map},
            Map;
        {newAnt, {X, Y} = Coords, AntPid} ->
            %io:format("~p - ~p \n", [Coords, AntPid]),
            addElementInMap(Coords, {ant, AntPid}, Map);
        {positionUpdate, {X, Y} = Coords, {NX, NY} = NewCoords, AntPid} ->
            %io:format("~p - ~p \n", [Coords, NewCoords]),
            moveElementInMap(Coords, NewCoords, {ant, AntPid}, Map)
    end,
    mapLoop(Map).

%%-----------------------------------------
%% Helper functions
%%-----------------------------------------
moveElementInMap(Key, NewKey, Elem, Map) ->
    [{Key, OldKeyList}] = ets:lookup(Map, Key),
    NewKeyList = lists:delete(Elem, OldKeyList),
    OldNewKeyList = ets:lookup(Map, NewKey),
    case OldNewKeyList of
        [] -> ets:insert(Map, {NewKey, [Elem]});
        [{NewKey, List}] -> ets:update_element(Map, NewKey, {2, [Elem | List]})
    end,
    ets:update_element(Map, Key, {2, NewKeyList}).
    

addElementInMap(Key, Elem, Map) ->
    List = ets:lookup(Map, Key),
    case List of
        [] -> ets:insert_new(Map, {Key, [Elem]});
        [{Key, L}] -> ets:update_element(Map, Key, {2, [Elem | L]})        
    end.