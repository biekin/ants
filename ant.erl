%% -*- coding: utf-8 -*-
-module(ant).
-export([start/1]).

-vsn(1.0).
% wersja

%% state -> stan mrówki 0-dopiero urodzona, 1-bez jedzonka, 2-z jedzonkiem

-define(MapHeigth, 800).
-define(MapWidth, 600).
-define(Directions, [n, ne, e, se, s, ws, w, nw]).

start(Map) ->
	spawn(fun() -> antLoop({0, {200,200}, 20, n}, Map) end).


antLoop({State, {_X, _Y}=Coords, Counter, Direction}, Map) ->
	case State of
		0 ->
            Map ! {newAnt, Coords, self()},
			Index = rand:uniform(length(?Directions) - 1) + 1,
            NewDirection = lists:nth(Index,?Directions),
            NewState = 1,
            NewCounter = Counter - 1;
        1 ->
            case Counter of
                0 ->
                    NewCounter = rand:uniform(30),
                    NewDirection = 
                        case rand:uniform(100) rem 2 of
                            1 -> getPrevCyclic(?Directions, Direction);
                            0 -> getNextCyclic(?Directions, Direction)
                        end,
                    NewState = 1;
                _ ->
                    NewCounter = Counter - 1,
                    NewDirection = Direction,
                    NewState = 1 % In next step, ask about food after coord update and then change state accordingly
            end
    end,
    %io:format("~p \n",[NewDirection]),
    NewCoords = updateCoords(Coords, NewDirection),
    %Update position
    Map ! {positionUpdate, Coords, NewCoords, self()},
    timer:sleep(120),
    antLoop({NewState, NewCoords, NewCounter, NewDirection}, Map).


%%-----------------------------------------
%% Coordinates update according to direction
%%-----------------------------------------

updateCoords({X,?MapHeigth - 5},n) -> {X, ?MapHeigth -6};
updateCoords({X,Y},n) -> {X, Y+1};
updateCoords({?MapWidth -5,Y},e) -> {?MapWidth -6, Y};
updateCoords({X,Y},e) -> {X+1, Y};
updateCoords({X,5},s) -> {X, 6};
updateCoords({X,Y},s) -> {X, Y-1};
updateCoords({5,Y},w) -> {6, Y};
updateCoords({X,Y},w) -> {X-1, Y};

updateCoords({_X,_Y} = Coords,ne) -> updateCoords(updateCoords(Coords, e), n);
updateCoords({_X,_Y} = Coords,se) -> updateCoords(updateCoords(Coords, e), s);
updateCoords({_X,_Y} = Coords,ws) -> updateCoords(updateCoords(Coords, s), w);
updateCoords({_X,_Y} = Coords,nw) -> updateCoords(updateCoords(Coords, w), n).

%%-----------------------------------------
%% Helper functions
%%-----------------------------------------
getPrevCyclic(List, Elem) ->
    getNextCyclic(lists:reverse(List), Elem).

getNextCyclic(List, Elem) ->
    case findNext(List, Elem) of
        false -> lists:nth(1, List);
        A -> A
    end.

findNext([Elem | T], Elem) ->
    case T of
        [A | _] -> A;
        [] -> false
    end;
findNext([_ | T], Elem) ->
    findNext(T, Elem).        