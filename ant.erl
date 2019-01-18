%% -*- coding: utf-8 -*-
-module(ant).
-export([start/2]).

-vsn(1.0).
% wersja

%% state -> stan mrówki 0-dopiero urodzona, 1-bez jedzonka, 2-z jedzonkiem

-define(MapHeigth, 500).
-define(MapWidth, 500).
-define(Directions, [n, ne, e, se, s, sw, w, nw]).

start(Map, PK) ->
	spawn(fun() -> antLoop({80, 0, {250,250}, 20, n}, Map, PK) end).


antLoop({Delay, State, {X, Y}=Coords, Counter, Direction}, Map, PK) ->
	case State of
		0 ->
            PK ! {newAnt, Coords, self()},
			Index = rand:uniform(length(?Directions) - 1) + 1,
            NewDirection = lists:nth(Index,?Directions),
            ProbState = 1,
            NewCounter = Counter - 1,
            NewCoords = updateCoords(Coords, NewDirection);
        1 ->
            Map ! {foodNear, Coords, self()},
            FoodCoords = receive
                {food, Food} -> Food
            end,
            case FoodCoords of
                [] -> 
                    case Counter of
                        0 ->
                            NewCounter = rand:uniform(300) rem 30,
                            NewDirection = getNewDirection(Direction),
                            ProbState = 1;
                        _ ->
                            NewCounter = Counter - 1,
                            NewDirection = Direction,
                            ProbState = 1 % In next step, ask about food after coord update and then change state accordingly
                    end;
                _ ->
                    timer:sleep(2000),
                    ProbState = 2,
                    NewDirection = getOppositeDirection(Direction),
                    NewCounter = 15 + rand:uniform(300) rem 30
            end,
            NewCoords = updateCoords(Coords, NewDirection);
        2 ->
            Map ! {pheromoneNear, Coords, Direction, self()},
            PheromoneCoords = receive
                {pheromone, Pheromone} -> Pheromone
            end,
            if 
                abs(X - 250) =< 10  , abs(Y - 250) =< 10 ->
                    NewDirection = getHoleDirection(X, Y),
                    case Counter of
                            0 ->
                                NewCounter = rand:uniform(300) rem 30;
                            _ ->
                                NewCounter = Counter - 1
                    end,
                    NewCoords = updateCoords(Coords, NewDirection);
                PheromoneCoords == [] ->
                    case Counter of
                        0 ->
                            NewCounter = rand:uniform(300) rem 30,
                            ProbDirection = getNewDirection(Direction);
                        _ ->
                            NewCounter = Counter - 1,
                            ProbDirection = Direction
                    end,
                    NewCoords = updateCoords(Coords, ProbDirection),
                    
                    case isCloserToHole(Coords, NewCoords) of 
                        false ->
                            NewDirection = getNewDirection(Direction);
                        _ -> 
                            NewDirection = ProbDirection
                    end;
                true -> 
                    ProbCoords = getMaxPheromone(PheromoneCoords),
                    ProbDirection = Direction,
                    ProbCounter = Counter - 1,
                    case isCloserToHole(Coords, ProbCoords) and (ProbCounter > 0) of 
                        false ->
                            NewDirection = getHoleDirection(X, Y),
                            NewCoords = updateCoords(Coords, NewDirection),
                            NewCounter = 10;
                        _ -> 
                            NewDirection = ProbDirection,
                            NewCoords = ProbCoords,
                            NewCounter = ProbCounter
                    end
            end,
            ProbState = 2
    end,
    %io:format("~p \n",[NewDirection]),
    %Update position
    receive 
        kataklizm ->
            NewDelay = 50,
            NewState = 2
        after 0 ->
            NewDelay = Delay,
            NewState = ProbState
    end,
    receive 
        die -> ok
        after Delay ->
            {NX, NY} = NewCoords,
            if  
                State == 2 , abs(NX - 250) =< 0  , abs(NY - 250) =< 0 -> 
                    PK ! {positionUpdate, Coords, NewCoords, self(), State},
                    PK ! {died, NewCoords, self()};
                true -> 
                    PK ! {positionUpdate, Coords, NewCoords, self(), State},
                    antLoop({NewDelay, NewState, NewCoords, NewCounter, NewDirection}, Map, PK)
            end
    end.


getNewDirection(Direction) ->
    case rand:uniform(100) rem 2 of
        1 -> getPrevCyclic(?Directions, Direction);
        0 -> getNextCyclic(?Directions, Direction)
    end.

%%-----------------------------------------
%% Coordinates update according to direction
%%-----------------------------------------

updateCoords({X, Y},n) when Y >= ?MapHeigth - 5 -> {X, ?MapHeigth -6};
%updateCoords({250,Y},n) when Y >= 246, Y =< 249 -> {250, Y-1};
updateCoords({X,Y},n) -> {X, Y+1};
updateCoords({X,Y},e) when X >= ?MapWidth -5 -> {?MapWidth -6, Y};
%updateCoords({X,250},e) when X >= 246, X =< 249 -> {X-1, 250};
updateCoords({X,Y},e) -> {X+1, Y};
updateCoords({X,5},s) -> {X, 6};
%updateCoords({250,Y},s) when Y >= 251, Y =< 254 -> {250, Y+1};
updateCoords({X,Y},s) -> {X, Y-1};
updateCoords({5,Y},w) -> {6, Y};
%updateCoords({X,250},w) when X >= 251, X =< 254 -> {X+1, 250};
updateCoords({X,Y},w) -> {X-1, Y};

updateCoords({_X,_Y} = Coords,ne) -> updateCoords(updateCoords(Coords, e), n);
updateCoords({_X,_Y} = Coords,se) -> updateCoords(updateCoords(Coords, e), s);
updateCoords({_X,_Y} = Coords,sw) -> updateCoords(updateCoords(Coords, s), w);
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

getOppositeDirection(n) -> s;
getOppositeDirection(s) -> n;
getOppositeDirection(w) -> e;
getOppositeDirection(e) -> w;
getOppositeDirection(ne) -> sw;
getOppositeDirection(nw) -> se;
getOppositeDirection(se) -> nw;
getOppositeDirection(sw) -> ne.

isCloserToHole({X1, Y1}, {X2, Y2}) when (abs(250-X2) > abs(250-X1)) or (abs(250-Y2) > abs(250-Y1)) -> false;
isCloserToHole({X1, X2}, {Y1, Y2}) -> true.

getMaxPheromone(List) ->
    {{X, Y}, pheromone, P1} = lists:foldl(fun(A, B) -> comparePheromones(A, B) end, {{500, 500}, pheromone, -1}, List),
    {X, Y}.

comparePheromones({{X, Y}, pheromone, P1}, {{XB, YB}, pheromone, P2}) ->
    case isCloserToHole({XB, YB}, {X, Y}) of
        true -> {{X, Y}, pheromone, P1};                
        false ->
            if
                P1 > P2 ->
                    {{X, Y}, pheromone, P1};
                true ->
                    {{XB, YB}, pheromone, P2}
            end
    end.

getHoleDirection(X, Y) ->
    if
        X > 250 ->
              if
                  Y > 250 -> sw;
                  Y < 250 -> nw;
                  true -> w
              end;      
        X < 250 ->
            if
                Y > 250 -> se; 
                Y < 250 -> ne;
                true -> e
            end;
        true ->
            if
                Y > 250 -> s;        
                true -> n
            end
    end.