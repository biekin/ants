-module(hole).
-export([startHole/2]).

startHole(NumberOfAnts, Fun) ->
    spawn(fun() -> holeLoop(NumberOfAnts, Fun) end).

holeLoop(Counter, Fun) ->
    receive
        die -> ok
        after 20 ->
            case Counter of
                0 -> ok;
                N -> 
                    Fun(),
                    holeLoop(N - 1, Fun)    
            end
    end.