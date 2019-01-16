-module(window).
-export([startWindow/1]).

startWindow(Map) ->
    spawn(fun() -> windowLoop(init, a, Map, a) end).

windowLoop(State, Panel, Map, Background) ->
    case State of 
        init ->
            NewPanel = initWindow(),
            NewState = run,
            Image = wxImage:new("back.png"),
            NewBackground = wxBitmap:new(Image);
        run ->
            %timer:sleep(5)
            draw(Panel, Map, Background),
            NewState = run,
            NewPanel = Panel,
            NewBackground = Background
    end,
    timer:sleep(20),
    windowLoop(NewState, NewPanel, Map, NewBackground).

initWindow() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Ants", [{size, {800, 600}}]),    
    Panel = wxPanel:new(Frame),
    wxFrame:connect(Panel, paint),
    wxFrame:show(Frame),
    Panel.

draw(Panel, Map, Bitmap) ->
    Ants = ets:foldl(fun({Key, List}, Acc) -> addIfAnt(Acc, List, Key) end, [], Map),
    ClientDC = wxClientDC:new(Panel),
    wxDC:drawBitmap(ClientDC, Bitmap, {0,0}),
    wxDC:setPen(ClientDC, wxPen:new({0, 0, 0, 255}, [{width, 1}])),
    %io:format("~p", Ants),
    lists:foreach(fun({X,Y}) -> wxDC:drawCircle(ClientDC, {X, Y}, 1) end, Ants),

    wxClientDC:destroy(ClientDC).

addIfAnt(Acc, List, Key) ->
    case lists:any(fun({ant, _}) -> true;(_)-> false end, List) of
        true -> [Key | Acc];
        false -> Acc
    end.