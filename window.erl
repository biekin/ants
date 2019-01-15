-module(window).
-export([startWindow/1]).

startWindow(MapPid) ->
    spawn(fun() -> windowLoop(init, a, MapPid) end).

windowLoop(State, Panel, MapPid) ->
    case State of 
        init ->
            NewPanel = initWindow(),
            NewState = run;
        run ->
            MapPid ! {sendState, self()},
            %timer:sleep(5),
            %MapPid ! {sendState, self()},
            receive 
                {state, Map} ->
                    draw(Panel, Map)
            end,
            NewState = run,
            NewPanel = Panel
    end,
    timer:sleep(20),
    windowLoop(NewState, NewPanel, MapPid).

initWindow() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Ants", [{size, {800, 600}}]),    
    Panel = wxPanel:new(Frame),
    wxFrame:connect(Panel, paint),
    wxFrame:show(Frame),
    Panel.

draw(Panel, Map) ->
    ClientDC = wxClientDC:new(Panel),

    Image = wxImage:new("back.png"),
    Bitmap = wxBitmap:new(Image),
    wxDC:drawBitmap(ClientDC, Bitmap, {0,0}),
    wxDC:setPen(ClientDC, wxPen:new({0, 0, 0, 255}, [{width, 1}])),
    
    Ants = maps:filter(fun(Key, Val) -> lists:any(fun({ant, _}) -> true;(_)->false end ,Val) end, Map),
    lists:foreach(fun({X,Y}) -> wxDC:drawCircle(ClientDC, {X, Y}, 1) end, maps:keys(Ants)),

    wxBitmap:destroy(Bitmap),
    wxClientDC:destroy(ClientDC).
    %timer:sleep(20).