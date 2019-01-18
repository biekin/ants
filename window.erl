-module(window).
-export([startWindow/3]).

-include_lib("wx/include/wx.hrl").

startWindow(Map, MapPid, HolePid) ->
    spawn(fun() -> windowLoop(init, a, Map, a, a, MapPid, HolePid) end).

windowLoop(State, Panel, Map, Background, Frame, MapPid, HolePid) ->
    case State of 
        init ->
            {NewPanel, NewFrame} = initWindow(self()),
            NewState = run,
            Image = wxImage:new("back.png"),
            NewBackground = wxBitmap:new(Image);
        run ->
            %timer:sleep(5)
            draw(Panel, Map, Background),
            NewState = run,
            NewFrame = Frame,
            NewPanel = Panel,
            NewBackground = Background
    end,
    receive 
        die -> 
            HolePid ! die,
            MapPid ! die
        after 10 -> 
            windowLoop(NewState, NewPanel, Map, NewBackground, NewFrame, MapPid, HolePid)
    end.

initWindow(Pid) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Ants", [{size, {500, 550}}]),
    Panel = wxPanel:new(Frame),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Ants"}]),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),    
    B = wxButton:new(Panel, 10, [{label,"Kataklizm"}]),
    Canvas = wxPanel:new(Panel),
    wxFrame:connect(Canvas, paint),

    wxSizer:add(Sizer, B, [{border, 5}, {flag, ?wxALL}]),
    wxSizer:addSpacer(Sizer, 5),
    wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND},
				{proportion, 1}]),

    wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND},
				   {proportion, 1}]),

    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:layout(MainSizer),

    wxButton:connect(B, command_button_clicked, [{callback,
             fun(Evt, Obj) ->
                 io:format("click~n  event = ~p~n  obj = ~p~n", [Evt, Obj])
                 end
             }]),
    wxFrame:connect(Frame, close_window, [{callback,
        fun(Evt, Obj) ->
            %io:format("click~n  event = ~p~n  obj = ~p~n", [Evt, Obj]),
            Pid ! die
            end
        }]),
    wxFrame:show(Frame),
    {Canvas, Frame}.

draw(Panel, Map, Bitmap) ->
    ets:safe_fixtable(Map, true),
    Ants = ets:match(Map, {'$1', ant, '_'}),
    Food = ets:match(Map, {'$1', food}),
    %Pheromones = ets:match(Map, {'$1', pheromone, '_'}),
    ClientDC = wxClientDC:new(Panel),
    wxDC:drawBitmap(ClientDC, Bitmap, {0,0}),
    wxDC:setPen(ClientDC, wxPen:new({250, 0, 250, 255}, [{width, 1}])),
    %io:format("~p", Ants),
    wxDC:drawCircle(ClientDC, {250, 250}, 3),
    %wxDC:setPen(ClientDC, wxPen:new({0, 0, 255, 200}, [{width, 1}])),
    %lists:foreach(fun([{X,Y}]) -> wxDC:drawCircle(ClientDC, {X, Y}, 1) end, Pheromones),
    wxDC:setPen(ClientDC, wxPen:new({255, 0, 0, 255}, [{width, 2}])),
    lists:foreach(fun([{X,Y}]) -> wxDC:drawCircle(ClientDC, {X, Y}, 1) end, Food),
    wxDC:setPen(ClientDC, wxPen:new({0, 0, 0, 255}, [{width, 1}])),
    lists:foreach(fun([{X,Y}]) -> wxDC:drawCircle(ClientDC, {X, Y}, 1) end, Ants),
    ets:safe_fixtable(Map, false),
    wxClientDC:destroy(ClientDC).

addIfAnt(Acc, List, Key) ->
    case lists:any(fun({ant, _}) -> true;(_)-> false end, List) of
        true -> [Key | Acc];
        false -> Acc
    end.