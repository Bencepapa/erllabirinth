-module(gui).
-export([start/0, start/1, init/1,
        handle_info/2, handle_call/3, handle_cast/2, handle_event/2,
        handle_sync_event/3]).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

-record(state, {win, demo, example, selector, log, code, img, lab, panel}).

-define(DEBUG_NONE, 101).
-define(DEBUG_VERBOSE, 102).
-define(DEBUG_TRACE, 103).
-define(DEBUG_DRIVER, 104).

start() ->
    start([]).

start(Debug) ->
    Obj = wx_object:start(?MODULE, Debug, []),
    %timer:sleep(1000), %need some time to init called
    %wx_object:call(Obj, draw_labirinth),
    loop(Obj).
    %wx:destroy(Obj).

loop(_Frame) ->
    ok.
    % receive 
   	% #wx{event=#wxClose{}} ->
   	%     io:format("~p Closing window ~n",[self()]),
   	%     ok = wxFrame:setStatusText(Frame, "Closing...",[]),
	%     wxWindow:destroy(Frame),
	%     ok;
	% Msg ->
	%     io:format("Got ~p ~n", [Msg]),
	%     loop(Frame)
    % end.

%% Handled as in normal gen_server callbacks
handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(Msg, _From, #state{img=Images, lab=Lab, panel=Panel} = State) ->
    io:format("Got Call ~p~n",[Msg]),
    draw_labirinth(Lab, Images, Panel),
    {reply,ok,State};
handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

init_labirinth() ->
    labirinth:generate(49,25, 1000).

init(Options) ->
    wx:new(Options),
    process_flag(trap_exit, true),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "wxErlang widgets",
        [{size,{1320,700}}]),
    % MB = wxMenuBar:new(),
    % File    = wxMenu:new([]),
    % wxMenu:append(File, ?wxID_PRINT, "&Print code"),
    % wxMenu:appendSeparator(File),
    % wxMenu:append(File, ?wxID_EXIT, "&Quit"),
    % Debug    = wxMenu:new([]),
    % wxMenu:appendRadioItem(Debug, ?DEBUG_NONE, "None"), 
    % wxMenu:appendRadioItem(Debug, ?DEBUG_VERBOSE, "Verbose"), 
    % wxMenu:appendRadioItem(Debug, ?DEBUG_TRACE, "Trace"), 
    % wxMenu:appendRadioItem(Debug, ?DEBUG_DRIVER, "Driver"), 
    % Help    = wxMenu:new([]),
    % wxMenu:append(Help, ?wxID_HELP, "Help"), 
    % wxMenu:append(Help, ?wxID_ABOUT, "About"), 
    % wxMenuBar:append(MB, File, "&File"),
    % wxMenuBar:append(MB, Debug, "&Debug"),
    % wxMenuBar:append(MB, Help, "&Help"),
    % wxFrame:setMenuBar(Frame,MB),
    wxFrame:connect(Frame, close_window),

    Panel = wxPanel:new(Frame, [{size,{400,600}}]),
    %Vbox = wxBoxSizer:new(?wxVERTICAL),
    %wxSizer:add(Vbox, Panel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    Images = loadResources(
        ["img/wall_1.png", "img/wall_2.png", "img/wall_3.png"]
    ),
   %wxBitmap:new("wall_1.png", [{type, ?wxBITMAP_TYPE_PNG}]),
    %F = fun(I, _) -> redraw(Image,I, 0, 0) end,
    %wxPanel:connect(Panel, paint, [{callback,F}]),

    Lab = init_labirinth(),
    %draw_labirinth(Lab, Images, Panel),

    wxPanel:connect(Panel, left_down, [{skip, true}]),
    wxPanel:connect(Panel, paint, [callback]),
    wxFrame:show(Frame),

    State = #state{win=Frame, img=Images, lab=Lab, panel=Panel}, %, demo={DemoPanel,DemoSz}, selector=LB, log=EvCtrl, code=Code},
    {Frame, State}.


% copied elsewhere
%handle_sync_event(#wx{event = #wxPaint{}}, _wxObj, #state{panel=Panel, memoryDC=MDC, w=W, h=H}) ->
%    redraw(Panel,MDC,W,H).

 handle_sync_event(#wx{event = #wxPaint{}}, _wxObj, #state{panel=Panel, img=Images, lab=Lab}) ->
     draw_labirinth(Lab, Images, Panel).

loadResources([]) -> [];
loadResources([H|T]) ->
    Image = wxBitmap:new(H, [{type, ?wxBITMAP_TYPE_PNG}]),
    io:format("~p~n", [H]),
    [{H, Image} | loadResources(T)].

draw_labirinth_row([], _Images, _Panel, _X, _Y) ->
    ok;
draw_labirinth_row([0 | T], Images, Panel, X, Y) ->
    draw_labirinth_row(T, Images, Panel, X + 27, Y);
draw_labirinth_row([_H | T], Images, Panel, X, Y) ->
    {_, Image} = lists:nth(rand:uniform(length(Images)), Images),
    drawImage(Image, Panel, X, Y),
    draw_labirinth_row(T, Images, Panel, X + 27, Y).

draw_labirinth([], _Images, _Panel, _X, _Y) ->
    ok;
draw_labirinth([Row|Labirinth], Images, Panel, X, Y) when is_tuple(Row)->
    draw_labirinth_row(tuple_to_list(Row), Images, Panel, X, Y),
    draw_labirinth(Labirinth, Images, Panel, X, Y + 27);
draw_labirinth(Lab, Images, Panel, X, Y) when is_tuple(Lab) ->
    Labirinth = tuple_to_list(Lab),
    draw_labirinth(Labirinth, Images, Panel, X, Y).

draw_labirinth(#{data := Data} = _Lab, Images, Panel) ->
    draw_labirinth(Data, Images, Panel, 0, 0).

%% Draw random Image to X,Y
drawImage(Image, #wx{obj=Panel, userData = _Udata}, X, Y) ->
    DC = wxPaintDC:new(Panel),
    wxDC:drawBitmap(DC,Image,{X,Y}),
    %wxDC:drawBitmap(DC,Image,{240,120}),
    wxPaintDC:destroy(DC);
drawImage(Image, Panel, X, Y) ->
    drawImage(Image, #wx{obj=Panel}, X, Y).

% Handle mouse click
handle_event(I=#wx{event=#wxMouse{type=left_down,x=X,y=Y}, obj=_Panel},
	     S = #state{img=Images, lab=_Lab}) ->
    {_, Image} = lists:nth(rand:uniform(length(Images)), Images),
    drawImage(Image, I, X - (X rem 27),Y - (Y rem 27)),
    %draw_labirinth(Lab, Images, Panel),
    {noreply, S};
% handle_event(#wx{event=#wxPaint{}}, S = #state{img=Images, lab=Lab, panel=Panel}) ->
%      draw_labirinth(Lab, Images, Panel),
%      {noreply, S};
handle_event(#wx{event=#wxClose{}}, State = #state{win=_Frame}) ->
    io:format("~p Closing window ~n",[self()]),
    %ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State};
handle_event(_Ev, State) ->
    io:format("Event ~p~n", [_Ev]),
    {noreply,State}.