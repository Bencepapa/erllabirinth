-module(labirinth).

-export([generate/3, print/1,
         new_obj/2, clear/1, set/4, get/3, get_row/2, get_column/2,
         horiz/5, vert/5, fill/6]).
-define(WALL, 3). % [32 + 3] = '#'

% Create an empty labirinth with outer walls and start-stop holes in walls
% Then try to place random walls, iterate only MaxInteration times.
% Width and Height should be odd numbers bigger than 3
generate(Width, _Height, _MaxIteration) when Width rem 2 =:= 0->
    {error, width_should_be_an_odd_number, Width};
generate(_Width, Height, _MaxIteration) when Height rem 2 =:= 0->
    {error, height_should_be_an_odd_number, Height};
generate(Width, Height, _MaxIteration) when Width < 5, Height < 5 ->
    {error, width_or_height_less_than_five, {Width, Height}};
generate(_Width, _Height, MaxIteration) when MaxIteration < 0 ->
    {error, maxiteration_is_negative, MaxIteration};
generate(Width, Height, MaxIteration) ->
    % create new lab obj with outer walls
    Empty = new_obj(Width, Height),
    % open start and end door at the top-left and bottom-rigth
    EmptyWithWalls = set(set(Empty, 1, 2, 0), Width, Height-1, 0),
    make_walls(EmptyWithWalls, MaxIteration).

% set an element in the labirinth object
set(#{data := Data, width := _Width} = Lab, X, Y, Value) ->
    ModifiedRow = setelement(X, element(Y, Data), Value),
    Lab#{data => setelement(Y, Data, ModifiedRow)}.

% set an element from the labirinth object
get(#{data := Data, width := _Width} = _Lab, X, Y) ->
    element(X, element(Y, Data)).

% get a complete row
get_row(#{data := Data} = _Lab, Row) ->
    tuple_to_list(element(Row, Data)).

% get a complete column
get_column(#{height := Height} = Lab, Column) ->
    [get(Lab, Column, Y) || Y <- lists:seq(1, Height)].

% add a horizontal wall
% @TODO: sould get a Value to use for the wall
horiz(Lab, Y, X, X, Value) ->
    set(Lab, X, Y, Value);
horiz(Lab, Y, X1, X2, Value) when X1 < X2 ->
    horiz(set(Lab, X1, Y, Value), Y, X1+1, X2, Value);
horiz(_, _,_,_,_) ->
    erlang:error("horiz: invalid X1, X2").

% add a vertical wall
% @TODO: sould get a Value to use for the wall
vert(Lab, X, Y, Y, Value) ->
    set(Lab, X, Y, Value);
vert(Lab, X, Y1, Y2, Value) when Y1 < Y2->
    vert(set(Lab, X, Y1, Value), X, Y1+1, Y2, Value);
vert(_, _,_,_,_) ->
    erlang:error("vert: invalid Y1, Y2").

% fill a rectangle with Value
fill(_Lab, _X1, Y1, _X2, Y2, _Value) when Y2<Y1->
    error;
fill(Lab, X1, Y, X2, Y, Value) ->
    horiz(Lab, Y, X1, X2, Value);
fill(Lab, X1, Y1, X2, Y2, Value) ->
    fill(horiz(Lab, Y1, X1, X2, Value), X1, Y1+1, X2, Y2, Value).

% clear a labirinth object with 0s
clear(#{width := W, height := H} = Lab) ->
    fill(Lab, 1, 1, W, H, 0).

%  create an empty labirinth data
create_empty_data(W, H) ->
    list_to_tuple(
        [list_to_tuple([0 || _I <- lists:seq(0, W-1)])
            || _J <- lists:seq(0, H-1)
        ]
    ).  

% new empty labirinth object with outer walls.
new_obj(W, H) ->
    Empty = #{width => W, height => H, data => create_empty_data(W, H)},
    % create walls
    vert(
        vert(
            horiz(
                horiz(Empty, 1, 1, W, ?WALL),
                H, 1, W, ?WALL
            ),
            1, 1, H, ?WALL
        ),
        W, 1, H, ?WALL
    ).

% get back the ranges {Value, Startposition, StopPosition}
% of a list of values
% should be used in a lists:foldl
% example: [1,0,0,0,1,1,1,0,0,0,1]
% returns: [{1,11,11},{0,8,10},{1,5,7},{0,2,4},{1,1,1}]
get_ranges(E, [{E, Start, End} | Tail]) ->
    [{E, Start, End + 1} | Tail];
get_ranges(E, [{F, _Start, End} | _Tail] = Acc) when E =/= F ->
    [{E, End + 1, End + 1} | Acc];
get_ranges(E, []) ->
    [{E, 1, 1}].

% calculate ranges and return with ranges where value=0 and
% the range is bigger than 0
% useful to get the big enough empty space where a new wall can be created
get_big_empty_ranges(RowData) ->
    Ranges = lists:foldl(fun get_ranges/2, [], RowData),
    [Element || {0,S,E} = Element <- Ranges, E - S > 1].

% returns a random element from the list
select_randomly(List) ->
    lists:nth(rand:uniform(length(List)), List).

make_horiz_wall_if_possible(Lab, Row) ->
    % get the whole row data
    RowData = get_row(Lab, Row),
    % get the possible ranges where a wall can be placed
    case get_big_empty_ranges(RowData) of
        [] ->
            {error, out_of_luck, RowData};
        EmptyRanges ->
            {0, Start, End} = select_randomly(EmptyRanges),
            % place always a random length wall, length should be an even number
            Length = rand:uniform((End - Start) div 2) * 2,
            % make a random decision to determine where the hole should be
            case rand:uniform(2) of
                1 ->
                    % hole at the end of the wall
                    horiz(Lab, Row, Start, Start + Length-1, ?WALL);
                2 ->
                    % hole at the start of the wall
                    horiz(Lab, Row, Start + 1, Start + Length, ?WALL)
            end
    end.

make_vert_wall_if_possible(Lab, Column) ->
    CoulmnData = get_column(Lab, Column),
    case get_big_empty_ranges(CoulmnData) of
        [] ->
            {error, out_of_luck, CoulmnData};
        EmptyRanges ->
            {0, Start, End} = select_randomly(EmptyRanges),
            Length = rand:uniform((End - Start) div 2) * 2,
            case rand:uniform(2) of
                1 ->
                    vert(Lab, Column, Start, Start + Length - 1, ?WALL);
                2 ->
                    vert(Lab, Column, Start + 1, Start + Length, ?WALL)
            end
    end.

% random choose a row or column and try to place a wall
% retruns labirint object with new wall, or {error, out_of_luck, _}
make_random_wall(#{height := Height, width := Width} = Lab) ->
    case rand:uniform(2) of
        1 ->
            Row = rand:uniform((Height - 2) div 2)*2 +1,
            make_horiz_wall_if_possible(Lab, Row);
        2 ->
            Column = rand:uniform((Width - 2) div 2)*2 +1,
            make_vert_wall_if_possible(Lab, Column)
    end.

% recursivly try to place walls 
make_walls(Lab, 0) ->
    Lab;
make_walls(Lab, Iteration) ->
    case make_random_wall(Lab) of
        {error, out_of_luck, _} ->
            make_walls(Lab, Iteration-1);
        NewLab ->
            make_walls(NewLab, Iteration-1)
    end.

% can print out labirinth object or data
print({error, _, _} = Error)->
    Error;
print([])->
    ok;
print([Row|Labirinth]) when is_tuple(Row)->
    [io:format("~c", [32+X]) || X <- tuple_to_list(Row)],
    io:format("~n"),
    print(Labirinth);
print([Row|Labirinth]) ->
    [io:format("~c", [32+X]) || X <- Row],
    io:format("~n"),
    print(Labirinth);
print(Lab) when is_tuple(Lab) ->
    Labirinth = tuple_to_list(Lab),
    print(Labirinth);
print(#{data := Data} = _Lab)  ->
    print(Data).
