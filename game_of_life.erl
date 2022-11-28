-module(game_of_life).
-export([run/0]).

-import(term, [pos/1]).

-record(coord, {x, y}).
-record(cell, {coord=#coord{}, state, changed}).

-define(Width, 80).
-define(Height, 40).


wrap({X, Y}) when X < 0 -> wrap({?Width + X, Y});
wrap({X, Y}) when Y < 0 -> wrap({X, ?Height + Y});
wrap({X, Y}) when X >= ?Width -> wrap({X - ?Width, Y});
wrap({X, Y}) when Y >= ?Height -> wrap({X, Y - ?Height});
wrap({X, Y}) -> {X, Y}.


update_cell(Cell, Board) ->
    {_, X, Y} = Cell#cell.coord,
    %io:format("update_cell/2: Cell -> ~p~n---------~n", [Cell]),
    N = get_neighbours(Cell#cell.coord, Board),
    %io:format("update_cell/2: N -> ~p~n", [N]),
    S = get_score(N),
    NS =
        if Cell#cell.state ->
            case S of
                A when A < 2 -> false;
                A when A > 3 -> false;
                _ -> true
            end;
        true ->
            case S of
                A when A == 3 -> true;
                _ -> false
            end
        end,
    %io:format("get_neighbours/2: S -> ~p~n", [S]),
    %io:format("get_neighbours/2: SN -> ~p~n---------~n", [NS]),

    C = #cell{coord=#coord{x=X, y=Y}, state=NS, changed=Cell#cell.state/=NS},
    %io:format("~p~n", [C]),
    C.


get_neighbours(Coord, Board) ->
    {_, X, Y} = Coord,
    N = [
        wrap({X-1, Y-1}), wrap({X, Y-1}), wrap({X+1, Y-1}), 
        wrap({X-1, Y}),                   wrap({X+1, Y}), 
        wrap({X-1, Y+1}), wrap({X, Y+1}), wrap({X+1, Y+1})
    ],
    %io:format("get_neighbours/2: length(Board) -> ~p~n", [length(Board)]),
    %io:format("get_neighbours/2: Coord -> ~p~n----~n", [Coord]),
    %io:format("get_neighbours/2: N -> ~p~n---------~n", [N]),
    lists:map(fun(C) -> lists:nth(get_pos(C), Board) end, N).


get_score(N) ->
    lists:sum([1 || X <- N, X#cell.state]).


get_coord(PI) when is_number(PI) ->
    P = PI - 1,
    X = P rem ?Width,
    Y = trunc((P - X) / ?Width),
    %io:format("get_coord/1: P -> ~p~n", [P]),
    %io:format("get_coord/1: #coord{x, y} -> ~p~n---------~n", [#coord{x=X, y=Y}]),
    #coord{x=X, y=Y}.


get_pos({X, Y}) ->
    %io:format("get_pos/1: {X, Y} -> ~p~n", [{X, Y}]),
    %io:format("get_pos/1: (?Width * Y + X) + 1 -> ~p~n---------~n", [(?Width * Y + X) + 1]),
    (?Width * Y + X) + 1.


state_char(true) -> "#";
state_char(false) -> " ".

print_cell(Cell) ->
    Coord = Cell#cell.coord,
    State = Cell#cell.state,
    term:pos({Coord#coord.x, Coord#coord.y}),
    io:format("~s", [state_char(State)]).
    %io:format("~p~n", [Cell]).


print_board(Board) ->
    lists:foreach(fun(C) -> print_cell(C) end, [C ||C <- Board, C#cell.changed]).


loop(_, 0) -> ok;
loop(Board, N) ->
    %io:format("loop/1~n"),
    %io:format("~p~n---------~n",[Board]),
    print_board(Board),

    B = lists:map(fun(C) -> update_cell(C, Board) end, [B#cell{changed=false} || B <- Board]),
    %timer:sleep(24),
    loop(B, N - 1).


rnd_board() ->
    [#cell{coord=get_coord(P), state=rand:uniform() > 0.75, changed=true} || P <- lists:seq(1, ?Width * ?Height)].


print_border_vline(0) -> ok;
print_border_vline(N) ->
    io:format("~tc~ts~tc~n", [$│, lists:duplicate(?Width, hd(" ")), $│]),
    print_border_vline(N-1).
    
print_border() -> 
    io:format("┌"),
    io:format("~ts", [lists:duplicate(?Width, hd("─"))]),
    io:format("┐~n"),

    print_border_vline(?Height),

    io:format("└"),
    io:format("~ts", [lists:duplicate(?Width, hd("─"))]),
    io:format("┘~n").


run() ->
    term:enter_alt(true),
    print_border(),
    Board = rnd_board(),
    loop(Board, 200),
    term:enter_alt(false),
    ok.
