-module(game_of_life).
-export([run/0, cell_printer/0]).

-import(term, [pos/1]).

-record(coord, {x, y}).
-record(cell, {coord=#coord{}, state, changed}).

-define(Width, 60).
-define(Height, 30).


wrap({X, Y}) when X < 0 -> wrap({?Width + X, Y});
wrap({X, Y}) when Y < 0 -> wrap({X, ?Height + Y});
wrap({X, Y}) when X >= ?Width -> wrap({X - ?Width, Y});
wrap({X, Y}) when Y >= ?Height -> wrap({X, Y - ?Height});
wrap({X, Y}) -> {X, Y}.


update_cell(Cell, Board) ->
    N = get_neighbours(Cell#cell.coord, Board),
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

    Cell#cell{state=NS, changed=Cell#cell.state/=NS}.


get_neighbours(Coord, Board) ->
    {_, X, Y} = Coord,
    N = [
        wrap({X-1, Y-1}), wrap({X, Y-1}), wrap({X+1, Y-1}), 
        wrap({X-1, Y}),                   wrap({X+1, Y}), 
        wrap({X-1, Y+1}), wrap({X, Y+1}), wrap({X+1, Y+1})
    ],
    lists:map(fun(C) -> lists:nth(get_pos(C), Board) end, N).


get_score(N) ->
    lists:sum([1 || X <- N, X#cell.state]).


get_coord(PI) when is_number(PI) ->
    P = PI - 1,
    X = P rem ?Width,
    Y = trunc((P - X) / ?Width),
    #coord{x=X, y=Y}.


get_pos({X, Y}) ->
    (?Width * Y + X) + 1.


state_char(true) -> "#";
state_char(false) -> " ".

print_cell(Cell) ->
    Coord = Cell#cell.coord,
    State = Cell#cell.state,
    term:pos({Coord#coord.x, Coord#coord.y}),
    io:format("~s", [state_char(State)]).

cell_printer() ->
    receive
        {paint, C} ->
            %io:format("Printing cell at ~p, state is ~p~n", [C#cell.coord, C#cell.state]),
            print_cell(C),
            cell_printer();
        {stop} ->
            io:format("Printer shutting down...~n", []),
            ok
    end. 

print_board(Board, Printer) ->
    Ch = [C ||C <- Board, C#cell.changed],
    %io:format("Queueing ~B changed cells for updates~n", [length(Ch)]),
    lists:foreach(fun(C) -> Printer ! {paint, C} end, Ch).
    %io:format("Done queueing~n", []).


loop(_, _, 0) -> ok;
loop(Board, Printer, N) ->
    print_board(Board, Printer),

    B = lists:map(fun(C) -> update_cell(C, Board) end, [B#cell{changed=false} || B <- Board]),
    %timer:sleep(24),
    loop(B, Printer, N - 1).


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
    Printer = spawn(?MODULE, cell_printer, []),
    io:format("Printer Pid: ~p~n", [Printer]),
    timer:sleep(1000),
    
    term:enter_alt(true),
    print_border(),
    Board = rnd_board(),
    loop(Board, Printer, 200),
    term:enter_alt(false),
    
    Printer ! {stop},
    fin.
