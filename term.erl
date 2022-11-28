-module(term).
-export([pos/1, enter_alt/1]).

-define(ESC, <<"\e[">>).
-define(RST, <<"0">>).
-define(SEP, <<";">>).
-define(END, <<"m">>).
-define(CUPEND, <<"H">>).

pos({X, Y}) ->
    io:format("\e[~B;~BH", [Y+2, X+2]).


enter_alt( true) -> io:fwrite("\e[?1049h\e[?25l", []);
enter_alt(false) -> io:fwrite("\e[?1049l\e[?25h", []).
