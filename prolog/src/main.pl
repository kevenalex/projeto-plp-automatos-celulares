:- use_module("./Matrix.pl").
:- use_module("./Prints.pl").
:- use_module("./Files.pl").
:- use_module("./Utils/Render.pl").

:- set_prolog_flag(encoding, utf8).



main :-
    render:printScreen("../storage/mainMenu/mainMenu.txt"),
    read_line_to_string(user_input, Option),
    option(Option).


option("1") :- render:printMid(sandbox), main, !.
option("2") :- render:printMid(cenas), main, !.
option("3") :- render:printMid(celulas), main, !.
option("4") :- render:printMid(tutorial), main, !.
option("5") :- halt.
option(_) :- render:printMid("OPÇÃO INVÁLIDA"), sleep(2), main.

