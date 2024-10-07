:- use_module("./Matrix.pl").
:- use_module("./Prints.pl").
:- use_module("./Files.pl").
:- use_module("./Utils/Render.pl").
:- use_module("./Tutorial.pl").
:- use_module("./controllers/ScenesController.pl").
:- use_module("./controllers/CellController.pl").
:- use_module("./controllers/SimulationController.pl").


:- set_prolog_flag(encoding, utf8).



main :-
    files:getCells,
    render:printScreen("../storage/mainMenu/mainMenu.txt"),
    read_line_to_string(user_input, Option),
    option(Option).


option("1") :- 
    matrix:createSquareMatrix(35, "dead", M),
    simulation:run(M), main, !.

option("2") :- scenesController:main, main, !.
option("3") :- cellController:main, main, !.
option("4") :- tutorial:tutorial, main, !.
option("5") :- halt.
option(_) :- render:printMid("OPÇÃO INVÁLIDA"), sleep(2), main.


test:-
A = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:f, 2:g}, 
        2:_{0:h, 1:i, 2:j}
            },
    files:getCells,
    read(List),
    matrix:put(List, A, cu, B),
    writeln(B).

stringNumber(String, Number) :- number_string(Number, String).