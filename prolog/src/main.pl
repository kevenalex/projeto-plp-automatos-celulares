:- use_module("./Matrix.pl").
:- use_module("./Prints.pl").
:- use_module("./Files.pl").
:- use_module("./Utils/Render.pl").
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
option("3") :- render:printMid(celulas), main, !.
option("4") :- render:printMid(tutorial), main, !.
option("5") :- halt.
option(_) :- render:printMid("OPÇÃO INVÁLIDA"), sleep(2), main.

