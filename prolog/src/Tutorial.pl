:-module(tutorial, []).
:-use_module("./app/Utils/Render.pl").
:-use_module("./Cell.pl").
:-use_module("./utilsPrint.pl").
:-use_module(library(ansi_term)).
:-set_prolog_flag(encoding, utf8).

tutorial:-
    cell:createCell(GameOfLife, "green", [3], [2,3]),

    render:clearScreen,
    render:printScreen("app/storage/tutorial/intro.txt"),
    read_line_to_string(user_input, _),
    
    
    render:clearScreen,
    render:printScreen("app/storage/tutorial/apresentaRegras.txt"),
    read_line_to_string(user_input, _),
    render:setCursorColumn(90),
    prints:printCell(GameOfLife),

    render:printEmptyLines(25),
    render:printMidScreen('aperte Enter para continuar...'),
    render:setCursorColumn(90),
    render:printMidScreen(printCell(GameOfLife))
    .

teste1:-
    tutorial.
    % cell:createCell(GameOfLife, "green", [3], [2,3]),
    % prints:printCell(GameOfLife), halt.
% conways = Cell "Game of Life" (Rule [3] [2,3]) "VERDE  BRILHANTE"