:-module(tutorial, []).
:-use_module("../app/Utils/Render.pl").
:-use_module("./Cell.pl").
:-use_module("./Prints.pl").
:-use_module("./Matrix.pl").
:-use_module(library(ansi_term)).
:-set_prolog_flag(encoding, utf8).

tutorial:-
    cell:createCell("GameOfLife", "green", [3], [2, 3]),
    cell:createCell("High Life", "red", [3, 6], [2, 3]),
    matrix:createSquareMatrix(3, "dead", Square),

    render:clearScreen,
    render:printScreen("../app/storage/tutorial/intro.txt"),
    read_line_to_string(user_input, _),
    
    render:clearScreen,
    render:printScreen("../app/storage/tutorial/apresentaRegras.txt"),
    render:setCursorColumn(90),
    prints:printCell("GameOfLife"),
    render:printEmptyLines(25),
    render:printMidScreen("aperte Enter para continuar..."),
    read_line_to_string(user_input, _),

    printCell("GameOfLife"),

    render:printScreen("../app/storage/tutorial/explicaBirth.txt"),
    matrix:put([[1,1], [1,3], [3,2]], Square, "GameOfLife", GridInicial),
    prints:printMatrix(GridInicial),
    render:printEmptyLines(20),
    read_line_to_string(user_input, _),

    prints:printMatrix(GridInicial),
    render:printEmptyLines(1),
    render:printMidScreen("|"),
    render:printMidScreen("V"),
    render:printEmptyLines(1),
    matrix:matrixUpdate(GridInicial, Grid2),
    prints:printMatrix(Grid2),

    render:printEmptyLines(1),
    render:setCursorColumn(90),
    prints:printCell("GameOfLife"),
    render:printScreen("../app/storage/tutorial/explicaStay.txt"),

    render:printEmptyLines(18),
    render:printMidScreen("aperte Enter para continuar..."),
    read_line_to_string(user_input, _),

    render:clearScreen,
    render:printMidScreen("No caso abaixo, todas as células estão confortavelmente juntas. Cada uma tem exatamente 3 vizinhos."),

    matrix:put([[1,1], [1,2], [2,1], [2,2]], Square, "GameOfLife", Grid3),

    prints:printMatrix(Grid3),
    render:printEmptyLines(1),
    render:setCursorColumn(101),
    render:printMidScreen("|"),
    render:printMidScreen("V"),
    render:printEmptyLines(1),
    prints:printMatrix(Grid3),
    render:printMidScreen("aperte Enter para ver nada catastrófico..."),
    render:printEmptyLines(30),
    
    read_line_to_string(user_input, _),
    matrix:createMatrix(10, 25, "dead", Grid4),
    matrix:put([[4, 20], [5,20], [4, 21], [5,21]], Grid4, "GameOfLife", Grid5),
    matrix:put([[4,1], [5,1], [4,2], [5,2], [6,2], [3,3], [5,3], [6,3], [3,4], [4,4], [5,4], [4,5]], Grid5, "High Life", GridAtaque),
    render:setCursorColumn(47),
    write("Meu Deus! Um pato do tipo"),
    prints:printCell("High Life"),
    write(" está atacando nosso quadrado "),
    prints:printCell("GameOfLife"),
    write("!"),
    prints:printMatrix(GridAtaque),
    render:printMidScreen("aperte Enter para continuar..."),
    read_line_to_string(user_input, _),

    loopGrid(GridAtaque, 0),

    render:printScreen("../app/storage/tutorial/final.txt"),
    render:printMidScreen("aperte Enter para voltar!"),
    read_line_to_string(user_input, _).


loopGrid(_, 65):- !.
loopGrid(Matrix, Num):-
    matrix:matrixUpdate(Matrix, NewMatrix),
    NewNum is Num + 1,

    prints:printMatrix(NewMatrix),

    render:printMidScreen("aperte Enter para continuar..."),
    read_line_to_string(user_input, _),

    loopGrid(NewMatrix, NewNum).

teste1:-
    tutorial.
    % cell:createCell(GameOfLife, "green", [3], [2,3]),
    % prints:printCell(GameOfLife), halt.
% conways = Cell "Game of Life" (Rule [3] [2,3]) "VERDE  BRILHANTE"