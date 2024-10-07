:-module(tutorial, []).
:-use_module("./Utils/Render.pl").
:-use_module("./Cell.pl").
:-use_module("./Prints.pl").
:-use_module("./Matrix.pl").
:-use_module(library(ansi_term)).
:-set_prolog_flag(encoding, utf8).

tutorial:-
    cell:createCell("GameOfLife", "green", [2, 3], [3]),
    cell:createCell("High Life", "red", [2, 3], [3, 6]),
    matrix:createSquareMatrix(3, "dead", Square),

    render:clearScreen,
    render:printScreen("../storage/tutorial/intro.txt"),
    read_line_to_string(user_input, _),
    
    render:clearScreen,
    render:printScreen("../storage/tutorial/apresentaRegras.txt"),
    render:setCursorColumn(90),
    prints:toStringCell("GameOfLife"),
    render:printEmptyLines(25),
    render:printMidScreen("aperte Enter para continuar..."),
    read_line_to_string(user_input, _),

    render:setCursorColumn(90),
    prints:toStringCell("GameOfLife"),

    render:printScreen("../storage/tutorial/explicaBirth.txt"),
    matrix:put([[0,0], [0,2], [2,1]], Square, "GameOfLife", GridInicial),
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
    prints:toStringCell("GameOfLife"),
    render:printScreen("../storage/tutorial/explicaStay.txt"),

    render:printEmptyLines(18),
    render:printMidScreen("aperte Enter para continuar..."),
    read_line_to_string(user_input, _),

    render:clearScreen,
    render:printMidScreen("No caso abaixo, todas as células estão confortavelmente juntas. Cada uma tem exatamente 3 vizinhos."),

    matrix:put([[1,1], [1,2], [2,1], [2,2]], Square, "GameOfLife", Grid3),

    prints:printMatrix(Grid3),
    render:printEmptyLines(1),
    render:setCursorColumn(100),
    render:printMidScreen("|"),
    render:printMidScreen("V"),
    render:printEmptyLines(1),
    prints:printMatrix(Grid3),
    render:printMidScreen("aperte Enter para ver nada catastrófico..."),
    render:printEmptyLines(30),
    
    read_line_to_string(user_input, _),
    matrix:createMatrix(10, 25, "dead", Grid4),
    matrix:put([[3, 19], [4,19], [3, 20], [4,20]], Grid4, "GameOfLife", Grid5),
    matrix:put([[3,0], [4,0], [3,1], [4,1], [5,1], [2,2], [4,2], [5,2], [2,3], [3,3], [4,3], [3,4]], Grid5, "High Life", GridAtaque),
    render:setCursorColumn(47),
    write("Meu Deus! Um pato do tipo"),
    render:setCursorColumn(90),
    prints:toStringCell("High Life"),
    write(" está atacando nosso quadrado "),
    render:setCursorColumn(90),
    prints:toStringCell("GameOfLife"),
    write("!"),
    prints:printMatrix(GridAtaque),
    render:printMidScreen("aperte Enter para continuar..."),
    read_line_to_string(user_input, _),

    loopGrid(GridAtaque, GridAtaque, 0),

    render:printScreen("../storage/tutorial/final.txt"),
    render:printMidScreen("aperte Enter para voltar!"),
    read_line_to_string(user_input, _).


loopGrid(Matrix, NewMatrix, N):- N \= 0,Matrix = NewMatrix, !.
loopGrid(Matrix, _, Num):-
    render:clearScreen,
    render:printEmptyLines(44),
    matrix:matrixUpdate(Matrix, Matrix2),
    NewNum is Num + 1,

    prints:printMatrix(Matrix2),

    render:printMidScreen("aperte Enter para continuar..."),
    read_line_to_string(user_input, _),

    loopGrid(Matrix2, Matrix, NewNum).

teste1:-
    tutorial.
    % cell:createCell(GameOfLife, "green", [3], [2,3]),
    % prints:toStringCell(GameOfLife), halt.
% conways = Cell "Game of Life" (Rule [3] [2,3]) "VERDE  BRILHANTE"