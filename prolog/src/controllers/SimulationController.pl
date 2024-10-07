:- module(simulation, []).

:- use_module("../Matrix.pl").
:- use_module("../Cell.pl").
:- use_module("../Prints.pl").
:- use_module("./Utils/Render.pl").





run(Matrix):-
    % render:clearScreen,
    % render:printEmptyLines(10),
    prints:printMatrix(Matrix),
    render:printMid("1) ADICIONAR CÉLULAS 2) REMOVER CÉLULAS 3) SALVAR CENA 4) VOLTAR"),
    render:printMid("ENTER PRA CONTINUAR"),
    read_line_to_string(user_input, Option),
    option(Option, Matrix).
    

option("", Matrix):- matrix:matrixUpdate(Matrix, NewMatrix), run(NewMatrix), !.
option("1", Matrix):- addCells(Matrix, NewMatrix),    read_line_to_string(user_input, _), run(NewMatrix), !.
option("2", Matrix):- removeCells(Matrix, NewMatrix), read_line_to_string(user_input, _), run(NewMatrix), !.
option("3", Matrix):- salvar(Matrix), run(Matrix), !.
option("4", _):- !.
option(_, Matrix):- render:printMid("OPÇÃO INVÁLIDA"), sleep(2), run(Matrix), !.


addCells(Matrix, NewMatrix) :-
    render:printMid("QUAL CÉLULA?."),
    cell:listCellNames(List),
    writeln(List),
    read_line_to_string(user_input, Name),
    render:printMid("DIGITE AS COORDENADAS NO FORMATO [[X, Y], [X, Y]]."),
    read(Coords),
    matrix:put(Coords, Matrix, Name, NewMatrix).

removeCells(Matrix, NewMatrix) :-
    render:printMid("DIGITE AS COORDENADAS NO FORMATO [[X, Y], [X, Y]]."),
    read(Coords),
    matrix:put(Coords, Matrix, "dead", NewMatrix).

salvar(Matrix) :-
    render:printMid("DIGITE O NOME DA CENA"),
    read_line_to_string(user_input, Name),
    files:saveScene(Name, Matrix),
    render:printMid("cena salva.").


test:-
    matrix:createSquareMatrix(4,"dead", Matrix0),
    cell:createCell(cellNova, "blue", [1,2,3], [2,3,4]),
    matrix:put(1,2,cellNova, Matrix0, Matrix),
    prints:printMatrix(Matrix).

test2:-
    matrix:createSquareMatrix(4,"dead", Matrix0),
    cell:createCell("game of life", "green", [2,3], [3]),
    matrix:put([[1,0],[1,1],[1,2]], Matrix0, "game of life", Matrix),
    run(Matrix).
    
