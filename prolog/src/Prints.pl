:- module(prints, []).
:- use_module("./Matrix.pl").
:- use_module( "./Cell.pl").    
:-use_module("./Utils/Render.pl").


toStringCell(Cell):-
    cell:cell(Cell, Color, Stay, Birth),
    atom_string(Acolor, Color),
    ansi_format([fg(Acolor)], "~w B~w/S~w", [Cell, Birth, Stay]).


printMatrix(Matrix):-
    matrix:matrixToList(Matrix, List),
    printMatrixList(List).

printMatrixList([]).
printMatrixList([H|T]):-
    printLine(H),
    printMatrixList(T).

printLine([]):- writeln("").
printLine([H|T]):-
    printCell(H),
    printLine(T).

printCell(Cell):-
    % writeln(Cell),
    cell:getCellColor(Cell, Color),
    atom_string(Acolor, Color),
    render:setCursorColumn(98),
    ansi_format([fg(Acolor)], "██", []).

test:-
    matrix:createSquareMatrix(4,"dead", Matrix0),
    cell:createCell(cellNova, "blue", [1,2,3], [2,3,4]),
    matrix:put(1,2,cellNova, Matrix0, Matrix),
    printMatrix(Matrix).

test2:-
    cell:createCell(cellNova, "blue", [1,2,3], [2,3,4]),
    toStringCell(cellNova),
    printCell(cellNova).