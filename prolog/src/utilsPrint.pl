:- module(prints, []).
:- use_module("./Matrix.pl").
:- use_module( "./Cell.pl").    

printCell(Cell):-
    cell:getCellColor(Cell, Color),
    atom_string(Acolor, Color),
    ansi_format([fg(Acolor)], "██", []).

toStringCell(Cell):-
    cell:cell(Cell, Color, Stay, Birth),
    atom_string(Acolor, Color),
    ansi_format([fg(Acolor)], "~w B~w/S~w", [Cell, Birth, Stay]).

printLine([]):- writeln("").
printLine([H|T]):-
    printCell(H),
    printLine(T).

printMatrix(Matrix):-
    matrix:matrixToList(Matrix, List),
    printMatrixList(List).

printMatrixList([]).
printMatrixList([H|T]):-
    printLine(H),
    printMatrixList(T).

test:-
    matrix:createSquareMatrix(4,dead, Matrix0),
    cell:createCell(cellNova, "blue", [1,2,3], [2,3,4]),
    matrix:put(1,2,cellNova, Matrix0, Matrix),
    printMatrix(Matrix).

test2:-
    cell:createCell(cellNova, "blue", [1,2,3], [2,3,4]),
    printCell(cellNova).