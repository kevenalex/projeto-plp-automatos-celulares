:- module(prints, []).
:- use_module("./Matrix.pl").
:- use_module( "./Cell.pl").    

printCell(Cell):-
    cell:getCellColor(Cell, Color),
    ansi_format([fg(Color)], "██", []).

toStringCell(Cell):-
    cell:cell(Cell, Color, Stay, Birth),
    ansi_format([fg(Color)], "~w B~w/S~w", [Cell, Birth, Stay]).

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
    cell:createCell(cellNova, blue, [1,2,3], [2,3,4]),
    matrix:put(1,2,cellNova, Matrix0, Matrix),
    printMatrix(Matrix).