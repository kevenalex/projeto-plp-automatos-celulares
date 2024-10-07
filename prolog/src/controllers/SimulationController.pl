:- module(simulation, []).

:- use_module("../Matrix.pl").
:- use_module("../Cell.pl").
:- use_module("../Prints.pl").





run(Matrix):-
    matrix:matrixUpdate(Matrix, NewMatrix),
    prints:printMatrix(Matrix),
    (checkSpace -> 
        run(NewMatrix)
    ;
        render:printMidScreen("eba adoro gays")
    ).

checkSpace:-
    get_single_char(Code),
    Code == 32.

test:-
    matrix:createSquareMatrix(4,"dead", Matrix0),
    cell:createCell(cellNova, "blue", [1,2,3], [2,3,4]),
    matrix:put(1,2,cellNova, Matrix0, Matrix),
    prints:printMatrix(Matrix).

test2:-
    matrix:createSquareMatrix(4,"dead", Matrix0),
    cell:createCell(conways, "blue", [2,3], [3]),
    matrix:put([[1,0],[1,1],[1,2]], Matrix0, conways, Matrix),
    run(Matrix).
    
