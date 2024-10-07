:- module(simulation, []).

:- use_module("../Matrix.pl").
:- use_module("../Cell.pl").
:- use_module("../Prints.pl").
:- use_module("./Utils/Render.pl").





run(Matrix):-
    render:clearScreen,
    prints:printMatrix(Matrix),
    matrix:matrixUpdate(Matrix, NewMatrix),
    read_line_to_string(current_output, Option),
    option(Option, NewMatrix).

option(" ", Matrix):- run(Matrix).
option("1", Matrix):- run(Matrix).
option("2", Matrix):- run(Matrix).
option("3", Matrix):- run(Matrix).
option("4", Matrix):- run(Matrix).

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
    
