:- module(prints, [])

:use_module(matrix).

printCell(Name):-
    getCellColor(Name, Color),
    ansi_format([bold,fg(Color)], '██', []).

printGrid(Matrix) :-
    dict_size(Matrix, X),
    printGridPartial(Matrix, 0, X).

printGridPartial(Matrix, Y, Y).
printGridPartial(Matrix, Y, X):- 
    matrix:get(Matrix, X),
    printGridLine(),
    X2 is X - 1,
    printGridPartial(Matrix, Y, X2).


printGridPartial(Matrix, X):-
    get(Matrix, 0, 0, Cell),
    printCell(Cell),
