:-use_module("./Matrix.pl").
:-use_module("./Prints.pl").
:-use_module("./Files.pl").



main :-
    files:getCells,
    matrix:createSquareMatrix(3, "dead", M),
    matrix:put([[1,0], [1,1], [1,2]], M, "cu", M1),
    prints:printMatrix(M1),
    writeln(""),

    matrix:matrixUpdate(M1, M2),
    prints:printMatrix(M2).
    