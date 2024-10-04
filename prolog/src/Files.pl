:- module(files, []).
:- use_module(matrix).
:- use_module(library(http/json)).


saveScene(Name, Matrix, Path):-
    matrix:matrixSize(Matrix, X, Y),
    matrix:matrixToList(Matrix, List),
    open(Path, write, File),
    json_write(File, json([Name=json([matrix=List, x=X, y=Y])])),
    close(File).


main:-
    A = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:f, 2:g}, 
        2:_{0:h, 1:i, 2:j}
            },
    saveScene(cu, A, "../cu.json").

