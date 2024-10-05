:- module(files, []).
:- use_module(matrix).
:- use_module(library(http/json)).


saveScene(Name, Matrix, Path):-
    exists_file(Path),
    fileNotEmpty(Path),

    open(Path, read, ScenesFile),
    json_read_dict(ScenesFile, Scenes),
    close(ScenesFile),

    matrix:matrixSize(Matrix, X, Y),
    matrix:matrixToList(Matrix, List),

    open(Path, write, File),
    Scene = _{matrix:List, x:X, y:Y},

    json_write_dict(File, Scenes.put(Name, Scene)),
    close(File), !.


% Pra quando arquivo não existe
saveScene(Name, Matrix, Path) :-
    matrix:matrixSize(Matrix, X, Y),
    matrix:matrixToList(Matrix, List),

    open(Path, write, File),
    json_write(File, json([Name=json([matrix=List, x=X, y=Y])])),
    close(File).


fileNotEmpty(FilePath) :-
    open(FilePath, read, Stream),
    get_char(Stream, Char),      
    close(Stream), 
    Char \= end_of_file.



main:-
    A = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:f, 2:g}, 
        2:_{0:h, 1:i, 2:j}
            },
    saveScene(adhjasdjkl, A, "../storage/scenes.json").

