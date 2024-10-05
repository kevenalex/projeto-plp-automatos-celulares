:- module(files, []).
:- use_module("./Matrix.pl").
:- use_module("./Cell.pl").
:- use_module(library(http/json)).


saveScene(Name, Matrix):-
    exists_file("../storage/scenes.json"),
    fileNotEmpty("../storage/scenes.json"),

    open("../storage/scenes.json", read, ScenesFile),
    json_read_dict(ScenesFile, Scenes),
    close(ScenesFile),

    matrix:matrixToList(Matrix, List),

    open("../storage/scenes.json", write, File),
    json_write_dict(File, Scenes.put(Name, List)),
    close(File), !.


% Pra quando o arquivo não existe
saveScene(Name, Matrix) :-
    matrix:matrixToList(Matrix, List),

    open("../storage/scenes.json", write, File),
    json_write(File, json([Name=List])),
    close(File).


getSceneMatrix(Name, Matrix):-
    open("../storage/scenes.json", read, File),
    json_read_dict(File, Dict),
    matrix:matrixFromList(Dict.Name, Matrix).


fileNotEmpty(FilePath) :-
    open(FilePath, read, Stream),
    get_char(Stream, Char),      
    close(Stream), 
    Char \= end_of_file.


saveCells:-
    open("../storage/cells.json", write, File),
    cell:listCells(Cells),
    json_write(File, json([Cells])),
    close(File).
    


main:-
    A = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:f, 2:g}, 
        2:_{0:h, 1:i, 2:j}
            },
    saveScene(cu, A),
    getSceneMatrix(cu, R),
    writeln(R).


