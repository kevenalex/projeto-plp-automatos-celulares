:- module(scenesController, []).
:- use_module("./Matrix.pl").
:- use_module("./Prints.pl").
:- use_module("./Files.pl").
:- use_module("./controllers/SimulationController.pl").


main:-
    render:printEmptyLines(17),
    render:printMid("CENAS SALVAS:"),
    listScenes,
    render:printMid(" 1) SIMULAR CENA   2) DELETAR CENA   3) VOLTAR"),
    read_line_to_string(user_input, Option),
    option(Option).

option("1"):- simula, !.
option("2"):- deleta, !.
option("3"):- !.
option(_).

listScenes:-
    files:getSceneNames(List),
    listScenes(List).

listScenes([H|T]):-
    render:printMid(H), listScenes(T).
listScenes([]) :- 
    render:printEmptyLines(2).

simula:-
    render:clearScreen,
    render:printEmptyLines(17),
    listScenes,
    render:printMid("ESCOLHA A CENA QUE DESEJA SIMULAR:"),
    read_line_to_string(user_input, Name),
    files:getSceneMatrix(Name, Matrix),
    simulation:run(Matrix).


deleta:-
    render:clearScreen,
    render:printEmptyLines(17),
    listScenes,
    render:printMid("ESCOLHA A CENA QUE DESEJA DELETAR:"),
    read_line_to_string(user_input, Option),
    files:deleteScene(Option).
