:- module(scenesController, []).
:- use_module("./Matrix.pl").
:- use_module("./Prints.pl").
:- use_module("./Files.pl").


main:-
    render:printEmptyLines(17),
    render:printMid("CENAS SALVAS:"),
    listScenes,
    read_line_to_string(user_input, Option),
    option(Option).

option("1"):- simula, main, !.
option("2"):- deleta, main, !.
option("3"):- !.
option(_).

listScenes:-
    files:getSceneNames(List),
    listScenes(List).

listScenes([H|T]):-
    render:printMid(H), listScenes(T).
listScenes([]) :- 
    render:printEmptyLines(2),
    render:printMid(" 1) SIMULAR CENA   2) DELETAR CENA   3) VOLTAR").

simula:-
    render:clear_screen,
    listScenes,
    render:printEmptyLines(17),
    render:printMid("ESCOLHA A CENA QUE DESEJA SIMULAR:").

deleta:-
    render:clear_screen,
    render:printEmptyLines(17),
    listScenes,
    render:printMid("ESCOLHA A CENA QUE DESEJA DELETAR:"),
    read_line_to_string(user_input, Option),
    files:deleteScene(Option).
