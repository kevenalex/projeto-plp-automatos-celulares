:-module(render, []).
:-use_module(library(ansi_term)).
:-set_prolog_flag(encoding, utf8).

% Implementações básicas do Render, ainda faltam algumas mais complexas 
% que preciso ver com Ramon na hora de desenvolver a lógica de aplicar cores e etc.
% além disso, provavelmente esses predicados vao ser alterados para se ajustar a
% logica do projeto.

% Predicado para imprimir conteúdo de um arquivo de texto no terminal.
printScreen(FilePath):-
    open(FilePath, read, Stream, [encoding(utf8)]),
    printStream(Stream),
    close(Stream).

% Predicado para imprimir conteúdo de um arquivo de texto no terminal com atraso
% printScreen(FilePath, Clear, Delay):-
%     Clear -> (clearScreen, printScreenWithDelay(FilePath, Delay))
%     ;
%     printScreenWithDelay(FilePath, Delay).

% printScreenWithDelay(FilePath, Delay):-
%     Delay -> m

% Lê o conteúdo (Stream) do arquivo txt, separa-o em linhas e imprime linha 
% a linha no terminal até o fim do arquivo.
printStream(Stream):-
    read_line_to_string(Stream, Line),
    (
        Line \= end_of_file -> 
            writeln(Line),
            printStream(Stream)
        ;
            true
    ).

% Imprime N linhas vazias no terminal.
printEmptyLines(0).
printEmptyLines(N):-
    N > 0,
    writeln(""),
    M is N - 1,
    printEmptyLines(M).

% Imprime uma String centralizada no terminal.
printMidScreen(Text):-
    string_length(Text, Length),
    HalfLengthStr is Length // 2,
    CursorPosition is (100 - HalfLengthStr),
    setCursorColumn(CursorPosition),
    writeln(Text).

% Move o cursor do terminal para uma coluna específica.
setCursorColumn(Column):-
    ansi_format([fg(white)], '~` t~*|', [Column]).


% clearScreen:- 
%     (   
%         current_prolog_flag(os, windows) ->  shell('cls') 
%         ;   
%         shell('clear')
%     ).

clearScreen :- write('\33\[2J').

% Testes dos predicados (todos rodam 100)
% teste1:-
%     printScreen("/home/pedroln/faculdade/projeto-plp-haskell-automatos-celulares/prolog/storage/TesteString.txt"), halt.

% teste2:-
%     printEmptyLines(5),
%     printScreen("../storage/TesteString.txt"), halt.

% teste3:-
%     printMidScreen('-=-=-=-=-=-=-=-=-'),
%     printEmptyLines(3),
%     printMidScreen('Isso tem que ficar centralizado'), 
%     printEmptyLines(3),
%     printMidScreen('-=-=-=-=-=-=-=-=-'),halt.