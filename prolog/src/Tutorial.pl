:-module(tutorial, []).
% :- use_module("./Matrix.pl").
:- use_module( "./Cell.pl").  
:-use_module(library(ansi_term)).
:-set_prolog_flag(encoding, utf8).

% Conways = (createCell('Game of Life', green, [3], [2,3])).

teste1:-
    createCell('Game of Life', green, [3], [2,3]),
    printCell('Game of Life').
% conways = Cell "Game of Life" (Rule [3] [2,3]) "VERDE BRILHANTE"