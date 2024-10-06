:- module(utils, []).

in(X, [X|_]):- !.
in(X, [_|T]):- in(X, T).
