:- module(cell, []).

cell("dead", "black", [], []).

% Cell é o predicado que relaciona um tipo de célula(identificada pelo nome) 
% a suas regras e sua cor.
createCell(Name, Color, StayRule, BirthRule) :-isValidColor(Color), 
    assert(cell(Name, Color, StayRule, BirthRule)).
deleteCell(Name) :- retract(cell(Name, _, _, _)).
:- dynamic cell/4.


% Pra quando as celulas forem lidas do json.
createCells([]) :- 
    listCellNames(Names),
    (member("dead", Names) -> !
    ; createCell("dead", "black", [], [])).

createCells([[N,C,S,B]|T]):-  createCell(N, C, S, B), createCells(T).


% Lista todas as células
listCells(R) :- findall(cell(X, Y, Z, W), cell(X, Y, Z, W), R).
listCellNames(R) :- findall(X, cell(X, _, _, _), R).

getCellColor(Name, Color) :- cell(Name, Color, _, _).
getCellStay(Name, Stay) :- cell(Name, _, Stay, _).
getCellBirth(Name, Birth) :- cell(Name, _, _, Birth).

% esboço, esse predicado deveria relacionar a cor ao código de escape necessário
% pra imprimir texto com aquela cor no terminal, 
% e assim ao mesmo tempo indicar as cores válidas pro sistema.
isValidColor("blue").
isValidColor("red").
isValidColor("green").
isValidColor("white").
isValidColor("black").

