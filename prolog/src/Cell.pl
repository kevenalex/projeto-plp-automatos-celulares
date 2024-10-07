:- module(cell, []).


% Cell é o predicado que relaciona um tipo de célula(identificada pelo nome) 
% a suas regras e sua cor.
createCell(Name, Color, StayRule, BirthRule) :-
    colorToHex(Color, HexColor),
    isValidColor(HexColor), 
    assert(cell(Name, HexColor, StayRule, BirthRule)).
deleteCell(Name) :- retract(cell(Name, _, _, _)).
:- dynamic cell/4.


% Pra quando as celulas forem lidas do json.
createCells(List) :- retractall(cell(_, _, _, _)), createCellsRecur(List).
createCellsRecur([]) :- 
    listCellNames(Names),
    (member("dead", Names) -> !
    ; createCell("dead", "black", [], [])).

createCellsRecur([[N,C,S,B]|T]):-  createCell(N, C, S, B), createCellsRecur(T).


% Lista todas as células
listCells(R) :- findall(cell(X, Y, Z, W), cell(X, Y, Z, W), R).
listCellNames(R) :- findall(X, cell(X, _, _, _), R).

getCellColor(Name, Color) :- cell(Name, Color, _, _).
getCellStay(Name, Stay) :- cell(Name, _, Stay, _).
getCellBirth(Name, Birth) :- cell(Name, _, _, Birth).

% esboço, esse predicado deveria relacionar a cor ao código de escape necessário
% pra imprimir texto com aquela cor no terminal, 
% e assim ao mesmo tempo indicar as cores válidas pro sistema.
% isValidColor("blue").
% isValidColor("red").
% isValidColor("green").
% isValidColor("white").
% isValidColor("black").
% isValidColor("cyan").
% isValidColor("yellow").
% isValidColor("dourado").

isValidColor("black").
isValidColor("red").
isValidColor("green").
isValidColor("yellow").
isValidColor("blue").
isValidColor("magenta").
isValidColor("cyan").
isValidColor("white").
isValidColor("bright red").
isValidColor("bright green").
isValidColor("bright yellow").
isValidColor("bright magenta").
isValidColor("bright cyan").
isValidColor("bright white").
isValidColor("golden").
isValidColor("purple").
isValidColor("aquamarine").
isValidColor("salmon").
isValidColor("orange").
isValidColor("pink").
isValidColor("hot pink").
isValidColor("neon green").


isValidColor("#FF0000"). % Vermelho
isValidColor("#008000"). % Verde
isValidColor("#FFFF00"). % Amarelo
isValidColor("#0000FF"). % Azul
isValidColor("#FF00FF"). % Magenta
isValidColor("#00FFFF"). % Ciano
isValidColor("#FFFFFF"). % Branco
isValidColor("#FF5555"). % Vermelho Brilhante
isValidColor("#55FF55"). % Verde Brilhante
isValidColor("#FFFF55"). % Amarelo Brilhante
isValidColor("#FF55FF"). % Magenta Brilhante
isValidColor("#55FFFF"). % Ciano Brilhante
isValidColor("#F5F5F5"). % Branco Brilhante
isValidColor("#FFD700"). % Dourado
isValidColor("#800080"). % Roxo
isValidColor("#20B2AA"). % Verde Água
isValidColor("#FA8072"). % Salmão
isValidColor("#FFA500"). % Laranja
isValidColor("#FFC0CB"). % Rosinha
isValidColor("#FF1493"). % Rosa Shock
isValidColor("#39FF14"). % Verde Neon

colorToHex("black", "black").
colorToHex("red", "#FF0000").
colorToHex("green", "#008000").
colorToHex("yellow", "#FFFF00").
colorToHex("blue", "#0000FF").
colorToHex("magenta", "#FF00FF").
colorToHex("cyan", "#00FFFF").
colorToHex("white", "#FFFFFF").
colorToHex("bright red", "#FF5555").
colorToHex("bright green", "#55FF55").
colorToHex("bright yellow", "#FFFF55").
colorToHex("bright magenta", "#FF55FF").
colorToHex("bright cyan", "#55FFFF").
colorToHex("bright white", "#F5F5F5").
colorToHex("golden", "#FFD700").
colorToHex("purple", "#800080").
colorToHex("aquamarine", "#20B2AA").
colorToHex("salmon", "#FA8072").
colorToHex("orange", "#FFA500").
colorToHex("pink", "#FFC0CB").
colorToHex("hot pink", "#FF1493").
colorToHex("neon green", "#39FF14").


colorToHex(Color, Color).

% colorToHex("verde", "#FFD700").
% colorToHex("amarelo", "#FFD700").
% colorToHex("azul", "#FFD700").
% colorToHex("magenta", "#FFD700").
% colorToHex("dourado", "#FFD700").
% colorToHex("dourado", "#FFD700").
% colorToHex("dourado", "#FFD700").
% colorToHex("dourado", "#FFD700").
% colorToHex("dourado", "#FFD700").
% colorToHex("dourado", "#FFD700").
% colorToHex("dourado", "#FFD700").

