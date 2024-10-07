:- module(matrix, []).
:- use_module(library(http/json)).
:- use_module("./Cell.pl").


% Um dicionário fingindo ser um array, não um array de dicionários. Preenchido com Value, Size vezes.
% Se eu não cortar aqui, ele diz que Dict = ao dicionario esperado 
% e da mais uma possibilidade vazia, bem estranho.
% 0 indexado, desculpa keven é mais complicado e devagar terminar em 1 e ter Tamanho valores inves de Tamanho -1


matrixUpdate(Matrix, NewMatrix):-
    matrixSize(Matrix, Rows, Cols),
    matrixUpdateRecur(Matrix, Rows, Cols, NewMatrix).


matrixUpdateRecur(Matriz, NumLinhas, NumColunas, NewMatrix) :-
    percorre_linhas(Matriz, 0, NumLinhas, NumColunas, NewMatrix).


percorre_linhas(_, RowIndex, NumLinhas, _, _{}):- RowIndex >= NumLinhas, !.
percorre_linhas(Matrix, RowIndex, NumLinhas, NumColunas, NewMatrix):-
    NewRowIndex is RowIndex + 1,

    percorre_linhas(Matrix, NewRowIndex, NumLinhas, NumColunas, Out),

    updateLine(Matrix, RowIndex, Matrix.get(RowIndex), 0, NumColunas, NewLine),
    NewMatrix = Out.put(RowIndex, NewLine).


updateLine(_, _, _, Index, ColsLimit, _{}):- Index >= ColsLimit, !.
updateLine(Matrix, Row, Line, Index, ColsLimit, NewLine1):-
    NewIndex is Index + 1,

    updateLine(Matrix, Row, Line, NewIndex, ColsLimit, NewLine2),

    cellUpdate([Row, Index], Matrix, Cell),
    
    NewLine1 = NewLine2.put(Index, Cell).


% % Percorre as linhas da matriz
% percorre_linhas(_, LinhaAtual, NumLinhas, _) :- LinhaAtual > NumLinhas, !. % Condição de parada
% percorre_linhas(Matriz, LinhaAtual, NumLinhas, NumColunas, NewMatrix) :-
%     percorre_colunas(Matriz, LinhaAtual, 1, NumColunas, Partial),  % Percorre as colunas da linha atual
%     NovaLinha is LinhaAtual + 1,
%     percorre_linhas(Matriz, NovaLinha, NumLinhas, NumColunas).

% % Percorre as colunas de uma linha específica
% percorre_colunas(_, _, ColunaAtual, NumColunas, NewMatrix) :- ColunaAtual > NumColunas, !. % Condição de parada
% percorre_colunas(Matriz, LinhaAtual, ColunaAtual, NumColunas, NewMatrix) :-
%     cellUpdate([LinhaAtual, ColunaAtual], Matrix, Cell),
%     put(LinhaAtual, ColunaAtual, Cell, Matrix, NewMatrix),
%     NovaColuna is ColunaAtual + 1,
%     percorre_colunas(Matriz, LinhaAtual, NovaColuna, NumColunas, NewMatrix).


% Dada uma célula qualquer da matrix, atribui a Cell o próximo estado da mesma.
cellUpdate([X, Y], Matrix, Cell):-
    get(Matrix, X, Y, Value),
    (Value = "dead" -> deadUpdateCell([X , Y], Matrix, Cell)
    ; liveUpdateCell([X, Y], Matrix, Cell)).


% Dada uma posição da matriz com uma célula viva, atribui a Name a célula do próximo estágio
liveUpdateCell([X, Y], Matrix, Name):-
    
    get(Matrix, X, Y, Value),
    numOfLiveNeighbors([X, Y], Matrix, NumNeighbors),
    cell:cell(Value, _, StayRule, _),

    (member(NumNeighbors, StayRule) -> Name = Value
    ; Name = "dead").


deadUpdateCell([X, Y], Matrix, Cell):-
    numOfLiveNeighbors([X, Y], Matrix, NumNeighbors),
    lifeCellsCoord([X, Y], Matrix, CoordLiveNeighbors),
    coordsProposedRules(CoordLiveNeighbors, NumNeighbors, Matrix, CoordsRules),
    frequencyCells(CoordsRules, CoordsRules, Matrix, Frequenty),

    (CoordsRules = [] -> Cell = "dead"
    ; biggestOnList(Frequenty, [_, Cell])).


% Retorna o elemento que possui a maior frequência na lista de frequência gerada por frequency cells.
biggestOnList([[Qnt, Cell]], [Qnt, Cell]):- !.
biggestOnList([[Qnt1, Cell1]|T], [X, Y]):-
    biggestOnList(T, [Qnt2, Cell2]),
    (Qnt1 >= Qnt2 -> X = Qnt1, Y = Cell1
    ; X = Qnt2, Y = Cell2).


% Atribui a Out uma lista de tuplas com a frequência e o termo ao qual se refere
frequencyCells([], _, _, []).
frequencyCells([[X, Y]|T], Coords, Matrix, Out):-
    frequencyCells(T, Coords, Matrix, Partial),
    get(Matrix, X, Y, Name),
    numTimesFoundCell(Name, Coords, Matrix, Num),

    add_to_set([Num, Name], Partial, Out).


% Atribui a variável Qnt a quantidade de vezes que determinada célula aparece em uma sequência de coordenadas.
numTimesFoundCell(_, [], _, 0).
numTimesFoundCell(Name, [[X, Y]|T], Matrix, Qnt):-
    numTimesFoundCell(Name, T, Matrix, Sum),
    get(Matrix, X, Y, Value),
    (Name = Value -> Qnt is Sum + 1
    ; Qnt = Sum).


add_to_set(X, Set, Set) :- member(X, Set), !.
add_to_set(X, Set, [X|Set]).


% Atribui a Out as coordenadas daquelas células que possuem regras que assumem o nascimento
% para determinada posição de célula morta.
coordsProposedRules([], _, _, []):- !.
coordsProposedRules([[X, Y]|T], Num, Matrix, Out):-
    
    coordsProposedRules(T, Num, Matrix, NewOut),

    get(Matrix, X, Y, Name),

    cell:cell(Name, _, _, BirthRule),

    (member(Num, BirthRule) -> append([[X, Y]], NewOut, Out)
    ; Out = NewOut).

% Atribui a variável NumNeighbors a quantidade de vizinhos vivos ao redor de uma coordenada
% (x,y).
numOfLiveNeighbors([X, Y], Matrix, NumNeighbors):-
    lifeCellsCoord([X, Y], Matrix, List),
    length(List, NumNeighbors).


% Dada uma coordenada e uma Matrix, atribui a variável Out, uma lista com as coordenadas
% das células vivas em volta da mesma.
lifeCellsCoord(Coord, Matrix, Out):-
    listOfCoord(Coord, List),
    validCoords(List, Matrix, ValidCoords),
    lifeCellsCoordRecur(ValidCoords, Matrix, Out).


lifeCellsCoordRecur([], _, []).
lifeCellsCoordRecur([[X, Y]|T], Matrix, List):-
    lifeCellsCoordRecur(T, Matrix, Parcial),
    (isAlive(X, Y, Matrix) -> append([[X, Y]], Parcial, List)
    ; List = Parcial).


% Este fato se valida caso a célula na posição (x,y), não seja uma célula morta.
isAlive(X, Y, Matrix):-
    get(Matrix, X, Y, Name),
    not(Name = "dead").


% Retorna uma lista com as coordenadas válidas dado um arranjo de possíveis
% coordenadas de uma matriz.
validCoords([], _, []).
validCoords([H|T], Matrix, Out):-
    validCoords(T, Matrix, Parcial),
    (validCoord(H, Matrix) -> append([H], Parcial, Out)
    ; Out = Parcial).


% Verifica se uma determinada coordenada está dentro dos limites de uma matriz.
validCoord([X, Y], Matrix):-
    matrixSize(Matrix, Rows, Cols),
    X >= 0,
    X < Rows,
    Y >= 0,
    Y < Cols.


% Retorna as 8 coordenadas vizinhas de uma coordenada (x, y).
listOfCoord(Coord, List):-
    coordOnTop(Coord, A),
    coordInBelow(Coord, B),
    coordInRight(Coord, C),
    coordInLeft(Coord, D),
    coordOnTopRight(Coord, E),
    coordInBelowRight(Coord, F),
    coordOnTopLeft(Coord, G),
    coordInBelowLeft(Coord, H),

    List = [A, B, C, D, E, F, G, H].




% Só existe por consistência com o get, Matrix.put(X/Y, Value) é bem mais ergonômico, mas talvez te levasse a fazer 
% Matrix.get() que não tem verificação de limites, diferente do meu get.
put(X, Y, Value, Matrix, Out) :- Out = Matrix.put(X/Y, Value).


put([], Matrix, _, Matrix).
put([[X, Y]|T], Matrix, Value, Out):-
    put(T, Matrix, Value, Parcial),
    put(X, Y, Value, Parcial, Out).


createArrayDict(0, _, _{}):-!.
createArrayDict(Size, Value, Dict) :-
    N is Size - 1,
    createArrayDict(N, Value, Partial),
    Dict = Partial.put(N, Value).


createMatrix(0, _, _, _{}):-!.
createMatrix(Lines, Cols, Value, Matrix):-
    N is Lines - 1,
    createArrayDict(Cols, Value, Line),
    createMatrix(N, Cols, Value, Partial),
    Matrix = Partial.put(N, Line).


createSquareMatrix(N, Value, Matrix):- createMatrix(N, N, Value, Matrix).


% Retorna o valor naquele ponto na matrix, se a posição for inválida retorna uma célula morta.
get(Matrix, X, Y, Value):-
    X > -1,
    Y > -1,
    dictSize(Matrix, Lines),
    X < Lines,
    dictSize(Matrix.0, Cols),
    Y < Cols, % Estruturado dessa forma pra tentar salvar perfomance não fazendo testes desnecessários
    Value = Matrix.get(X/Y), !.
% get(Matrix, X, Y, dead) :-
%     (X < 0; Y < 0);
%     (dictSize(Matrix.0, Cols),
%     dictSize(Matrix, Lines),
%     (Y > Lines ; X > Cols)).
get(_, _, _, "dead").




removeCell([X, Y], Matrix, Out):- put(X, Y, "dead", Matrix, Out).

removeCells(List, Matrix, Out):- put(List, Matrix, "dead", Out).


% Pega parte de uma Linha X da Matrix, do indice YStart até YEnd -1.
% A lista retornada é em ordem decrescente.
spliceLine(X, YStart, YEnd, Matrix, [N | Out]):-
    YStart < YEnd,
    Y is YEnd -1,
    spliceLine(X, YStart, Y, Matrix, Out),
    get(Matrix, X, Y, N), !. % não entendi porque preciso desse corte, mas é a mesma coisa co ArrayDict dnv.
spliceLine(_, Y, Y, _, []).

% A lista retornada é em ordem decrescente.
getLine(X, Matrix, Out):-
    dict_size(Matrix.X, Y),
    spliceLine(X, 0, Y, Matrix, Out).


% Retorna uma lista com os 8 valores ao redor do ponto (X, Y).
% A ordem da lista é: [abaixo do ponto, acima, esquerda, direita].
getSquareAround(X, Y, Matrix, Out):-
    UpX is X - 1,
    DownX is X + 1,
    LeftY is Y - 1,
    RightY is Y + 1,
    LineLenght is Y + 2,

    spliceLine(UpX, LeftY ,LineLenght, Matrix, Up),
    spliceLine(DownX, LeftY, LineLenght, Matrix, Down),
    get(Matrix, X, LeftY, Left),
    get(Matrix, X, RightY, Right),

    Partial1 = [Left, Right],
    append(Up, Partial1, Partial2),
    append(Down, Partial2, Out).


% Atribui a variável Size, o tamanho do dicionário de entrada.
dictSize(Dict,Size) :-
   assertion(is_dict(Dict)),
   assertion(var(Size);integer(Size)),
   compound_name_arity(Dict,_,Arity),
   Size is (Arity-1)//2.


% Atribui as variáveis X e Y, em ordem a quantidade de linhas
% e colunas da matriz de entrada.
matrixSize(Matrix, X, Y) :-
    dictSize(Matrix, X),
    dictSize(Matrix.0, Y).


arrayToList(Array, List):- 
    dict_pairs(Array, _, L),
    pairs_values(L, List).


matrixToList(Matrix, List) :-
    arrayToList(Matrix, L),
    maplist(arrayToList,L , List).


matrixFromList([], _{}):- !.
matrixFromList(List, Matrix):-
    matrixFromList(List, 0, Matrix).


matrixFromList([], _, _{}).
matrixFromList([H|T], Ctt, Matrix):-
    NewCtt is Ctt + 1,
    matrixFromList(T, NewCtt, Partial),
    arrayFromList(H, Array),
    Matrix = Partial.put(Ctt, Array).


arrayFromList(List, Array):-
    length(List, Size),
    arrayFromList(0, Size, List, Array).
      

arrayFromList(Index, Index, [], _{}):- !.
arrayFromList(Index, Size, [H|T], Dict):-
    NextIndex is Index + 1,
    arrayFromList(NextIndex, Size, T, Parcial),
    Dict = Parcial.put(Index, H).


% Fatos para cálculo de coordenadas.

coordOnTop([X1, Y1], [X2, Y1]):- X2 is X1 - 1.

coordInBelow([X1, Y1], [X2, Y1]):- X2 is X1 + 1.

coordInRight([X1, Y1], [X1, Y2]):- Y2 is Y1 + 1.

coordInLeft([X1, Y1], [X1, Y2]):- Y2 is Y1 - 1.

coordOnTopRight([X1, Y1], [X2, Y2]):- X2 is X1 - 1, Y2 is Y1 + 1.

coordInBelowRight([X1, Y1], [X2, Y2]):- X2 is X1 + 1, Y2 is Y1 + 1.

coordOnTopLeft([X1, Y1], [X2, Y2]):- X2 is X1 - 1, Y2 is Y1 - 1.

coordInBelowLeft([X1, Y1], [X2, Y2]):- X2 is X1 + 1, Y2 is Y1 - 1.

% Testes

equals(X, Y) :- X = Y.

testDictSize:- 
    A = _{0:a, 1:b, 2:c},
    B = _{0:a, 1:b, 2:c, 3:d},
    C = _{},

    dictSize(A, ASize),
    dictSize(B, BSize),
    dictSize(C, CSize),


    equals(3, ASize),
    equals(4, BSize),
    equals(0, CSize).

testMatrixSize:-
    A = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:f, 2:g}, 
        2:_{0:h, 1:i, 2:j}
            },

    B = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:f, 2:g}, 
        2:_{0:h, 1:i, 2:j},
        3:_{0:k, 1:l, 2:m}
            },


    matrixSize(A, XA, YA),
    matrixSize(B, XB, YB),

    equals(3, XA), equals(3, YA),
    equals(4, XB), equals(3, YB).


testGet:- 
    A = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:cell:cell("dead", preto, [], []), 2:g}, 
        2:_{0:h, 1:i, 2:j}
            },

    get(A, 1, 1, Value11),
    get(A, 0, 0, Value00),
    get(A, 2, 2, Value22),

    equals(Value00, a),
    equals(Value11, cell:cell("dead",preto,[],[])),
    equals(Value22, j).


testListOfCoords:-
 

    listOfCoord([1, 1], [A, B, C, D, E, F, G, H]),


    equals([0,1], A),

    equals([2,1], B),

    equals([1,2], C),

    equals([1,0], D),

    equals([0,2], E),

    equals([2,2], F),

    equals([0,0], G),

    equals([2,0], H).


testValidCoord:-
     A = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:f, 2:g}, 
        2:_{0:h, 1:i, 2:j}
            },

    not(validCoord([-1, 0], A)),
    not(validCoord([0, 3], A)),
    not(validCoord([3, 0], A)),
    validCoord([0, 0], A),
    validCoord([1, 1], A),
    validCoord([2, 2], A).


testValidCoords:-

    A = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:f, 2:g}, 
        2:_{0:h, 1:i, 2:j}
            },

    listOfCoord([0, 0], Coords),

    writeln(Coords),

    validCoords(Coords, A, CoordsValidas),

    writeln(CoordsValidas).


testUpdateLine:-

    createMatrix(3,3, "dead", A),

    cell:createCell(conways, "red", [2, 3], [3]),

    put(0, 1, conways, A, B),
    put(1, 1, conways, B, C),
    put(2, 1, conways, C, D),

    writeln(""),
    writeln(D),
    writeln(""),

    F = D.get(0),

    writeln(""),
    writeln(F),
    writeln(""),

    updateLine(D, 0, F, 0, 3, NewLine),

    writeln(NewLine).


testIsAlive:-

    createMatrix(3,3, "dead", A),

    Leo = cell:cell(leo, _, _, _),
    Keven = cell:cell(keven, _, _, _),

    put(1, 1, Keven, A, B),
    put(2, 1, Leo, B, C),

    isAlive(1, 1, C),
    isAlive(2, 1, C),
    not(isAlive(0, 0, C)),
    not(isAlive(0, 1, C)).


testLifeCellsCoord:-

    createMatrix(3,3, "dead", A),

    Leo = cell:cell(leo, _, _, _),
    Keven = cell:cell(keven, _, _, _),

    put(1, 1, Keven, A, B),
    put(2, 1, Leo, B, C),
    put(0, 2, Keven, C, D),

    lifeCellsCoord([1, 1], D, Out),

    equals([[2,1],[0,2]], Out).

testNumOfLiveNeighbors:-
    
    createMatrix(3,3, "dead", A),

    Leo = cell:cell(leo, _, _, _),
    Keven = cell:cell(keven, _, _, _),

    put(1, 1, Keven, A, B),
    put(2, 1, Leo, B, C),
    put(0, 2, Keven, C, D),

    numOfLiveNeighbors([1, 1], D, Out1),

    equals(2, Out1),

    removeCell([1,1], D, E),

    numOfLiveNeighbors([0, 0], E, Out2),

    equals(0, Out2).

testcoordsProposedRules:-

    createMatrix(3,3, "dead", A),

    cell:createCell("keven", "green", [1], [1, 4]),
    cell:createCell("leo", "red", [1], [4, 3]),
    cell:createCell("ramon", "red", [1], [7]),


    put(0, 1, "keven", A, B),
    put(2, 1, "leo", B, C),
    put(1, 2, "leo", C, D),
    put(0, 0, "ramon", D, E),

    writeln(E),

    numOfLiveNeighbors([1, 1], E, NumNeighbors),

    writeln(""),
    writeln(NumNeighbors),
    writeln(""),

    lifeCellsCoord([1, 1], E, CoordLiveNeighbors),
    writeln(CoordLiveNeighbors),
    writeln(""),

    coordsProposedRules(CoordLiveNeighbors, NumNeighbors, E, CoordsRules),

    writeln(CoordsRules).


testfrequencyCells:-

    createMatrix(3,3, "dead", A),

    cell:createCell("keven", "green", [1], [1, 4]),
    cell:createCell("leo", "red", [1], [4, 3]),
    cell:createCell("ramon", "red", [1], [7]),

    put(0, 1, "keven", A, B),
    put(2, 1, "leo", B, C),
    put(1, 2, "leo", C, D),
    put(0, 0, "ramon", D, E),

    writeln(""),
    writeln(E),
    writeln(""),

    numOfLiveNeighbors([1, 1], E, NumNeighbors),

    lifeCellsCoord([1, 1], E, CoordLiveNeighbors),

    coordsProposedRules(CoordLiveNeighbors, NumNeighbors, E, CoordsRules),

    frequencyCells(CoordsRules, CoordsRules, E, Frequenty),

    writeln(Frequenty).


testbiggestOnList:-

    createMatrix(3,3, "dead", A),

    cell:createCell("keven", "green", [1], [1, 4]),
    cell:createCell("leo", "red", [1], [4, 3]),
    cell:createCell("ramon", "red", [1], [7]),

    put(0, 1, "keven", A, B),
    put(2, 1, "leo", B, C),
    put(1, 2, "leo", C, D),
    put(0, 0, "ramon", D, E),

    writeln(""),
    writeln(E),
    writeln(""),

    numOfLiveNeighbors([1, 1], E, NumNeighbors),

    lifeCellsCoord([1, 1], E, CoordLiveNeighbors),

    coordsProposedRules(CoordLiveNeighbors, NumNeighbors, E, CoordsRules),

    frequencyCells(CoordsRules, CoordsRules, E, Frequenty),

    biggestOnList(Frequenty, Big),

    writeln(Big).


testMatrixUpdate:-

    createMatrix(3,3, "dead", A),


    cell:createCell("conways", "red", [2, 3], [3]),

    put(0, 1, "conways", A, B),
    put(1, 1, "conways", B, C),
    put(2, 1, "conways", C, D),

    writeln(""),
    writeln(D),
    writeln(""),

    writeln("HIT"),

    matrixUpdate(D, E),

    writeln(""),
    writeln(E),
    writeln(""),

    matrixUpdate(E, F),

    writeln(""),
    writeln(F),
    writeln("").
    
testCellUpdate:-

    createMatrix(3,3, "dead", A),

    cell:createCell("conways", "red", [2, 3], [2]),

    put(0, 1, "conways", A, B),
    put(1, 1, "conways", B, C),
    put(2, 1, "conways", C, D),

    writeln(D),

    liveUpdateCell([1,1], D, Mid),

    writeln(Mid),

    liveUpdateCell([0,1], D, Top),
    liveUpdateCell([2,1], D, Down),

    writeln(Top),
    writeln(Down).


testliveUpdateCell:-

    createMatrix(3,3, "dead", A),

    cell:createCell("keven", "green", [2], [1, 4]),
    cell:createCell("leo", "red", [1], [4, 3]),

    put(1, 1, "keven", A, B),

    writeln(""),
    writeln(B),
    writeln(""),

    liveUpdateCell([1, 1], B, NewKeven),

    writeln(NewKeven),

    put(0, 0, "keven", B, C),
    put(2, 2, "keven", C, D),

    writeln(""),
    writeln(D),
    writeln(""),


    liveUpdateCell([1, 1], D, NewKeven2),

    writeln(""),
    writeln(NewKeven2),
    writeln("").


testdeadUpdateCell:- 

    createMatrix(3,3, "dead", A),

    cell:createCell("keven", "green", [1], [1, 4]),
    cell:createCell("leo", "red", [1], [4, 3]),
    cell:createCell("ramon", "red", [1], [7]),

    put(0, 1, "keven", A, B),
    put(2, 1, "leo", B, C),
    put(1, 2, "leo", C, D),
    put(0, 0, "ramon", D, E),

    writeln(""),
    writeln(E),
    writeln(""),

    deadUpdateCell([1, 1], E, NewCell),

    writeln(NewCell).


testnumTimesFoundCell:-
    
    createMatrix(3,3, "dead", A),

    cell:createCell("keven", "green", [1], [1, 4]),
    cell:createCell("leo", "red", [1], [4, 3]),
    cell:createCell("ramon", "red", [1], [7]),


    put(0, 1, "keven", A, B),
    put(2, 1, "leo", B, C),
    put(1, 2, "leo", C, D),
    put(0, 0, "ramon", D, E),

    writeln(E),

    numOfLiveNeighbors([1, 1], E, NumNeighbors),


    lifeCellsCoord([1, 1], E, CoordLiveNeighbors),

    coordsProposedRules(CoordLiveNeighbors, NumNeighbors, E, CoordsRules),

    numTimesFoundCell("leo", CoordsRules, E, Qnt),

    writeln(""),
    writeln(Qnt).

% test :-
%     createSquareMatrix(5, cu, Matrix),
%     % getLine(1, 5, Matrix, Out),
%     % writeln(Out),
%     getSquareArounB(0, 0, Matrix, Square),
%     getSquareAround(2, 2, Matrix, Square2),
%     writeln(Square),
%     writeln(Square2),
%     append([2], Square2, S),
%     writeln(S).


% test2:-
%     A = _{
%         0:_{0:a, 1:b, 2:c}, 
%         1:_{0:d, 1:f, 2:g}, 
%         2:_{0:h, 1:i, 2:j}
%             },
%     % spliceLine(1, 0, 3, A, Out),
%     put(4, 1, cuzinho, A, Out),
%     writeln(Out).


test3 :- 
    A = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:f, 2:g}, 
        2:_{0:h, 1:i, 2:j}
            },
    arrayToList(A, L),
    writeln(L),
    matrixToList(A, Re),
    writeln(Re),
    json_write(current_output, json([name=test, matrix=Re])).


test4 :- 
    A = _{0:a, 1:b, 2:c},
    arrayToList(A, L),
    writeln(L),
    arrayFromList(L, Array),
    writeln(Array).


test5 :-
    A = [[a, b, c], [d, e, f], [h, i, j]],

    matrixFromList(A, B),

    writeln(B).

% Teste de put em uma Matrix

test6 :-
    createMatrix(3, 3, "dead", Matrix),

    writeln(Matrix),

    _coords = [[0,0], [1,1], [2,2]],

    Leo = cell:cell(leo, _, _, _),

    put(_coords, Matrix, Leo, NewMatrix),

    writeln(""),

    writeln(NewMatrix),

    removeCell([0, 0], NewMatrix, Out),

    writeln(Out).

% Teste de Coordenadas válidas
test7:-

    createMatrix(3, 3, "dead", Matrix),
    writeln(Matrix),
    writeln("").