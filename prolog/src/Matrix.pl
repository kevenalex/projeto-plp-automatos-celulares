:- module(matrix, []).
:- use_module(library(http/json)).

% Um dicionário fingindo ser um array, não um array de dicionários. Preenchido com Value, Size vezes.
% Se eu não cortar aqui, ele diz que Dict = ao dicionario esperado 
% e da mais uma possibilidade vazia, bem estranho.
% 0 indexado, desculpa keven é mais complicado e devagar terminar em 1 e ter Tamanho valores inves de Tamanho -1
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

% retorna o valor naquele ponto na matrix, se a posição for inválida retorna uma célula morta.
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
get(_, _, _, dead).


% Só existe por consistência com o get, Matrix.put(X/Y, Value) é bem mais ergonômico, mas talvez te levasse a fazer 
% Matrix.get() que não tem verificação de limites, diferente do meu get.
put(X, Y, Value, Matrix, Out) :- Out = Matrix.put(X/Y, Value).


% Pega parte de uma Linha X da Matrix, do indice YStart até YEnd -1.
% A lista retornada é em ordem decrescente.
spliceLine(X, YStart, YEnd, Matrix, [N | Out]):-
    YStart < YEnd,
    Y is YEnd -1,
    spliceLine(X, YStart, Y, Matrix, Out),
    get(Matrix, X, Y, N), !. % não entendi porque preciso desse corte, mas é a mesma coisa co ArrayDict dnv.
spliceLine(_, Y, Y, _, []).


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


dictSize(Dict,Size) :-
   assertion(is_dict(Dict)),
   assertion(var(Size);integer(Size)),
   compound_name_arity(Dict,_,Arity),
   Size is (Arity-1)//2.


matrixSize(Matrix, X, Y) :-
    dictSize(Matrix, X),
    dictSize(Matrix.0, Y).


arrayToList(Array, List):- 
    dict_pairs(Array, _, L),
    pairs_values(L, List).


matrixToList(Matrix, List) :-
    arrayToList(Matrix, L),
    maplist(arrayToList,L , List).




% test :-
%     createSquareMatrix(5, cu, Matrix),
%     % getLine(1, 5, Matrix, Out),
%     % writeln(Out),
%     getSquareAround(0, 0, Matrix, Square),
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

