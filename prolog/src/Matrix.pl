:- module(matrix, []).

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
    dict_size(Matrix, Lines),
    X < Lines,
    dict_size(Matrix.0, Cols),
    Y < Cols, % Estruturado dessa forma pra tentar salvar perfomance não fazendo testes desnecessários
    Value = Matrix.get(X/Y), !.
% get(Matrix, X, Y, dead) :-
%     (X < 0; Y < 0);
%     (dict_size(Matrix.0, Cols),
%     dict_size(Matrix, Lines),
%     (Y > Lines ; X > Cols)).
get(_, _, _, dead).

% "Take" numa linha de uma matrix, pega de 0 até Y e coloca numa lista.
% Mesmo corte estranho do createArrayDict
getLine(_, 0, _, []):- !.
getLine(X, Y, Matrix, [N | Out]):-
    Y > 0,
    Y1 is Y - 1,
    getLine(X, Y1, Matrix, Out),
    get(Matrix, X, Y1, N).



% Retorna uma lista com os 8 valores ao redor do ponto (X, Y).
getSquareAround(X, Y, Matrix, Out):-
    UpX is X - 1,
    DownX is X + 1,
    LeftY is Y - 1,
    RightY is Y + 1,

    getLine(UpX, RightY, Matrix, Up),
    getLine(DownX, RightY, Matrix, Down),
    get(Matrix, X, LeftY, Left),
    get(Matrix, X, RightY, Right),

    Partial1 = [Left, Right],
    append(Up, Partial1, Partial2),
    append(Down, Partial2, Out).


dict_size(Dict,Size) :-
   assertion(is_dict(Dict)),
   assertion(var(Size);integer(Size)),
   compound_name_arity(Dict,_,Arity),
   Size is (Arity-1)//2.


test :-
    createSquareMatrix(5, cu, Matrix),
    % getLine(1, 5, Matrix, Out),
    % writeln(Out),
    getSquareAround(0, 0, Matrix, Square),
    getSquareAround(2, 2, Matrix, Square2),
    writeln(Square),
    writeln(Square2),
    append([2], Square2, S),
    writeln(S).