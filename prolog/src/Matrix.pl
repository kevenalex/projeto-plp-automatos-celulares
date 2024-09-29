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