:-use_module(matrix).

main :-
    matrix:createSquareMatrix(5, cu, M),
    % matrix:get(M, 3,3, V),
    % write(V),
    matrix:getLine(1, 0, M, S),
    write(S).