:-module(cellController, []).
:-use_module("../src/Files.pl").
:-use_module("../Utils/Render.pl").
:-use_module("../src/Cell.pl").

% Prints do menu principal do CellController
main:-
    render:clearScreen,
    render:printEmptyLines(17),
    render:printMid("CELULAS:"),
    writeln(""),
    files:getCells,
    render:listCellsPrints,
    render:printEmptyLines(2),
    render:printMid(" 1) ADICIONAR CELULA   2) DELETAR CELULA   3) VOLTAR"),
    read_line_to_string(user_input, Option),
    option(Option).

% Inicio do procedimento de criação da Celula, recebendo o nome da Celula
% e chamando a função para adicionar a regra de nascimento.
addAutomata:-
    render:clearScreen,
    render:printScreen("../storage/ruleController/nameCellQuestion.txt"),

    render:setCursorColumn(85),
    read_line_to_string(user_input, CellName),

    addBirthRule(CellName).

% Criação da Regra de Nascimento da Celula, o usuario pode inserir 
% de 0 a 8 digitos entre 1 e 8.
addBirthRule(CellName):-
    render:clearScreen,
    render:printScreen("../storage/ruleController/birthRule.txt"),
    
    render:setCursorColumn(85),
    read_line_to_string(user_input, BirthRule),

    (handleBirthRule(BirthRule) ->
        nascList(BirthRule, BirthList),
        addStayRule(CellName, BirthList)
        ;
        render:printScreen("../storage/ruleController/birthRuleError.txt"),
        sleep(2),
        addBirthRule(CellName)
    ).

% Criação da Regra de Permanencia da Celula, o usuario pode inserir de
% 0 a 8 digitos entre 1 e 8.
addStayRule(CellName, BirthList):-
    render:clearScreen,
    render:printScreen("../storage/ruleController/stayRule.txt"),

    render:setCursorColumn(85),
    read_line_to_string(user_input, StayRule),
    (handleStayRule(StayRule) ->
        stayList(StayRule, StayList),
        addColor(CellName, BirthList, StayList)
        ;
        render:printScreen("../storage/ruleController/stayRuleError.txt"),
        sleep(2),
        addStayRule(CellName, BirthList)
    ).

% Escolha da cor da celula.
addColor(CellName, BirthList, StayList):-
    render:clearScreen,
    render:printScreen("../storage/ruleController/colorMenu.txt"), %tem que alterar o Color Menu

    render:setCursorColumn(85),
    read_line_to_string(user_input, Color),
    (cell:isValidColor(Color) ->
        cell:createCell(CellName, Color, StayList, BirthList),
        files:saveCells,
        main
        ;
        render:printScreen("../storage/ruleController/colorMenuError.txt"),
        sleep(0.5),
        addColor(CellName, BirthList, StayList)
    ).

% Menu de exclusao de celula por nome.
removeAutomata:-
    render:clearScreen,
    cell:listCellNames(Cells),
    (Cells = [] ->
        main
    ;
        render:printEmptyLines(17),
        render:printMid(" CELULAS EXISTENTES:"),
        render:printEmptyLines(1),
        render:listCellsPrints,
        render:printEmptyLines(2),
        render:printMid(" QUAL O NOME DA CELULA QUE VOCE QUER REMOVER?"),
        render:setCursorColumn(85),

        read_line_to_string(user_input, CellName),
        cell:deleteCell(CellName),
        files:saveCells,
        main
    ).

% Tratamento da regra de sobrevivência, verifica se não tem espaços em branco,
% se todos os caracteres são numeros entre 1 e 8, se o length <= 8 e remove 
% digitos repetidos.
handleStayRule(StayRule):-
    string_chars(StayRule, Chars),
    exclude(=( ' ' ), Chars, RegraFormatada),
    length(RegraFormatada, Length),
    Length =< 8,
    isAllValidStayDigits(RegraFormatada).

% Tratamento da regra de nascimento, verifica se não tem espaços em branco, 
% se todos os caracteres são numeros entre 0 e 8, se o length <= 8 e remove 
% digitos repetidos.
handleBirthRule(BirthRule) :-
    string_chars(BirthRule, Chars),
    exclude(=( ' ' ), Chars, RegraFormatada),
    length(RegraFormatada, Length),
    Length =< 8,
    isAllValidBirthDigits(RegraFormatada).

% Predicado auxiliar para verificar se o Char é digito.
isDigit(Char) :-
    char_type(Char, digit).

% Predicado auxiliar para transformar Char em Int.
charToInt(Char, Int) :-
    atom_number(Char, Int).

% Faz o mapeamento para sequencia de números virarem elementos de uma lista
% na regra de sobrevivência.
stayList(StayRule, StayList) :-
    string_chars(StayRule, Chars),
    include(isDigit, Chars, DigitChars),
    maplist(charToInt, DigitChars, StayListNoDuplicates),
    list_to_set(StayListNoDuplicates, StayList).

% Faz o mapeamento para sequencia de números virarem elementos de uma lista
% na regra de nascimento.
nascList(BirthRule, BirthList) :-
    string_chars(BirthRule, Chars),
    include(isDigit, Chars, DigitChars),
    maplist(charToInt, DigitChars, BirthListNoDuplicates),
    list_to_set(BirthListNoDuplicates, BirthList).

% Predicado auxiliar que verifica se todos os caracteres são valores entre '1' e '8'.
isAllValidBirthDigits([]).
isAllValidBirthDigits([H|T]) :-
    member(H, ['1', '2', '3', '4', '5', '6', '7', '8']),
    isAllValidBirthDigits(T).

% Predicado auxiliar que verifica se todos os caracteres são valores entre '0' e '8'.
isAllValidStayDigits([]).
isAllValidStayDigits([H|T]) :-
    member(H, ['0','1', '2', '3', '4', '5', '6', '7', '8']),
    isAllValidStayDigits(T).

% Tratamento de erro no menu.
option("1"):- addAutomata.
option("2"):- removeAutomata.
option("3"):- !.
option(_):- main.