:-module(cellController, []).
:-use_module("../src/Files.pl").
:-use_module("../Utils/Render.pl").
:-use_module("../src/Cell.pl").

main:-
    render:clearScreen,
    render:printEmptyLines(17),
    render:printMid("CELULAS:"),
    writeln(""),
    files:getCells,
    % render:setCursorColumn(95),
    % files:saveCells,
    % prints:toStringCell(Cell),
    render:listCellsPrints,
    render:printEmptyLines(2),
    render:printMid(" 1) ADICIONAR CELULA   2) DELETAR CELULA   3) VOLTAR"),
    read_line_to_string(user_input, Option),
    option(Option, "../storage/cells.json").

% Inicio do procedimento de criação da Celula, recebendo o nome da Celula e chamando a função
% para adicionar a regra de nascimento
addAutomata(Path):-
    render:printScreen("../storage/ruleController/nameCellQuestion.txt"),

    render:setCursorColumn(85),
    read_line_to_string(user_input, CellName),

    addBirthRule(CellName, Path).
% Criação da Regra de Nascimento da Celula, o usuario pode inserir de 0 a 8 digitos entre 1 e 8.
% (tem que ver se os digitos validos pra criar celula ainda são esses, se não forem, alterar em isAllValidBirthDigits)
addBirthRule(CellName, Path):-
    render:printScreen("../storage/ruleController/birthRule.txt"),
    
    render:setCursorColumn(85),
    read_line_to_string(user_input, BirthRule),

    (handleBirthRule(BirthRule) ->
        nascList(BirthRule, BirthList),
        addStayRule(Path, CellName, BirthList)
        ;
        render:printScreen("../storage/ruleController/birthRuleError.txt"),
        sleep(0.8),
        addBirthRule(CellName, Path)
    ).

% Criação da Regra de Permanencia da Celula, o usuario pode inserir de 0 a 8 digitos entre 1 e 8.
% (tem que ver se os digitos validos pra criar celula ainda são esses, se não forem, alterar em isAllValidStayDigits)
addStayRule(Path, CellName, BirthList):-
    render:printScreen("../storage/ruleController/stayRule.txt"),

    render:setCursorColumn(85),
    read_line_to_string(user_input, StayRule),
    (handleStayRule(StayRule) ->
        stayList(StayRule, StayList),
        addColor(Path, CellName, BirthList, StayList)
        ;
        render:printScreen("../storage/ruleController/stayRuleError.txt"),
        sleep(0.8),
        addBirthRule(CellName, Path)
    ).

% Criação da cor da celula, tem que alterar o Color Menu que imprime na tela (ta o antigo de haskell)
addColor(Path, CellName, BirthList, StayList):-
    render:printScreen("../storage/ruleController/colorMenu.txt"), %tem que alterar o Color Menu

    render:setCursorColumn(85),
    read_line_to_string(user_input, Color),
    (cell:isValidColor(Color) ->
        cell:createCell(CellName, Color, StayList, BirthList),
        files:saveCells,
        main
        ;
        render:printScreen("../storage/ruleController/colorMenuError.txt"),
        sleep(0.8),
        addColor(Path, CellName, BirthList, StayList)
    ).

% menu de exclusao de celula por nome, ta funcional, mas precisa deixar bonito os prints
% alem de que ele só considera as celulas que foram criadas em Cell.pl, e nao as que estao em cell.json
removeAutomata(Path):-
    cell:listCellNames(Cells),
    (Cells = [] ->
        write('esta vazio'),
        main
    ;
        render:printScreen("../storage/ruleController/listOfCells.txt"),
        render:printEmptyLines(2),
        render:setCursorColumn(95),
        files:saveCells,
        % render:printMidScreen(Celulas),
        render:printEmptyLines(1),
        render:printScreen("../storage/ruleController/removeCellMenu.txt"),
        render:setCursorColumn(85),
        read_line_to_string(user_input, CellName),
        cell:deleteCell(CellName),
        files:saveCells,
        main
    ).

% verifica se nao tem espaço em branco, se tudo é numero entre 1 e 8, se o length <= 8 e tira digitos repetidos
handleStayRule(StayRule):-
    string_chars(StayRule, Chars),
    exclude(=( ' ' ), Chars, RegraFormatada),
    length(RegraFormatada, Length),
    Length =< 8,
    isAllValidStayDigits(RegraFormatada).

% verifica se nao tem espaço em branco, se tudo é numero entre 0 e 8, se o length <= 8 e tira digitos repetidos
handleBirthRule(BirthRule) :-
    string_chars(BirthRule, Chars),
    exclude(=( ' ' ), Chars, RegraFormatada),
    length(RegraFormatada, Length),
    Length =< 8,
    isAllValidBirthDigits(RegraFormatada).

% predicado auxiliar pra verificar se o char é digito
isDigit(Char) :-
    char_type(Char, digit).

% predicado auxiliar pra transformar char em int
charToInt(Char, Int) :-
    atom_number(Char, Int).

% faz aquele mapeamento de 123 virar [1,2,3]
stayList(StayRule, StayList) :-
    string_chars(StayRule, Chars),
    include(isDigit, Chars, DigitChars),
    maplist(charToInt, DigitChars, StayListNoDuplicates),
    list_to_set(StayListNoDuplicates, StayList).

% faz aquele mapeamento de 123 virar [1,2,3]
nascList(BirthRule, BirthList) :-
    string_chars(BirthRule, Chars),
    include(isDigit, Chars, DigitChars),
    maplist(charToInt, DigitChars, BirthListNoDuplicates),
    list_to_set(BirthListNoDuplicates, BirthList).

% Verifica se todos os caracteres são números entre '1' e '8'
isAllValidBirthDigits([]).
isAllValidBirthDigits([H|T]) :-
    member(H, ['1', '2', '3', '4', '5', '6', '7', '8']),
    isAllValidBirthDigits(T).

% Verifica se todos os caracteres são números entre '0' e '8'
isAllValidStayDigits([]).
isAllValidStayDigits([H|T]) :-
    member(H, ['0','1', '2', '3', '4', '5', '6', '7', '8']),
    isAllValidStayDigits(T).

option("1", Path):- addAutomata(Path).
option("2", Path):- removeAutomata(Path).
option("3", Path):- !.
option(_, Path).