:-module(cellController, []).
:-use_module("../src/Files.pl").
:-use_module("../Utils/Render.pl").
:-use_module("../src/Cell.pl").

menuCells(FilePath):-
    read_line_to_string(user_input, Choice),
    option(Choice, FilePath).

addAutomata(FilePath):-
    render:printScreen("../app/storage/ruleController/nameCellQuestion.txt"),

    render:setCursorColumn(85),
    read_line_to_string(user_input, CellName),

    addBirthRule(CellName, FilePath).

addBirthRule(CellName, FilePath):-
    render:printScreen("../app/storage/ruleController/birthRule.txt"),
    
    render:setCursorColumn(85),
    read_line_to_string(user_input, BirthRule),

    (handleBirthRule(BirthRule) ->
        nascList(BirthRule, BirthList),
        addStayRule(FilePath, CellName, BirthList)
        ;
        render:printScreen("../app/storage/ruleController/birthRuleError.txt"),
        sleep(0.8),
        addBirthRule(CellName, FilePath)
    ).

addStayRule(FilePath, CellName, BirthList):-
    render:printScreen("../app/storage/ruleController/stayRule.txt"),

    render:setCursorColumn(85),
    read_line_to_string(user_input, StayRule),
    (handleStayRule(StayRule) ->
        stayList(StayRule, StayList),
        addColor(FilePath, CellName, BirthList, StayList)
        ;
        render:printScreen("../app/storage/ruleController/stayRuleError.txt"),
        sleep(0.8),
        addBirthRule(CellName, FilePath)
    ).

addColor(FilePath, CellName, BirthList, StayList):-
    render:printScreen("../app/storage/ruleController/colorMenu.txt"), %tem que alterar o Color Menu

    render:setCursorColumn(85),
    read_line_to_string(user_input, Color),
    (cell:isValidColor(Color) ->
        cell:createCell(CellName, Color, StayList, BirthList),
        files:saveCells
        ;
        render:printScreen("../app/storage/ruleController/colorMenuError.txt"),
        sleep(0.8),
        addColor(FilePath, CellName, BirthList, StayList)
    ).

handleStayRule(StayRule):-
    string_chars(StayRule, Chars),
    exclude(=( ' ' ), Chars, RegraFormatada),
    length(RegraFormatada, Length),
    Length =< 8,
    isAllValidStayDigits(RegraFormatada).

handleBirthRule(BirthRule) :-
    string_chars(BirthRule, Chars),
    exclude(=( ' ' ), Chars, RegraFormatada),
    length(RegraFormatada, Length),
    Length =< 8,
    isAllValidBirthDigits(RegraFormatada).

isDigit(Char) :-
    char_type(Char, digit).

charToInt(Char, Int) :-
    atom_number(Char, Int).

stayList(StayRule, StayList) :-
    string_chars(StayRule, Chars),
    include(isDigit, Chars, DigitChars),
    maplist(charToInt, DigitChars, StayListNoDuplicates),
    list_to_set(StayListNoDuplicates, StayList).

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

option("1", FilePath):- addAutomata(FilePath).
option("2", FilePath):- removeAutomata(FilePath).
option("3", FilePath):- menuCells(FilePath).
option(_, FilePath):- menuCells(FilePath).

test:-
    % cell:createCell("teste", "blue", [1], [2]),
    % files:saveCells,
    % cell:createCell("teste2", "green", [1], [2]),
    % files:saveCells,
    % cell:createCell("teste3", "white", [1], [2]),
    % files:saveCells,
    % cell:listCellNames(R),
    % write(R),
    % cell:createCell(GameOfLife, "green", [2], [2,3]),
    menuCells("../storage/cells.json").