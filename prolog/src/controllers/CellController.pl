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

    handleBirthRule(BirthRule) ->
        write("OMG LOBO CUIDA PAPAI")
        
        ;
        render:printScreen("../app/storage/ruleController/birthRuleError.txt"),
        sleep(0.8),
        
        addBirthRule(CellName, FilePath).

handleBirthRule(BirthRule):-false.

option("1", FilePath):- addAutomata(FilePath).
option("2", FilePath):- menuCells(FilePath).
option("3", FilePath):- menuCells(FilePath).
option(_, FilePath):- menuCells(FilePath).

teste1:-
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