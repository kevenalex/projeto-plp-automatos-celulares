:-module(cellController, []).
:-use_module("../src/Files.pl").
:-use_module("../Utils/Render.pl").
:-use_module("../src/Cell.pl").

menuCells(FilePath):-
    % Cells is cell:listCells(Cells),
    % render:printMidScreen(Cells),
    read(Choice),
    choiceMenuCells(Choice, FilePath).
    
choiceMenuCells(1, FilePath):- addAutomata(FilePath).
choiceMenuCells(2, FilePath):- addAutomata(FilePath).
choiceMenuCells(3, FilePath):- addAutomata(FilePath).
choiceMenuCells(_, FilePath):- menuCells(FilePath).

% teste1:-
%     cell:createCell(cu, "azul", [1], [2]),
%     files:saveCells,
%     files:getCells,
%     % cell:createCell(GameOfLife, "green", [2], [2,3]),
%     menuCells("../storage/cells.json").

main:-
    A = _{
        0:_{0:a, 1:b, 2:c}, 
        1:_{0:d, 1:f, 2:g}, 
        2:_{0:h, 1:i, 2:j}
            },
    files:saveScene(cu, A),
    files:getSceneMatrix(cu, R),
    writeln(R).