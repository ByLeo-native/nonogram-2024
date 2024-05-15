:- module(proylcc,
    [  
        put/8,
        solve/4,
        check_clues/5,
        check_line/3
    ]).

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the result of replacing the occurrence of X in position XIndex of Xs by Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%

put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat):-
	% NewGrid is the result of replacing the row Row in position RowN of Grid by a new row NewRow (not yet instantiated).
	replace(Row, RowN, NewRow, Grid, NewGrid),

	% NewRow is the result of replacing the cell Cell in position ColN of Row by _,
	% if Cell matches Content (Cell is instantiated in the call to replace/5).	
	% Otherwise (;)
	% NewRow is the result of replacing the cell in position ColN of Row by Content (no matter its content: _Cell).			
	(replace(Cell, ColN, _, Row, NewRow),
	Cell == Content
		;
	replace(_Cell, ColN, Content, Row, NewRow)
    ),
    check_position(NewGrid, [RowN,ColN], RowsClues, ColsClues, RowSat, ColSat).


check_clues(Grid, RowsClues, ColsClues, StatusOfTheRows, StatusOfTheCols) :-
    check_rows(Grid, RowsClues, StatusOfTheRows),
    check_cols(Grid, ColsClues, StatusOfTheCols).

%%%%%%%%%%%%%%%%%%%%%%%%%
check_rows([], [], []).
check_rows([Row | Rows], [Clue | Clues], [RowSat | RowsSatTail]) :-
    check_line(Row, Clue, RowSat),
    check_rows(Rows, Clues, RowsSatTail).

%%%%%%%%%%%%%%%%%%%%%%%%%
check_cols([], [], []).
check_cols(Grid, ColsClues, StatusOfTheCols):-
    transpose(Grid, TransposedGrid),
    check_rows(TransposedGrid, ColsClues, StatusOfTheCols).

%%%%%%%%%%%%%%%%%%%%%

check_position(Grid, [RowN, ColN], RowsClues, ColsClues, RowSat, ColSat) :-
    % Obtener la fila y columna especificadas
    nth0(RowN, Grid, Row),
    nth0(RowN, RowsClues, RowNClues), 

    % Verificar si se cumplen las restricciones en fila
    check_line(Row, RowNClues, RowSat),

    % Transponer la grilla para verificar las columnas
    transpose(Grid, TransposedGrid),

    % Obtener la columna correspondiente
    nth0(ColN, TransposedGrid, Col),
    nth0(ColN, ColsClues, ColNClues),

    % Verificar si se cumplen las restricciones en columna
    check_line(Col, ColNClues, ColSat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_line(Line, Clues, LineSat) :-
    check_line(Line, Clues, 0, 0, LineSat).

% Caso base: si tanto la línea como las restricciones están vacías, LineSat es 1.
check_line([], [], 0, 0, 1).

% Caso base: si la línea está vacía pero quedan restricciones por verificar, LineSat es 0.
check_line([], [Clue|_], _, _, 0) :- Clue > 0.

% Caso base: si quedan elementos en la línea pero no restricciones,
% seguir analizando la línea mientras se mantenga la cantidad de pintados.
check_line([Cell|LineTail], [], PaintCount, _, LineSat) :-
    % Verificar si el elemento actual de la línea es pintado ('#').
    (Cell == "#" ->
        % Si hay una celda pintada en la línea restante, LineSat es 0.
        LineSat = 0
    ;   
        check_line(LineTail, [], PaintCount, _, LineSat)
    ).

% Si quedan elementos en la línea y restricciones por verificar.
check_line([Cell|LineTail], [Clue|CluesTail], PaintCount, _, LineSat) :-
    % Verificar si el primer elemento de la línea es '#' (pintado) o 'X' (no pintado).
    Clue > 0,
    (Cell == "#", NewPaintCount is PaintCount + 1 ; 
    Cell == "X", NewPaintCount is 0 ;
    var(Cell), NewPaintCount is 0),
    % Verificar si se ha alcanzado la restricción actual.
    (NewPaintCount == Clue ->
        % Si se alcanzó la restricción actual, continuar verificando con las restricciones restantes.
        check_line(LineTail, CluesTail, 0, 0, LineSat)
    ;   % Si no se alcanzó la restricción actual, continuar verificando con la misma restricción actual.
        check_line(LineTail, [Clue|CluesTail], NewPaintCount, 1, LineSat)
    ).

%%%%%%%%%%%%%%%% Contemplacion para cuando la restriccion de la linea es 0

% Caso base: si la restricción actual es 0 y la línea está vacía,
% se considera una línea válida.
check_line([], [0|_], 0, _, 1).

% Caso base: si la restricción actual es 0 y la línea no está vacía,
% entonces si la celda esta pintada ('#') entonces no cumple con la restricción de la línea.
check_line([Cell|_], [0|_], _, _, LineSat) :- Cell == "#", LineSat = 1.

% Caso recursivo: si la restricción actual es 0 y la línea no está vacía,
% y si la celda es marcada con no-pintada entonces se verifica recursivamente la línea sin modificar la restricción.
check_line([Cell|LineTail], [0|_], PaintCount, _, LineSat) :- Cell == "X", check_line(LineTail, [0], PaintCount, _, LineSat).

% Caso recursivo: si la restriccion actual es 0 y la línea no está vacía,
% y si la celda esta vacía (no instanciada) entonces se verifica recursivamente la línea sin modificar la restricción.
check_line([Cell|LineTail], [0|_], PaintCount, _, LineSat) :- var(Cell), check_line(LineTail, [0], PaintCount, _, LineSat).

%%%%%%%%%%%%%%%%%%%%%%%%%%% transpose %%%%%%%%%%%% Verificado que funciona

transpose([[]|_], []).
transpose(Matrix, [Row|Rows]) :- transpose_1st_col(Matrix, Row, RestMatrix),
                                 transpose(RestMatrix, Rows).
transpose_1st_col([], [], []).
transpose_1st_col([[H|T]|Rows], [H|Hs], [T|Ts]) :- transpose_1st_col(Rows, Hs, Ts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado solve que verifica si todas las restricciones se cumplen en una grilla.
% solve/4 toma una grilla, las restricciones en fila, las restricciones en columna y un valor de resolución.
solve(Grid, RowClues, ColClues, Solved) :-
    % Verificar si todas las restricciones en filas se cumplen.
    check_lines(Grid, RowClues, RowsSatisfied),
    % Verificar si todas las restricciones en columnas se cumplen.
    transpose(Grid, TransposedGrid),
    check_lines(TransposedGrid, ColClues, ColsSatisfied),
    % Si tanto las restricciones en filas como en columnas se cumplen, el nonograma está resuelto.
    (RowsSatisfied == 1, ColsSatisfied == 1 ->
        % Indicar que el nonograma está resuelto.
        Solved = true
    ;   % En caso contrario, el nonograma no está resuelto.
        Solved = false
    ).

% Predicado auxiliar para verificar todas las líneas (filas o columnas) en la grilla.
check_lines([], [], 1).
check_lines([Line|LinesTail], [Clues|CluesTail], Satisfied) :-
    check_line(Line, Clues, LineSatisfied),
    % Verificar si esta línea cumple las restricciones y si las restantes también lo hacen.
    check_lines(LinesTail, CluesTail, RemainingSatisfied),
    % Verificar si todas las líneas cumplen las restricciones.
    Satisfied is min(LineSatisfied, RemainingSatisfied).

