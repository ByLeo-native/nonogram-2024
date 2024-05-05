:- module(proylcc,
	[  
		put/8,
    grid_solved/3
	]).

:-use_module(library(lists)).

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
	replace(_Cell, ColN, Content, Row, NewRow)),

    
    % Verificar si la fila modificada cumple con las restricciones
    check_line(NewRow, RowsClues),
    (check_line(NewRow, RowsClues) ->
        RowSat = 1
    ;
        RowSat = 0
    ),

    % Transponer la grilla para verificar las columnas
    transpose(NewGrid, TransposedGrid),
    
    % Verificar si la columna modificada cumple con las restricciones
    nth1(ColN, TransposedGrid, ModifiedCol),
    check_line(ModifiedCol, ColsClues),
    (check_line(ModifiedCol, ColsClues) ->
        ColSat = 1
    ;
        ColSat = 0
    ).


%
% Nonogram solving algorithm
%

/**
 * solve(+Lines:list, +Constrs:list) is nondet.
 *
 * Solves the nonogram given through the lines and their constraints.
 * Uses an optimized algorithm that solves lines with few possibilities first.
 */
solve(Lines, Constrs) :-
    pack(Lines, Constrs, Pack),
    sort(Pack, SortedPack),
    solve(SortedPack).

solve([]).
solve([line(_, Line, Constr)|Rest]) :-
    check_line(Line, Constr),
    solve(Rest).

/**
 * pack(+Lines:list, +Constrs:list, -Result:list) is det.
 *
 * Packs a line and its constraints into a single term and adds the number of
 * possible line solutions given the line's length and constraints as the term's
 * first argument to enable sorting.
 */
pack([], [], []).
pack([Line|Lines], [Constr|Constrs], [line(Count, Line, Constr)|Result]) :-
    length(Line, LineLength),
    length(CheckLine, LineLength),
    findall(CheckLine, check_line(CheckLine, Constr), NCheckLine),
    length(NCheckLine, Count),
    pack(Lines, Constrs, Result).

/**
 * check_line(+Line:list ,+Constraints:list) is nondet.
 *
 * Checks if the given Line satisfies the Constraints. Can also generate all
 * valid lines if given a line with some or all members unbound. Examples:
 *   ?- check_line([x, ' ', x, ' '], [1,1]).
 *   true .
 *   ?- L = [_,_,_,_,_], check_line(L, [2,1]).
 *   L = [x, x, ' ', x, ' '] ;
 *   L = [x, x, ' ', ' ', x] ;
 *   L = [' ', x, x, ' ', x] .
 */
check_line([],[]) :- !.
check_line(Line, [Part|Rest]) :-
    Rest \= [],
    add_space(Line, Line2),
    check_part(Line2, Line3, Part),
    force_space(Line3, Line4),
    check_line(Line4, Rest).
check_line(Line, [Part|[]]) :-
    add_space(Line, Line2),
    check_part(Line2, Line3, Part),
    add_space(Line3, Line4),
    check_line(Line4, []).

force_space([' '|Line],Line).

add_space(Line, Line).
add_space([' '|Line],RestLine) :-
    add_space(Line, RestLine).

check_part(Line, Line, 0).
check_part(['x'|Line], RestLine, N) :-
    N > 0,
    N1 is N - 1,
    check_part(Line, RestLine, N1).


% Predicado para verificar si el estado actual de la grilla está solucionado
grid_solved(RowConstraints, ColConstraints, Grid) :-
    % Convertir la grilla en una lista de filas
    transpose(Grid, TransposedGrid),
    % Verificar las filas
    maplist(check_line, Grid, RowConstraints),
    % Verificar las columnas
    maplist(check_line, TransposedGrid, ColConstraints).

% Predicado para transponer una lista de listas
transpose([], []).
transpose([[]|_], []).
transpose(Grid, [Row|TRows]) :-
    transpose_col(Grid, Row, RestGrid),
    transpose(RestGrid, TRows).

transpose_col([], [], []).
transpose_col([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    transpose_col(Rows, Hs, Ts).
