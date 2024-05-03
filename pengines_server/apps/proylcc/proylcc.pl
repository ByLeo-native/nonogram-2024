:- module(proylcc,
	[  
		put/8
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
	% Verificar si la fila satisface la pista después de la modificación
    check_row(NewRow, RowN, RowsClues, RowSat),
    % Verificar si la columna satisface la pista después de la modificación
    check_col(NewGrid, ColN, ColsClues, ColSat).



	ccheck_row([], _, _, 0).

check_row([], _, _, 0).

check_row([Cell | Row], RowN, RowsClues, RowSat) :-
  (
    Cell == '#' ->
      decrement_current_clue(RowsClues, RowN, UpdatedRowsClues),
      nth0(0, UpdatedRowsClues, 0) ->
        RowSat = 1,
        replace_nth(RowsClues, RowN, -1, _), % No es necesario guardar UpdatedRowsCluesNew
        % **Pasar UpdatedRowsClues a la llamada recursiva**
        check_row(Row, RowN, UpdatedRowsClues, RowSat)
      ;
      check_row(Row, RowN, UpdatedRowsClues, RowSat)
  ;
    check_row(Row, RowN, RowsClues, RowSat)
  ).


decrement_current_clue(RowsClues, RowN, UpdatedRowsClues) :-
    nth0(RowN, RowsClues, CurrentClue),
    NewClue is max(CurrentClue - 1, 0),
    replace_nth(RowsClues, RowN, NewClue, UpdatedRowsClues).

replace_nth([_|T], 0, X, [X|T]).
replace_nth([H|T], N, X, [H|R]) :-
    N > 0,
    N1 is N - 1,
    replace_nth(T, N1, X, R).

check_col([], _, _, 0).

check_col([Row | RestRows], ColN, ColsClues, ColSat) :-
  % Obtener la celda actual de la columna
  nth0(ColN, Row, Cell),

  % Verificar si la celda actual es un marcador
  (
    Cell == '#' ->
      decrement_current_clue(ColsClues, ColN, UpdatedColsClues),
      nth0(0, UpdatedColsClues, 0) ->
        ColSat = 1,
        % No es necesario guardar UpdatedColsCluesNew
        replace_nth(ColsClues, ColN, -1, _),
        % **Pasar UpdatedColsClues a la llamada recursiva**
        check_col(RestRows, ColN, UpdatedColsClues, ColSat)
      ;
      check_col(RestRows, ColN, UpdatedColsClues, ColSat)
  ;
    check_col(RestRows, ColN, ColsClues, ColSat)
  ).



