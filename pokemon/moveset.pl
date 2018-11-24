% NOTE THIS CODE DOES NOT WORK

:- use_module(library(apply)).
:- use_module(library(csv)).
:- dynamic moveset/2. moveset(num,value).

get_moveset():-
  get_rows_data("movesetsNew.csv", [H|T]),
  get_next_moveset_row(H,T).

get_next_moveset_row([Num,Move|Moves],T):-
  add_move(Num,Move,Moves),
  get_next_moveset_row(T).

add_move(Num,Move,[]) :- assert(moveset(Num,Move)).
add_move(Num,Move,[H|T]) :-
  assert(moveset(Num,Move)),
  add_move(Num,H,T).

get_rows_data(File, Lists):-
  csv_read_file(File, Rows, []),
  maplist(row_to_list, Rows, Lists).

% change default row(_,_,...) to a list so we could get value by index
row_to_list(Row, List):-
  Row =.. [row|List].
