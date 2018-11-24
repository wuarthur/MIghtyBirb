:- use_module(library(apply)).
:- use_module(library(csv)).
:- dynamic pokeDex/3. pokeDex(num,col, value).
:- dynamic moveSet/2.
:- dynamic moves/3.
:- dynamic types/3.

import:-load().
load():-
  get_rows_data("movesetsNew.csv", [H|T]),
  add_MoveSet(H, T),
  get_rows_data("moves.csv", [H2|T2]),
  add_move(H2, T2),
  get_rows_data("types.csv", [H3|T3]),
  add_types(H3, T3).

add_types(_, []).
add_types(Col, [[Num|H]|T]):-
  recursive_type(Col, H, Num),
  add_types(Col, T).

recursive_type(_,[],_).
recursive_type(_,[''|_],_).
recursive_type([],_, _).
recursive_type([C1|Ct], [Val|T],Num):-
  nl(),
  print(types(Num,C1, Val)),
  assert(types(Num,C1,Val)),
  recursive_type(Ct,T, Num).


%%%%%%%%%%%%%
add_move(_, []).
add_move(Col, [[Num|H]|T]):-
  recursive_move(Col, H, Num),
  add_move(Col, T).

recursive_move(_,[],_).
recursive_move(_,[''|_],_).
recursive_move([],_, _).
recursive_move([C1|Ct], [Val|T],Num):-
  assert(moves(Num,C1,Val)),
  recursive_move(Ct,T, Num).

%%%%%%%%%%%%
add_MoveSet(_, []).
add_MoveSet(Col, [[Num|H]|T]):-
  recursive_moveset(Col, H, Num),
  add_MoveSet(Col, T).

recursive_moveset(_,[],_).
recursive_moveset(_,[''|_],_).
recursive_moveset([],_, _).
recursive_moveset([C1|Ct], [Val|T],Num):-
  assert(moveSet(Num,Val)),
  recursive_moveset(Ct,T, Num).


% taken from https://stackoverflow.com/questions/23183662/prolog-parsing-a-csv-file
get_rows_data(File, Lists):-
  csv_read_file(File, Rows, []),
  maplist(row_to_list, Rows, Lists).

% change default row(_,_,...) to a list so we could get value by index
row_to_list(Row, List):-
  Row =.. [row|List].

% if there is no type, return null (targetted at pokemon with no second type)
types(_,'',1).

% get the damage multiplier a type of move would make to a pokemon
damageMultiplier(DefendingPokemon, AttackingType, MultiplierValueBetween0and4) :-
  pokeDex(DefendingPokemon,"Type1",T1),
  pokeDex(DefendingPokemon,"Type2",T2),
  types(T1,AttackingType,V1),
  types(T2,AttackingType,V2),
  MultiplierValueBetween0and4 is V1*V2.
