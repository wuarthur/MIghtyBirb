:- use_module(library(apply)).
:- use_module(library(csv)).
:- dynamic pokeDex/3.
:- dynamic moveSet/2.
:- dynamic moves/3.
:- dynamic types/3.


loadt(Z):-
  get_rows_data("./csvs/movesetsNew.csv", [[_|H]|T]),
  add_MoveSet(H, T),
  get_rows_data("./csvs/moves.csv", [[_|H2]|T2]),
  add_move(H2, T2),
  get_rows_data("./csvs/types.csv", [[_|H3]|T3]),
  add_types(H3, T3),
  get_rows_data("./csvs/pokedex.csv", [[_|H4]|T4]),
  add_pokeDex(H4, T4).


add_pokeDex(_, []).
add_pokeDex(Col, [[Num|H]|T]):-
  recursive_dex(Col, H, Num),
  add_pokeDex(Col, T).

recursive_dex(_,[],_).
recursive_dex([],_, _).
recursive_dex([C1|Ct], [Val|T],Num):-
  assert(pokeDex(Num,C1,Val)),
  recursive_dex(Ct,T, Num).

%%%%%%%%%%%%%%%
add_types(_, []).
add_types(Col, [[Num|H]|T]):-
  recursive_type(Col, H, Num),
  add_types(Col, T).

recursive_type(_,[],_).
recursive_type(_,[''|_],_).
recursive_type([],_, _).
recursive_type([DefendingType|Ct], [Val|T],AttackingType):-
  assert(types(AttackingType,DefendingType,Val)),
  recursive_type(Ct,T, AttackingType).


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
recursive_moveset([C1|Ct], [Move|T],Pokemon):-
  assert(moveSet(Pokemon,Move)),
  recursive_moveset(Ct,T, Pokemon).


% taken from https://stackoverflow.com/questions/23183662/prolog-parsing-a-csv-file
get_rows_data(File, Lists):-
  csv_read_file(File, Rows, []),
  maplist(row_to_list, Rows, Lists).

% change default row(_,_,...) to a list so we could get value by index
row_to_list(Row, List):-
  Row =.. [row|List].

% if there is no type, return null (targetted at pokemon with no second type)
types('',_,1).

% get the damage multiplier a type of move would make to a pokemon
damageMultiplier(DefendingPokemon, AttackingType, MultiplierValueBetween0and4) :-
  pokeDex(DefendingPokemon,'Type 1',T1),
  pokeDex(DefendingPokemon,'Type 2',T2),
  types(AttackingType,T1,V1),
  types(AttackingType,T2,V2),
  MultiplierValueBetween0and4 is V1*V2.

% get pokemon with highest where stats = hp/atk/def/spatk/spdef/spd/base
%https://stackoverflow.com/questions/40365709/prolog-getting-a-maximum-value-of-set-from-a-list-of-facts-using-fail-predica
% TODO: why does this return the same pokemon multiple times??? should we fix it
highestStat(Pokemon, StatName, Value):-
  pokeDex(Pokemon, StatName, Value),
  forall(pokeDex(Pokemon2, StatName, Value2),(Value2>Value->fail;true)).

% autoload the kb
:-loadt('hi').
