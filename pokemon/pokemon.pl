:- use_module(library(apply)).
:- use_module(library(csv)).

types("Normal", 1).
types("Fire", 2).
types("Water", 3).
types("Electric", 4).
types("Grass", 5).
types("Ice", 6).
types("Fighting", 7).
types("Poison", 8).
types("Ground", 9).
types("Flying", 10).
types("Psychic", 11).
types("Bug", 12).
types("Rock", 13).
types("Ghost", 14).
types("Dragon", 15).
types("Dark", 16).
types("Steel", 17).
types("Fairy", 18).

% taken from https://stackoverflow.com/questions/23183662/prolog-parsing-a-csv-file
get_rows_data(File, Lists):-
  csv_read_file(File, Rows, []),
  rows_to_lists(Rows, Lists).

rows_to_lists(Rows, Lists):-
  maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
  Row =.. [row|List].


% where D is defending pokemon, A is the Attacking pokemon
effectiveness_of_defense(D, A, V):-
  get_rows_data("types.csv", TypeChart),
  types(D, Dvalue),
  types(A, Avalue),
  nth0(Dvalue, TypeChart, Dstats),
  nth0(Avalue, Dstats, V).
