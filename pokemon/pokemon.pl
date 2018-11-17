:- use_module(library(apply)).
:- use_module(library(csv)).

% taken from https://stackoverflow.com/questions/23183662/prolog-parsing-a-csv-file
get_rows_data(File, Lists):-
  csv_read_file(File, Rows, []),
  maplist(row_to_list, Rows, Lists).

% change default row(_,_,...) to a list so we could get value by index
row_to_list(Row, List):-
  Row =.. [row|List].

% general function to get cell data
get_data(AllData, RowNo, ColumnNo, Value) :-
  nth0(RowNo, AllData, DataForRow),
  nth0(ColumnNo, DataForRow, Value).

% general function to get the name and number of a column
get_col_number(AllData, ColName, ColNo) :-
  nth0(0, AllData, AllColNames),
  nth0(ColNo, AllColNames, ColName).

% where D is defending pokemon, A is the Attacking pokemon
get_attack_effectiveness(D, A, V):-
  get_rows_data("types.csv", AllData),
  get_col_number(AllData, D, Dvalue),
  get_col_number(AllData, A, Avalue),
  get_data(AllData, Dvalue, Avalue, V).
