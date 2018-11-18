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

% where A is the Attacking type, D is defending type
get_attack_effectiveness(_,'',1).
get_attack_effectiveness(A, D, V):-
  get_rows_data("types.csv", AllData),
  get_col_number(AllData, A, Avalue),
  get_col_number(AllData, D, Dvalue),
  get_data(AllData, Avalue, Dvalue, V).

% where P is the defending Pokemon (of one or more types)
% A is the attacking type
% V is the multiplier of the damage
get_pokemon_effectiveness(P, A, V):-
  get_rows_data("pokedex.csv", AllData),
  get_col_number(AllData, 'Type 1', T1No),
  get_col_number(AllData, 'Type 2', T2No),
  get_data(AllData, P, T1No, T1),
  get_data(AllData, P, T2No, T2),
  get_attack_effectiveness(A, T1, V1),
  get_attack_effectiveness(A, T2, V2),
  V is V1*V2.

%0 if P faster than Pa, 1.25 if Pa is faster
%Pa is attacking pokemon
speed_factor(P, Pa, Points):-
  get_rows_data("pokedex.csv", AllData),
  get_col_number(AllData, 'Speed', SpedCol),
  get_data(AllData, P, SpedCol, SpP),
  get_data(AllData, Pa, SpedCol, SpPa),
  SpP > SpPa -> Points is 1.25;
  Points is 0.

%returns attack/defence
%Pa is attacking pokemon
attack_factor(P, Pa, Points):-
  get_rows_data("pokedex.csv", AllData),
  get_col_number(AllData, 'Type 1', T1Col),
  get_data(AllData, Pa, T1Col, Type1),
  get_pokemon_effectiveness(P, Type1, TypeF),
  get_col_number(AllData, 'Attack', AtkCol),
  get_col_number(AllData, 'Defense', DefCol),
  get_data(AllData, P, DefCol, Def),
  get_data(AllData, Pa, AtkCol, Atk),
  Points is TypeF*Atk/Def.

%returns defence/attackb
%Pa is attacking pokemon
defence_factor(P, Pa, Points):-
  get_rows_data("pokedex.csv", AllData),
  get_col_number(AllData, 'Type 1', T1Col),
  get_data(AllData, P, T1Col, Type1),
  get_pokemon_effectiveness(Pa, Type1, TypeF),
  get_col_number(AllData, 'Attack', AtkCol),
  get_col_number(AllData, 'Defense', DefCol),
  get_data(AllData, P, AtkCol, Atk),
  get_data(AllData, Pa, DefCol, Def),
  Points is Def/Atk/TypeF.

%basing ration without moves 
base_rating(P, Pa, Rating):-
  get_rows_data("pokedex.csv", AllData),
  get_col_number(AllData, 'HP', HPCol),
  get_data(AllData, Pa, HPCol, PaHP),
  get_data(AllData, P, HPCol, PHP),
  speed_factor(P, Pa, SpeedF),
  attack_factor(P, Pa, AtkF),
  defence_factor(P, Pa, DefF),
  Rating is 10/(PHP/(AtkF)-SpeedF) + (PaHP/DefF)*10.

% find pokemon with best rating against pokemon P
% right now it only returns true .__.
find_best_rating(_, Pbest, _, 720):-
  Pbest is Pbest.
find_best_rating(P, Pbest, BestRating, Row):-
  Row is 720-> Pbest is Pbest;
  Num is Row+0,
  Next is Row+1,
  base_rating(P, Num, Rating),
  Rating > BestRating -> find_best_rating(P, Num, Rating, Next);
  find_best_rating(P, Pbest, BestRating, Next).




% largest(N):-
%   dig(N),
%   not((
%       dig(M),
%       M > N
%   )).

% TODO: user IO interface (Probably from command line. see Poole's geography.pl)
% TODO: get pokemon with highest stat1 > stat2 > stat3... (where stats = hp/atk/def/spatk/spdef/spd)
% TODO: specify generation
% TODO: no legendaries + mythical pokemon
% TODO: rival teams (random, following strategy, hard coded -- ex. champion teams)
% TODO: find best match up for pokemon
% TODO: doubles
% TODO: moves - find csv, generate strategies (ex. 1 heal, 1 spatk, 1 atk, 1 buff)
