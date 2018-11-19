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

% use this instead of get_pokemon_effectiveness() for attack_factor, since its the attack type that matters
% defence_factor should still use get_pokemon_effectiveness because we do not know what moves enemy will use
moves_factor(P, Pa, Points):-
  get_move_set(Pa, Moves), %todo Moves is a list of Moves names
  best_move(P, Pa, Moves, Rating, BestMove), %todo
  Points is Rating.

% Moves is a list of Moves names
best_move(P, Pa, [NewMove|T], Rating, BestMove):-
  move_rating(P, Pa, NewMove, NewRating),
  NewRating> Rating -> best_move(P, Pa, T, NewRating, NewMove);
  best_move(P, Pa, T, Rating, BestMove).

% need to either a function that gets a row by name instead of row numebr,
% or make sure Moves in best_move() is a list of row numbers
%Formula: Types Effectiveness * STAB
%STAB is 1.25 if move is same type as Pa, 1 otherwise
% move_rating(P, Pa, NewMove, NewRating):-
%   get_rows_data("moves.csv", AllData),
%   get_col_number(AllData, 'type', Type),
%   get_data(AllData, NewMove, T1Col, Type1),

get_moveset(Pa, Moves):-
  get_rows_data("movesets.csv", AllData),
  Moves is AllData.


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

% TODO: generate KB so that we only get the csv files once
% TODO: user IO interface (Probably from command line. see Poole's geography.pl)
      % just make the basic frame for now, we will come up with the questions and possible interactions later
% TODO: return a list of pokemon that takes X damage from an type or less
      % ex. given a type, fire, and damage, 1, return a list of pokemon that takes less than 1 damage from fire
% TODO: get pokemon with highest total base stat given a list of pokemon
      % literally add all of the stats together and return highest
% TODO: get pokemon with highest stat1 > stat2 > stat3... (where stats = hp/atk/def/spatk/spdef/spd)
      % make a general formula that takes an order in an array ex. ['hp','speed',A,B,C,D] and a list of pokemon
      % then returns the pokemon from the list that has the highest hp, then if there is a tie, highest speed etc
% TODO: specify the stat strategy (ex. 2 atk, 2 sp atk, 1 hp, 1 spd)
      % given a list of strats (so the above would be ['atk','atk','spatk','spatk','hp','spd']) and a list of pokemon
      % return a pokemon team of 6
% TODO: specify generation, takes in a number (for the generation) and a list of pokemon
      % return a list of pokemon with a matching gen number
      % we will specify gen 0 as all generations (no filtering)
% TODO: no legendaries + mythical pokemon
      % takes a list of pokemon and returns a list of pokemon that are not legendaries or mythical
% TODO: rival teams (random, following strategy, hard coded -- ex. champion teams)
      % start with hard coded ones first
      % we will define the teams (probably in a csv or something)
      % then do random
      % randomly generate 6 pokemon. (allow for repeats???)
      % then do strategy
      % get the best 6 pokemon. start with 1, then check for type coverage (which weakenesses does it have), then do the 2nd one to cover those weakenesses
      % (above formula is subject to change, idk the best way to generate pokemon teams)
% TODO: find best match up for pokemon
      % based on types. so we will find a counter pokemon for each rival pokemon
% TODO: doubles
      % 2 pokemon on a field at a time. idk if this is too similar to solo battles
      % the only thing that's really affected are the moves it uses. might be put off
% TODO: moves - find csv, generate strategies (ex. 1 heal, 1 spatk, 1 atk, 1 buff)
      % DO THIS LAST
