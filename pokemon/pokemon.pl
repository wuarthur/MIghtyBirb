:- use_module(library(apply)).
:- use_module(library(csv)).
:- dynamic pokeDex/3. pokeDex(num,col, value).
:- dynamic moveSet/2. 




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
% best_move(P, Pa, [NewMove|T], Rating, BestMove):-
%   move_rating(P, Pa, NewMove, NewRating),
%   NewRating> Rating -> best_move(P, Pa, T, NewRating, NewMove);
%   best_move(P, Pa, T, Rating, BestMove).

% need to either a function that gets a row by name instead of row numebr,
% or make sure Moves in best_move() is a list of row numbers
%Formula: Types Effectiveness * STAB
%STAB is 1.25 if move is same type as Pa, 1 otherwise
% move_rating(P, Pa, NewMove, NewRating):-
%   get_rows_data("moves.csv", AllData),
%   get_col_number(AllData, 'type', Type),
%   get_data(AllData, NewMove, T1Col, Type1),
%todo
% get_moveset( Moves):-
%   get_rows_data("movesetsNew.csv", [H,H2|T]),
%   Moves is H2.

typeX_only([], _, Acc, Ret):-
  maplist(list_to_list, Acc, Ret).
typeX_only([H|T], TypeX, Acc, Ret):-
  nth1(3, H, TypeX)->typeX_only(T, TypeX, [H|Acc], Ret);
  typeX_only(T, TypeX, Acc, Ret).

% typeXY_only(Data, TypeX, TypeY, Ret):-
%   typeX_only(AllData, TypeX, [], TypeXOnly),
%   typeX_only(AllData, Typey, [], TypeYOnly),

test_typeX_only(PokemonName):-
  get_rows_data("pokedex.csv", AllData),
  typeX_only(AllData, 'Water', [], TypeXOnly),
  get_best_pokemon(TypeXOnly, [5,6,7,8,9,10,11], PokemonName).

%filter to only have generation GenX, see test2().
genX_only([], _, Acc, Ret):-
  maplist(list_to_list, Acc, Ret).
genX_only([H|T], GenX, Acc, Ret):-
  nth1(12, H, GenX)->genX_only(T, GenX, [H|Acc], Ret);
  genX_only(T, GenX, Acc, Ret).

%usage see test_lengendary_only()
lengendary_only([], Acc, Ret):-
  maplist(list_to_list, Acc, Ret).
lengendary_only([H|T], Acc, Ret):-
  nth1(13, H, 'True')->lengendary_only(T, [H|Acc], Ret);
  lengendary_only(T, Acc, Ret).
%same usage as above just does the opposite
no_lengendary([], Acc, Ret):-
  maplist(list_to_list, Acc, Ret).
no_lengendary([H|T], Acc, Ret):-
  nth1(13, H, 'False')->no_lengendary(T, [H|Acc], Ret);
  no_lengendary(T, Acc, Ret).


test_lengendary_only(PokemonName):-
  get_rows_data("pokedex.csv", AllData),
  lengendary_only(AllData, [], Lengends),
  get_best_pokemon(Lengends, [5,6,7,8,9,10,11], PokemonName).

test2(PokemonName):-
  get_rows_data("pokedex.csv", AllData),
  genX_only(AllData, 1, [], GenXOnly),
  get_best_pokemon(GenXOnly, [5,6,7,8,9,10,11], PokemonName).

test1(PokemonName):-
  get_rows_data("pokedex.csv", AllData),
  get_best_pokemon(AllData, [6,5,7,8,9,10,11], PokemonName).

%get_best_pokemon
%Data: list of pokemons see test1(), test2().
% Order: order of importance of each stats by their column, eg. [6,5,7,8,9,10,11],
% Last: Name of best pokemon
% To get best stats for a type use a filter that filter out unwanted types
get_best_pokemon(Data, Order, PokemonName):-
  sort_by_importance(Data, Order, [], SortedList),
  print(SortedList),
  last(SortedList, Last),
  nth1(8, Last, PokemonName).

%sort_by_importance([[42,'Golbat','Poison','Flying',455,75,80,70,65,75,90,1,'False'],['Butterfree','Bug','Flying',395,60,45,50,90,80,70,1,'False']],[6,5,7,8,9,10,11], [], Ret).
sort_by_importance([], _, Acc, Ret):-
  sort(Acc, SortedList),
  maplist(list_to_list, SortedList, Ret).

sort_by_importance([H|T], Order, Acc, Ret):-
  formatStat(H, Order, [], OnePokemon),
  nth1(2, H, Name),
  append(OnePokemon,[Name],NewPokemon),
  %maplist(list_to_list, [Name|OnePokemon], NewPokemon),
  sort_by_importance(T, Order, [NewPokemon|Acc], Ret).

%just a filter that does nothing
list_to_list(List1, Ret):-
  List1 = Ret.

%formatStat([42,'Golbat','Poison','Flying',455,75,80,70,65,75,90,1,'False'],[6,5,7,8,9,10], [], Ret).
formatStat( _, [], Acc, Ret) :-
  reverse(List1, Acc),
  maplist(list_to_list, List1, Ret).
formatStat(List, [H|T], Acc, Ret):-
  nth1(H, List, Elem),
  formatStat(List, T, [Elem|Acc], Ret).



%basing ration without moves
base_rating(Pa, P, Rating):-
  get_rows_data("pokedex.csv", AllData),
  get_col_number(AllData, 'Name', NameCol),
  get_data(AllData, P, NameCol, LeName),
  print(LeName),
  get_col_number(AllData, 'HP', HPCol),
  get_data(AllData, Pa, HPCol, PaHP),
  get_data(AllData, P, HPCol, PHP),
  speed_factor(P, Pa, SpeedF),
  attack_factor(P, Pa, AtkF),
  defence_factor(P, Pa, DefF),
  Rating is 10/(PHP/(AtkF)-SpeedF) + (PaHP/DefF)*10.

% find pokemon with best rating against pokemon P
%find_best_rating(3, 0, 0, 1,X).
find_base_best_rating(P, Pbest, BestRating, Row, X):-
  Row is 720-> X is Pbest;
  Num is Row+0,
  Next is Row+1,
  base_rating(P, Num, Rating),
  Rating > BestRating -> find_best_rating(P, Num, Rating, Next, X);
  find_best_rating(P, Pbest, BestRating, Next, X).

%list is a list of strings containing only [Total,HP,Attack,Defense,Sp. Atk,Sp. Def,Speed]
%


import:-load().
load():-
  get_rows_data("movesetsNew.csv", [H|T]),
  add_to_db(H, T).

add_to_db(_, []).
add_to_db(Col, [[Num|H]|T]):-
  recursive_add(Col, H, Num),
  add_to_db(Col, T).

recursive_add(_,[],_).
recursive_add(_,[''|_],_).
recursive_add([],_, _).
recursive_add([C1|Ct], [Val|T],Num):-
  nl(),
  print(moveSet(Num,Val)),
  assert(moveSet(Num,Val)), 
  recursive_add(Ct,T, Num).
