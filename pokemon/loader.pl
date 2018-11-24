:- use_module(library(csv)).



%%%%%%%% Sorry I got sick and didnt show up on Friday, here is my impl for loading KB






%%% List Util
:- dynamic tuple2/2. tuple3/3.
rep_list(X, N, List):-
  length(List, N),
  maplist(=(X), List).
tuple2ify(I1, I2, T2):-
  T2 = tuple2(I1, I2).
zip2(L1, L2, Zipped):-
  maplist(tuple2ify, L1, L2, Zipped).
row_to_list(Row, List):-
  Row =.. [row|List].

non_empty(E):-
  not(E = "").



%%%%%%%%%%%%%%%%%%%%%%%% Load all the CSV data.
load_everything:-
  load_pokedex,
  load_att_types,
  load_moves,
  load_moveset.


%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in pokedex
%% pokemon(Idx, name, Type, Generation, Legendary, ['Types'], [tuple2('Stat', Value)])

load_pokedex:-
  csv_read_file("./csvs/pokedex.csv", [_|B]),
  maplist(parse_pokedex_body, B, Res),
  maplist(assertz, Res).

parse_pokedex_body(B, Res):-
  B = row(Idx, Name, T1, T2, Tot, HP, Att, Def, SpAtt, SpDef, Speed, Generation, Legendary),
  exclude(=(''), [T1, T2], Types),
  Res = pokemon(Idx, Name, Types, Generation, Legendary, [
    stat('Total', Tot),
    stat('HP', HP),
    stat('Attack', Att),
    stat('Defense', Def),
    stat('Sp. Atk', SpAtt),
    stat('Sp. Def', SpDef),
    stat('Speed', Speed)
  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in attack type %% def CSV into KB
%% attacking('Att_Type', [tuple2('Def_Type', Multiplier)])
load_att_types:-
  csv_read_file("./csvs/types.csv", [H|B]),
  row_to_list(H, [_ | Def_type]),
  maplist(row_to_list, B, Att_type_Multiplier),
  length(Att_type_Multiplier, Length),
  rep_list(Def_type, Length, Repeated_Def_type),
  maplist(create_type, Att_type_Multiplier, Repeated_Def_type, Att_chart),
  maplist(assertz, Att_chart).

create_type([Att_type | Multiplier], Def_type, Res):-
  Res = attacking(Att_type, Zipped),
  zip2(Def_type, Multiplier, Zipped).


%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in moveset into KB
%% moveset(Idx, Species, Forme, [Moves])
load_moveset:-
  csv_read_file("./csvs/movesets.csv", [_|B]),
  maplist(parse_moveset_body, B, Res),
  maplist(assertz, Res).

parse_moveset_body(B, Res):-
  row_to_list(B, [Idx, Species, Forme | Moves]),
  exclude(=(''), Moves, M),
  Res = moveset(Idx, Species, Forme, M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in moves into KB
load_moves:-
  csv_read_file("./csvs/moves.csv", [_|B]),
  maplist(assert_moves, B).

assert_moves(M):-
  M = row(Id, Move, Desc, Type, Category, Power, Accuracy, PP, Z_effect, Priority, Crit),
  assertz(moves(Id, 'move', Move)),
  assertz(moves(Id, 'desc', Desc)),
  assertz(moves(Id, 'type', Type)),
  assertz(moves(Id, 'category', Category)),
  assertz(moves(Id, 'power', Power)),
  assertz(moves(Id, 'acc', Accuracy)),
  assertz(moves(Id, 'pp', PP)),
  assertz(moves(Id, 'z-effect', Z_effect)),
  assertz(moves(Id, 'priority', Priority)),
  assertz(moves(Id, 'crit', Crit)).
