

%%%%%%%%%%%%%%% Load V1 is kind crap, V2 is all triples.
%%%%%%%%%%%%%% Ironically V1 took much longer to write :|


%%%%%%%%%%%%%%%%%%%%%%%% Load all the CSV data.
load:-
  load_pokedex,
  load_att_types,
  load_moves,
  load_moveset.

load_pokedex:-
  csv_read_file("./csvs/pokedex.csv", [_|R]),
  maplist(assertz_pokedex, R).

assertz_pokedex(Row):-
  Row = row(Idx, Name, T1, T2, Tot, HP, Att, Def, SpAtt, SpDef, Speed, Generation, Legendary),
  exclude(=(''), [T1, T2], Types),
  assertz(pokemon(Idx, 'name', Name)),
  maplist(assertz_pokemon_type(Idx), Types),
  assertz(pokemon(Idx, 'generation', Generation)),
  assertz(pokemon(Idx, 'legendary', Legendary)),
  assertz(pokemon(Idx, 'tot', Tot)),
  assertz(pokemon(Idx, 'hp', HP)),
  assertz(pokemon(Idx, 'att', Att)),
  assertz(pokemon(Idx, 'def', Def)),
  assertz(pokemon(Idx, 'sp_att', SpAtt)),
  assertz(pokemon(Idx, 'sp_def', SpDef)),
  assertz(pokemon(Idx, 'speed', Speed)).

assertz_pokemon_type(Idx, Type):-
  assertz(pokemon(Idx, 'type', Type)).


%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in attack type %% def CSV into KB

load_att_types:-
  csv_read_file("./csvs/types.csv", [_|R]),
  maplist(assertz_attack_multi, R).

assertz_attack_multi(Row):-
  Row = row(Attacking,Normal,Fire,Water,Electric,Grass,Ice,Fighting,Poison,Ground,Flying,Psychic,Bug,Rock,Ghost,Dragon,Dark,Steel,Fairy),
  assertz(attack(Attacking, 'Normal', Normal)),
  assertz(attack(Attacking, 'Fire', Fire)),
  assertz(attack(Attacking, 'Water', Water)),
  assertz(attack(Attacking, 'Electric', Electric)),
  assertz(attack(Attacking, 'Grass', Grass)),
  assertz(attack(Attacking, 'Ice', Ice)),
  assertz(attack(Attacking, 'Fighting', Fighting)),
  assertz(attack(Attacking, 'Poison', Poison)),
  assertz(attack(Attacking, 'Ground', Ground)),
  assertz(attack(Attacking, 'Flying', Flying)),
  assertz(attack(Attacking, 'Psychic', Psychic)),
  assertz(attack(Attacking, 'Bug', Bug)),
  assertz(attack(Attacking, 'Rock', Rock)),
  assertz(attack(Attacking, 'Ghost', Ghost)),
  assertz(attack(Attacking, 'Dragon', Dragon)),
  assertz(attack(Attacking, 'Dark', Dark)),
  assertz(attack(Attacking, 'Steel', Steel)),
  assertz(attack(Attacking, 'Fairy', Fairy)).


%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in moveset into KB
pokemon(Idx, 'moveset', M):-
  M = moveset(Idx, _, _).

load_moveset:-
  csv_read_file("./csvs/movesets.csv", [_|B]),
  maplist(assertz_moveset, B).

assertz_moveset(Row):-
  Row =.. [row|Lst],
  Lst = [Idx, _, _| M],
  exclude(=(''), M, Moves),
  maplist(asserts_moveset_move(Idx), Moves).

asserts_moveset_move(Idx, Move):-
  assertz(pokemon(Idx, 'move', Move)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in moves into KB
load_moves:-
  csv_read_file("./csvs/moves.csv", [_|B]),
  maplist(assert_moves, B).

assert_moves(M):-
  M = row(Id, Move, Desc, Type, Category, Power, Accuracy, PP, Z_effect, Priority, Crit),
  assertz(move(Id, 'move', Move)),
  assertz(move(Id, 'desc', Desc)),
  assertz(move(Id, 'type', Type)),
  assertz(move(Id, 'category', Category)),
  assertz(move(Id, 'power', Power)),
  assertz(move(Id, 'acc', Accuracy)),
  assertz(move(Id, 'pp', PP)),
  assertz(move(Id, 'z-effect', Z_effect)),
  assertz(move(Id, 'priority', Priority)),
  assertz(move(Id, 'crit', Crit)).
