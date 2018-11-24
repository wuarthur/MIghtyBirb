

%%%%%%%%%%%%%%%%%%%%%%%% Load all the CSV data.
load:-
  load_pokedex,
  load_att_types,
  load_moves,
  load_moveset.

load_pokedex:-
  csv_read_file("pokedex.csv", [_|R]),
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
  csv_read_file("types.csv", [_|R]),
  maplist(assertz_attack_multi, R).

assertz_attack_multi(Row):-
  Row = row(Attacking,Normal,Fire,Water,Electric,Grass,Ice,Fighting,Poison,Ground,Flying,Psychic,Bug,Rock,Ghost,Dragon,Dark,Steel,Fairy),
  assert(attack(Attacking, 'Normal', Normal)),
  assert(attack(Attacking, 'Fire', Fire)),
  assert(attack(Attacking, 'Water', Water)),
  assert(attack(Attacking, 'Electric', Electric)),
  assert(attack(Attacking, 'Grass', Grass)),
  assert(attack(Attacking, 'Ice', Ice)),
  assert(attack(Attacking, 'Fighting', Fighting)),
  assert(attack(Attacking, 'Poison', Poison)),
  assert(attack(Attacking, 'Ground', Ground)),
  assert(attack(Attacking, 'Flying', Flying)),
  assert(attack(Attacking, 'Psychic', Psychic)),
  assert(attack(Attacking, 'Bug', Bug)),
  assert(attack(Attacking, 'Rock', Rock)),
  assert(attack(Attacking, 'Ghost', Ghost)),
  assert(attack(Attacking, 'Dragon', Dragon)),
  assert(attack(Attacking, 'Dark', Dark)),
  assert(attack(Attacking, 'Steel', Steel)),
  assert(attack(Attacking, 'Fairy', Fairy)).


%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in moveset into KB
pokemon(Idx, 'moveset', M):-
  M = moveset(Idx, _, _).

load_moveset:-
  csv_read_file("movesets.csv", [_|B]),
  maplist(assertz_moveset, B).

assertz_moveset(Row):-
  Row =.. [row|Lst],
  Lst = [Idx, Species, Forme | M],
  exclude(=(''), M, Moves),
  assertz(moveset(Idx, 'species', Species)),
  assertz(moveset(Idx, 'forme', Forme)),
  maplist(asserts_moveset_move(Idx), Moves).

asserts_moveset_move(Idx, Move):-
  assertz(moveset(Idx, 'move', Move)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in moves into KB
load_moves:-
  csv_read_file("moves.csv", [_|B]),
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