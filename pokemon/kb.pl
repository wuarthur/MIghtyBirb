%%%%%%%%%%%%%%%%%%%%%%%% Load all the CSV data.
load:-
  load_pokedex,
  load_att_types,
  load_moves,
  load_moveset,
  load_presets.

load_pokedex:-
  csv_read_file("./csvs/pokedex.csv", [_|R]),
  maplist(assertz_pokedex, R).

assertz_pokedex(Row):-
  Row = row(Idx, Name, T1, T2, Tot, HP, Att, Def, SpAtt, SpDef, Speed, Generation, Legendary),
  exclude(=(''), [T1, T2], Types),
  Atoms = [
    pokemon(Idx, 'name', Name),
    pokemon(Idx, 'generation', Generation),
    pokemon(Idx, 'legendary', Legendary),
    pokemon(Idx, 'tot', Tot),
    pokemon(Idx, 'hp', HP),
    pokemon(Idx, 'att', Att),
    pokemon(Idx, 'def', Def),
    pokemon(Idx, 'sp_att', SpAtt),
    pokemon(Idx, 'sp_def', SpDef),
    pokemon(Idx, 'speed', Speed)
  ],
  maplist(assertz, Atoms),
  maplist(assertz_pokemon_type(Idx), Types).

assertz_pokemon_type(Idx, Type):-
  assertz(pokemon(Idx, 'type', Type)).


%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in attack type %% def CSV into KB

load_att_types:-
  csv_read_file("./csvs/types.csv", [_|R]),
  maplist(assertz_attack_multi, R).

assertz_attack_multi(Row):-
  Row = row(Attacking,Normal,Fire,Water,Electric,Grass,Ice,Fighting,Poison,Ground,Flying,Psychic,Bug,Rock,Ghost,Dragon,Dark,Steel,Fairy),
  Atoms = [
    attack(Attacking, 'Normal', Normal),
    attack(Attacking, 'Fire', Fire),
    attack(Attacking, 'Water', Water),
    attack(Attacking, 'Electric', Electric),
    attack(Attacking, 'Grass', Grass),
    attack(Attacking, 'Ice', Ice),
    attack(Attacking, 'Fighting', Fighting),
    attack(Attacking, 'Poison', Poison),
    attack(Attacking, 'Ground', Ground),
    attack(Attacking, 'Flying', Flying),
    attack(Attacking, 'Psychic', Psychic),
    attack(Attacking, 'Bug', Bug),
    attack(Attacking, 'Rock', Rock),
    attack(Attacking, 'Ghost', Ghost),
    attack(Attacking, 'Dragon', Dragon),
    attack(Attacking, 'Dark', Dark),
    attack(Attacking, 'Steel', Steel),
    attack(Attacking, 'Fairy', Fairy)
  ],
  maplist(assertz, Atoms).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in moves into KB
load_moves:-
  csv_read_file("./csvs/moves.csv", [_|B]),
  exclude(is_status_move, B, M),
  maplist(assert_moves, M).

is_status_move(M):-
  M = row(_, _,_, _, 'Status', _, _, _, _, _, _).

assert_moves(M):-
  M = row(Id, Move, _, Type, Category, Power, _, _, _, _, _),
  Atoms = [
    move(Id, 'move', Move),
    move(Id, 'type', Type),
    move(Id, 'category', Category),
    move(Id, 'power', Power)
  ],
  maplist(assertz, Atoms).

%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in moveset into KB
pokemon(Idx, 'moveset', M):-
  M = moveset(Idx, _, _).

load_moveset:-
  csv_read_file("./csvs/movesetNewest.csv", [_|B]),
  findall(Idx, move(Idx, _, _), Moves),
  list_to_set(Moves, Viable_set),
  maplist(assertz_moveset(Viable_set), B).

assertz_moveset(Viable_set, Row):-
  Row =.. [row|Lst],
  Lst = [Idx, _, _| M],
  exclude(=(''), M, Moves),
  list_to_set(Moves, Moveset),
  intersection(Moveset, Viable_set, Reduced_set),
  maplist(asserts_moveset_move(Idx), Reduced_set).

asserts_moveset_move(Idx, Move):-
  assertz(pokemon(Idx, 'move', Move)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Loading in Presets into KB
load_presets:-
    csv_read_file("./csvs/presets.csv", [_|B]),
    maplist(assert_preset, B).


assert_preset(Row):-
  Row =.. [row|Lst],
  Lst = [Idx, Name | Pokemon_indices],
  assertz(preset(Idx, Name, Pokemon_indices)).
