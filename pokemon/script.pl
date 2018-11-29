start:-
  retractall(active_pokemon(_,_,_)),
  ask_user(
    'Welcome to the pokemon battling lite.\n
    Would you like to build a random team or use a specify strategy?\n',
    ['Random\n','Strategy\n'],
    ['random','strategy'],
    generate_full_team
  ).

%%% AI
%%% create a team of pokemon
generate_full_team(random):-
  random_pokemon(_, _, 6, ListOfPokemon),
  list_to_set(ListOfPokemon, Found_set),
  random_permutation(Found_set, I),
  take(6, I, Indices),
  maplist(activate_my_pokemon, Indices),
  getPokemonNames(LN, Indices),
  print("Your team:"),
  nl(),
  print(LN),
  generate_rival_team.

generate_full_team(strategy):-
  strategy_parameters('type',Type),
  strategy_parameters('legendary',Legend),
  strategy_parameters('generation',Gen),
  Q = [
    'Current strategies:\n- type: ', Type,
    '\n- ', Legend, ' legendary\n- generation: ', Gen, '\nModify:\n'
  ],
  add_to_string(Q, '', Question),
  ask_user(
    Question,
    ['types\n','legendary\n','generation\n','done\n'],
    ['types','legendary','generation','done'],
    modify_strategy
  ).

modify_strategy(types):-
  findall(T, attack('Normal', T, _), L1),
  append_nl(L1, L2),
  append(L1, ['any'], ListOfTypes1),
  append(L2, ['any\n'], ListOfTypes2),
  ask_user(
    'Choose type:\n',
    ListOfTypes2,
    ListOfTypes1,
    updateKb(type)
  ).

modify_strategy(legendary):-
  ask_user(
    'Allow legendary pokemon?\n',
    ['Yes, allow\n', 'No, do not allow\n', 'Only allow\n'],
    ['allow', 'no', 'only'],
    updateKb(legendary)
  ).

modify_strategy(generation):-
  ask_user(
    'Which generation?\n',
    ['1\n','2\n','3\n','4\n','5\n','6\n','any\n'],
    [1,2,3,4,5,6,any],
    updateKb(generation)
  ).

modify_strategy(done):-
  strategy_parameters('type',Type),
  get_gen_or_type_value(Type, T),
  strategy_parameters('legendary',Legend),
  get_legend_value(Legend, L),
  strategy_parameters('generation',Gen),
  get_gen_or_type_value(Gen, G),
  findall(Idx, (
    pokemon(Idx, 'type', T),
    pokemon(Idx, 'legendary', L),
    pokemon(Idx, 'generation', G)
    ), List
  ),
  list_to_set(List, Found_set),
  random_permutation(Found_set, I),
  take(6, I, Indices),
  maplist(activate_my_pokemon, Indices),
  getPokemonNames(LN, Indices),
  print("Your team:"),
  nl(),
  print(LN),
  generate_rival_team.


generate_rival_team:-
  random_pokemon(_, _, 6, ListOfPokemon),
  list_to_set(ListOfPokemon, Found_set),
  random_permutation(Found_set, I),
  take(6, I, Indices),
  maplist(activate_npc_pokemon, Indices),
  getPokemonNames(LN, Indices),
  nl(),
  print("Rival's team:"),
  nl(),
  print(LN)
  %TODO: add battle trigger here
  .




%%%%%%%%%%% helper functions to display all of the pokemon names
append_nl(OriginalList, NewList):-
  maplist(add_nl, OriginalList, NewList).

add_nl(Name, RetName):-
  atom_concat(Name, '\n', RetName).

getPokemonNames(ListOfNames, List):-
  maplist(getName, List, ListOfNames).

getName(Index, Name):-
  pokemon(Index, 'name', Name).

activate_npc_pokemon(Pokemon):-
  activate(true,Pokemon).
activate_my_pokemon(Pokemon):-
  activate(false,Pokemon).

add_to_string([H], String, FullString):-
  atom_concat(String, H, FullString).
add_to_string([H|T], String, FullString):-
  atom_concat(String, H, X),
  add_to_string(T, X, FullString).

updateKb(ChangedName, ChangedValue):-
  retractall(strategy_parameters(ChangedName, _)),
  assertz(strategy_parameters(ChangedName, ChangedValue)),
  generate_full_team(strategy).

get_legend_value(allow, _).
get_legend_value(no, 'False').
get_legend_value(only, 'True').
get_gen_or_type_value(any, _).
get_gen_or_type_value(N,N):- N \= any.
