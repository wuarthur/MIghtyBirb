start:-
  ask_user(
    'Welcome to the pokemon battling lite.\n
    Would you like to build a team yourself or have assistance?\n',
    ['DIY\n','Help me\n'],
    ['DIY','help'],
    create_team
  ).

%%% DIY
create_team('DIY'):-
  can_still_add_pokemon_to_team,
  findall(Idx, pokemon(Idx, 'name', _), List),
  getPokemonNames(ListOfNames, List),
  append(ListOfNames, ['any\t\t','stop'],L1),
  append(List, ['any','stop'],L2),
  ask_user(
    'Choose a pokemon.\n',
    L1,
    L2,
    add_to_team
  ).
create_team(help):-
  ask_user(
    'Random team or specify strategy?\n',
    ['Random\n','Strategy\n'],
    ['random','strategy'],
    generate_full_team
  ).

%%% For DIY
%%% add one pokemon at a time
add_to_team(stop):-
  generate_rival_team.
add_to_team(any):-
  can_still_add_pokemon_to_team,
  random_pokemon(_,_,1,[H|_]),
  activate(false, H).
add_to_team(N):-
  can_still_add_pokemon_to_team,
  pokemon(N, _, _),
  activate(false, N).



%%% AI
%%% create a team of pokemon
generate_full_team(random):-
  random_pokemon(_, _, 6, ListOfPokemon),
  list_to_set(ListOfPokemon, Found_set),
  random_permutation(Found_set, I),
  take(6, I, Indices),
  maplist(activate_my_pokemon, Indices).

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
  strategy_parameters('legendary',Legend),
  get_legend_value(Legend, L),
  strategy_parameters('generation',Gen),
  get_gen_value(Gen, G),
  findall(Idx, (
    pokemon(Idx, 'type', Type),
    pokemon(Idx, 'legendary', L),
    pokemon(Idx, 'generation', G)
    ), List
  ),
  list_to_set(List, Found_set),
  random_permutation(Found_set, I),
  take(6, I, Indices),
  maplist(activate_my_pokemon, Indices),
  generate_rival_team.


generate_rival_team:-
  random_pokemon(_, _, 6, ListOfPokemon),
  list_to_set(ListOfPokemon, Found_set),
  random_permutation(Found_set, I),
  take(6, I, Indices),
  maplist(activate_npc_pokemon, Indices)
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
  pokemon(Index, 'name', N),
  Mod is Index mod 5,
  det_if_nl(Mod, Bool),
  display_name(N, Name, Bool).

det_if_nl(Mod, true):-
  0 == Mod.
det_if_nl(Mod, false):-
  0 < Mod.

display_name(Name, RetName, true):-
  add_nl(Name, RetName).

display_name(Name, RetName, false):-
  atom_concat(Name, '\t\t', RetName).

can_still_add_pokemon_to_team :-
  findall(I, active_pokemon(I, npc, false), Lst),
  length(Lst, Len),
  Len < 6.

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
  retract(strategy_parameters(ChangedName, _)),
  assertz(strategy_parameters(ChangedName, ChangedValue)),
  generate_full_team(strategy).

get_legend_value(allow, _).
get_legend_value(no, 'False').
get_legend_value(only, 'True').
get_gen_value(any, _).
get_gen_value(N,N):- N \= any.
