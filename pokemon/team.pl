:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  We use active_pokemon here because we need to handle possible duplicate pokemon
% ie 6 Pikachu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic active_pokemon/3.

% Note NPC is true or false
activate(NPC, Pokemon_idx):-
  pokemon(Pokemon_idx, 'hp', HP),
  generate_id(Id),
  default_moves(Pokemon_idx, Move_indices),
  Atoms = [
    active_pokemon(Id, 'pokemon_idx',Pokemon_idx),
    active_pokemon(Id, 'hp', HP),
    active_pokemon(Id, 'npc', NPC),
    active_pokemon(Id, 'moves', Move_indices),
    active_pokemon(Id, 'in battle', false)
  ],
  maplist(assertz, Atoms).


generate_id(Id):-
  findall(I, active_pokemon(I, 'pokemon_idx', _), Lst),
  length(Lst, Id).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%% Pick 6 random pokemon :D

random_pokemon(Generation, Legendary, Count, Indices):-
  findall(Idx, (
    pokemon(Idx, 'generation', Generation),
    pokemon(Idx, 'legendary', Legendary)
  ), Found),
  list_to_set(Found, Found_set),
  random_permutation(Found_set, I),
  take(Count, I, Indices).

%%%%%%%%%%%% Finds according to stats
%%%%%%%%%%%%%%%%% Try select_pokemon(1, 'False', ['hp', 'hp', 'tot'], Indices).
%%%%%%%%%%%% Returns unique results
%%%%%%%%%%% Not really an efficient impl :|, but it should suffice for now.
%%%%% Has a bug if say one pokemon is best at two areas, it will be duplicated. Low priority bug?


select_pokemon(Generation, Legendary, Stats, Indices):-
  setxcount(Stats, SetxCount),
  maplist(select_pokemon_helper(Generation, Legendary), SetxCount, Temp_indices),
  flatten(Temp_indices, Indices).


setxcount(List, Res):-
  list_to_set(List, Set),
  maplist(mapcount(List), Set, Res).

mapcount(List, E, Res):-
  count(E, List, Count),
  Res = [E, Count].


select_pokemon_helper(Generation, Legendary, [Stat,Count], Indices):-
  findall([Idx, S], (
    pokemon(Idx, Stat, S),
    pokemon(Idx, 'generation', Generation),
    pokemon(Idx, 'legendary', Legendary)
  ), Found),
  list_to_set(Found, Found_set),
  predsort(compare_stat, Found_set, Sorted),
  reverse(Sorted, Reversed),
  take(Count, Reversed, Values),
  maplist(mapvalues, Values, Indices).

mapvalues([Idx | _], Idx).


compare_stat(R, [_, S1], [_, S2]):-
  compare(R, S1, S2).
