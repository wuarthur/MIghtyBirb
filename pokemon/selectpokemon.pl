:- use_module(library(clpfd)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%  Presets, we gonna use Idx for everything.

preset('preset name 1', [11, 22, 33, 44, 55, 66]).
preset('preset name 2', [77, 88, 99, 2, 41, 23]).
preset('preset name 3', [98, 13, 254, 45, 78, 25]).
preset('preset name 4', [87, 76, 65, 54, 23, 12]).
preset('preset name 5', [45, 123, 66, 78, 12, 66]).


%% Picks moves set for pokemon.
default_moves(Idx, Move_idx_set):-
  Move_idx_set is 2.




%%%%%%%%%%%% Finds according to stats
%%%%%%%%%%%%%%%%% Try select_pokemons(1, 'False', ['hp', 'hp', 'tot'], Indices).
%%%%%%%%%%%% Returns unique results
%%%%%%%%%%% Not really an efficient impl :|, but it should suffice for now.
%%%%% Has a bug if say one pokemon is best at two areas, it will be duplicated. Low priority bug?


select_pokemons(Generation, Legendary, Stats, Indices):-
  setxcount(Stats, SetxCount),
  maplist(select_pokemons_helper(Generation, Legendary), SetxCount, Temp_indices),
  flatten(Temp_indices, Indices).


setxcount(List, Res):-
  list_to_set(List, Set),
  maplist(mapcount(List), Set, Res).

mapcount(List, E, Res):-
  count(E, List, Count),
  Res = [E, Count].


select_pokemons_helper(Generation, Legendary, [Stat,Count], Indices):-
  findall([Idx, S], (
    pokemon(Idx, Stat, S),
    pokemon(Idx, 'generation', Generation),
    pokemon(Idx, 'legendary', Legendary)
  ), Found),
  predsort(compare_stat, Found, Sorted),
  reverse(Sorted, Reversed),
  take(Count, Reversed, Values),
  maplist(mapvalues, Values, Indices).

mapvalues([Idx | _], Idx).


compare_stat(R, [_, S1], [_, S2]):-
  compare(R, S1, S2).
