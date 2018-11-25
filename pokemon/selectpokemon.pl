:- use_module(library(clpfd)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%% Picks default moves set for pokemon, 4 is max I think.

default_moves(Idx, Move_indices):-
  findall(M, pokemon(Idx, 'move', M), Moves),
  random_permutation(Moves, M),
  take(4, M, Move_indices).



%%%%%%%%%%%%%%%%% Pick 6 random pokemons :D

random_pokemons(Generation, Legendary, Count, Indices):-
  findall(Idx, (
    pokemon(Idx, 'generation', Generation),
    pokemon(Idx, 'legendary', Legendary)
  ), Found),
  list_to_set(Found, Found_set),
  random_permutation(Found_set, I),
  take(Count, I, Indices).

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
  list_to_set(Found, Found_set),
  predsort(compare_stat, Found_set, Sorted),
  reverse(Sorted, Reversed),
  take(Count, Reversed, Values),
  maplist(mapvalues, Values, Indices).

mapvalues([Idx | _], Idx).


compare_stat(R, [_, S1], [_, S2]):-
  compare(R, S1, S2).
