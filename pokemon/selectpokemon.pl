:- use_module(library(clpfd)).


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
