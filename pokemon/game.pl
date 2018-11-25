:- [utils].
:- [loader2].
:- [selectpokemon].
:- [battle].



%%%%%%%%% Easier to update KB if we use same 'active_pokemons' for both.
%%%%%%% We probably gonna use Idx for active pokemons, and update their health via DB as in battle.pl
%%%%%%% less "INNTER LEFT JOIN" that way.

/*
  each pokemon in the array will have a:
    id to allow the same pokemon to appear multiple times
    pokedex number
    current hp
    moveSet (maximum 4 moves, minimum 1 move. i dont know why anyone would have less than 4 but you can)
*/


% active_pokemon(Id, 'idx', Idx).
% active_pokemon(Id, 'affliction', 'rivial' | 'user').
% active_pokemon(Id, 'move', Move_Id).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%% Update KB to reflect change in active_pokemon stats.
%%%%%%% TODO -- Only HP is affected? There is no buff && de/buff to change attack / def stats?

update_hp(Idx, Stat, Diff):-
  active_pokemon(Idx, Stat, Old_value),
  New_value is Old_value + Diff,
  retract(active_pokemon(Idx, _, _)),
  assertz(active_pokemon(Idx, Stat, New_value)).