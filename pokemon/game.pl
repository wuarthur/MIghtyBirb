:- [utils].
:- [loader2].
:- [selectpokemon].
:- [battle].
:- [userio].


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


% active_pokemon(Id, 'pokemon_idx', Idx).
% active_pokemon(Id, 'affliction', 'rivial' | 'user').
% active_pokemon(Id, 'moves', [Move_Id]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  We use active_pokemon here because we need to handle possible duplicate pokemons
% ie 6 Pikachu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

activate(Pokemon_idx, Affliction, Move_indices):-
  pokemon(Pokemon_idx, 'hp', HP),
  find_idx(Id),
  Atoms = [
    active_pokemon(Id, 'pokemon_idx',Pokemon_idx),
    active_pokemon(Id, 'hp', HP),
    active_pokemon(Id, 'affliction', Affliction),
    active_pokemon(Id, 'moves', Move_indices)
  ],
  maplist(assertz, Atoms).


find_id(Id):-
  findall(I, active_pokemon(I, 'pokemon_idx', _), Lst),
  length(Lst, Id).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

status(Active_pokemon_idx, Status):-
  active_pokemon(Active_pokemon_idx, 'hp', HP),
  hp_status(HP, Status).

hp_status(Hp, 'OK'):-
  Hp > 0.
hp_status(Hp, 'Fainted'):-
  Hp =< 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%% Update KB to reflect change in active_pokemon stats.
%%%%%%% TODO -- Only HP is affected? There is no buff && de/buff to change attack / def stats?
%% NOTE: we are going to ignore status changes and buffs

update_stat(Idx, Stat, Diff):-
  active_pokemon(Idx, Stat, Old_value),
  New_value is Old_value + Diff,
  retract(active_pokemon(Idx, _, _)),
  assertz(active_pokemon(Idx, Stat, New_value)).
