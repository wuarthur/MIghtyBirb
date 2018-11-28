:- [utils].
:- [loader2].
:- [selectpokemon].
:- [battle].
:- [userio].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game states

game_state('Choosing teams'):- \+ you_still_have_pokemon, \+ npc_still_has_pokemon.
game_state('Battling'):- you_still_have_pokemon, npc_still_has_pokemon.
game_state('You win'):- you_still_have_pokemon, \+ npc_still_has_pokemon.
game_state('You lose'):- \+ you_still_have_pokemon, npc_still_has_pokemon.

you_still_have_pokemon:-
  active_pokemon(Id, 'NPC', false),
  active_pokemon(Id, 'hp', Hp),
  Hp > 0.

npc_still_has_pokemon:-
  active_pokemon(Id, 'NPC', true),
  active_pokemon(Id, 'hp', Hp),
  Hp > 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  We use active_pokemon here because we need to handle possible duplicate pokemons
% ie 6 Pikachu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic active_pokemon/3.

% Note NPC is true or false
activate(Pokemon_idx, NPC, Move_indices):-
  pokemon(Pokemon_idx, 'hp', HP),
  find_id(Id),
  Atoms = [
    active_pokemon(Id, 'pokemon_idx',Pokemon_idx),
    active_pokemon(Id, 'hp', HP),
    active_pokemon(Id, 'npc', NPC),
    active_pokemon(Id, 'moves', Move_indices),
    active_pokemon(Id, 'in battle', false)
  ],
  maplist(assertz, Atoms).


find_id(Id):-
  findall(I, active_pokemon(I, 'pokemon_idx', _), Lst),
  length(Lst, Id).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Select a pokemon of affliction : rival or user, deselect all other pokemons of same affliction
%% Battle between active pokemons

select(Id):-
  active_pokemon(1, 'npc', NPC),
  findall(I, active_pokemon(I, 'npc', NPC), Ids),
  maplist(deselect, Ids),
  assertz(active_pokemon(Id, 'in battle', true)).

deselect(Id):-
  retractall(active_pokemon(Id, 'in battle', _)),
  assertz(active_pokemon(Id, 'in battle', false)).

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
  retractall(active_pokemon(Idx, _, _)),
  assertz(active_pokemon(Idx, Stat, New_value)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Pick a random move for active pokemon

random_move(Id, Move):-
  active_pokemon(Id, 'moves', Move_indices),
  random_permutation(Move_indices, Moves),
  Moves = [Move|_ ].
