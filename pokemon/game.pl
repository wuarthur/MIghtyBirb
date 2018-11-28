:- [utils].
:- [kb].
:- [team].
:- [moves].
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
 NOTE: what is select and deselect for?
 also what is status for?
*/


%% Select a pokemon of affliction : rival or user, deselect all other pokemon of same affliction
%% Battle between active pokemon

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





%%%%%%%%%%%%%%%YI"S PERONAL SPACE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%YI"S PERONAL SPACE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%YI"S PERONAL SPACE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%











%%%%%%%%%%%%%%%YI"S PERONAL SPACE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%YI"S PERONAL SPACE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%YI"S PERONAL SPACE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%