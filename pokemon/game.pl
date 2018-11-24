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

active_pokemons('rival', []).
active_pokemons('mine', []).



%%%%%%%%% TODO next -- Pokemon team picking
